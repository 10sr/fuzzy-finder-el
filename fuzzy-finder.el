;;; fuzzy-finder.el --- A Front-End for Fuzzy Finder Applications   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  10sr

;; Author: 10sr <8.slashes@gmail.com>
;; Keywords: matching

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A Front-End for Fuzzy Finder Applications.

;;; Code:

(require 'cl-lib)
(require 'term)

(defgroup fuzzy-finder nil
  "A Front-End for Fuzzy Finder Applications."
  :group 'convenience)

(defcustom fuzzy-finder-default-command "fzf --reverse"
  "Default value for `fuzzy-finder' COMMAND."
  :type 'string
  :group 'fuzzy-finder)

(defcustom fuzzy-finder-default-input-command nil
  "Default value for `fuzzy-finder' INPUT-COMMAND."
  :type 'string
  :group 'fuzzy-finder)

(defcustom fuzzy-finder-default-action 'fuzzy-finder-action-find-files
  "Default value for `fuzzy-finder' ACTION."
  :type 'function
  :group 'fuzzy-finder)

(defcustom fuzzy-finder-default-output-delimiter "\n"
  "Default value for `fuzzy-finder' OUTPUT-DELIMITER."
  :type 'string
  :group 'fuzzy-finder)

(defcustom fuzzy-finder-default-window-height 12
  "Default value for `fuzzy-finder' WINDOW-HEIGHT."
  :type 'integer
  :group 'fuzzy-finder)

(defvar fuzzy-finder--window-configuration nil
  "Window configuration before showing fuzzy-finder buffer.")

(defconst fuzzy-finder--process-name "fuzzy-finder"
  "Process name for fuzzy-finder.")

(defvar-local fuzzy-finder--output-file nil
  "File name for output of fuzzy-finder result.")

(defvar-local fuzzy-finder--output-delimiter nil
  "Delimiter for output of fuzzy-finder result.")

(defvar-local fuzzy-finder--action nil
  "Action function given to this fuzzy-finder session.")



(defsubst fuzzy-finder--get-buffer-create (&optional force-recreate)
  "Get or create buffer for fuzzy-finder process

If Optional FORCE-RECREATE is set to non-nil and buffer already exists,
destroy it and create new buffer with same name.
existing buffer and create "
  (let* ((name (concat "*" fuzzy-finder--process-name "*"))
         (buf (get-buffer name)))
    (when (and buf
               force-recreate)
      (kill-buffer buf))
    (get-buffer-create name)))

(defun fuzzy-finder--display-buffer (buf height)
  "Display fuzzy-finder BUF and set window height to HEIGHT.

This function sets current buffer to BUF, and returns created window."
  (let ((new-window nil)
        (height (min height
                     (/ (frame-height) 2))))
    (setq new-window (split-window (frame-root-window)
                                   (- height)
                                   'below))
    (select-window new-window)
    (switch-to-buffer buf)
    new-window))

(cl-defun fuzzy-finder--after-term-handle-exit (process-name msg)
  "Call the action function when fuzzy-finder program terminated normally.

Should be hooked to `term-handle-exit'.
PROCESS-NAME and MSG are ignored."
  (unless fuzzy-finder--output-file
    (cl-return-from fuzzy-finder--after-term-handle-exit))

  (let* ((output-file fuzzy-finder--output-file)
         (output-delimiter fuzzy-finder--output-delimiter)
         (action fuzzy-finder--action)
         (text (with-temp-buffer
                 (insert-file-contents output-file)
                 (buffer-substring-no-properties (point-min) (point-max))))
         (lines (split-string text output-delimiter t)))
    (delete-file output-file)
    (set-window-configuration fuzzy-finder--window-configuration)
    (funcall action lines)))
(advice-add 'term-handle-exit :after
            'fuzzy-finder--after-term-handle-exit)

(cl-defun fuzzy-finder (&key directory command input-command action output-delimiter window-height)
  "Invoke fzf executable and return resulting list."
  (interactive)
  (setq directory (or directory
                      default-directory))
  (setq command (or command
                    fuzzy-finder-default-command))
  (setq input-command (or input-command
                          fuzzy-finder-default-input-command))
  (setq action (or action
                   fuzzy-finder-default-action))
  (setq output-delimiter (or output-delimiter
                             fuzzy-finder-default-output-delimiter))
  (setq window-height (or window-height
                          fuzzy-finder-default-window-height))

  (setq fuzzy-finder--window-configuration
        (current-window-configuration))

  (let* ((buf (fuzzy-finder--get-buffer-create t))
         (sh-cmd (if input-command
                  (concat input-command " | " command)
                command))
         (output-file (make-temp-file "fzf-el-result"))
         (sh-cmd-with-redirect (concat sh-cmd
                                       " > "
                                       (shell-quote-argument output-file))))

    (fuzzy-finder--display-buffer buf window-height)

    (cd directory)
    (make-term fuzzy-finder--process-name
               "sh"
               nil
               "-c"
               sh-cmd-with-redirect)
    (term-char-mode)

    (setq-local fuzzy-finder--output-file output-file)
    (setq-local fuzzy-finder--output-delimiter output-delimiter)
    (setq-local fuzzy-finder--action action)

    (linum-mode 0)
    (visual-line-mode 0)
    (setq-local mode-line-format nil)
    (setq-local scroll-margin 0)
    (setq-local scroll-conservatively 0)
    (setq-local term-suppress-hard-newline t)  ; for paths wider than the window
    (setq-local show-trailing-whitespace nil)
    (setq-local display-line-numbers nil)
    (face-remap-add-relative 'mode-line '(:box nil))
    ))

(defun fuzzy-finder-action-find-files (files)
  "Visit FILES."
  (dolist (file files)
    (find-file file)))

;; (format-spec "%aaa" `((?a . ,default-directory) (?b . "aaa")))
;; (format "%2$s %3$s" "e" "f" "g")
;; (format "hoehoe" "eee")
(provide 'fuzzy-finder)

;;; fuzzy-finder.el ends here
