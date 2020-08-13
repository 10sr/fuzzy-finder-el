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

(defvar fuzzy-finder--window-configuration nil
  "Window configuration before showing fuzzy-finder buffer.")

(defconst fuzzy-finder--process-name "fuzzy-finder"
  "Process name for fuzzy-finder.")

(defvar-local fuzzy-finder--tmp-output-file nil
  "File name for output of fuzzy-finder result.")

(defvar-local fuzzy-finder--action nil
  "Action function given to this fuzzy-finder session.")



(defsubst fuzzy-finder--get-buffer-create ()
  "Get or create buffer for fuzzy-finder process"
  (get-buffer-create (concat "*" fuzzy-finder--process-name "*")))

(defun fuzzy-finder--display-buffer (buf height)
  "Display fuzzy-finder BUF and set window height to  HEIGHT.

This function sets current buffer to BUF, and returns created window."
  (let* ((new-window nil))
    (setq new-window (split-window (frame-root-window)
                                   (- height)
                                   'below))
    (select-window new-window)
    (switch-to-buffer buf)
    new-window))

(cl-defun fuzzy-finder (&key directory command input-command action output-delimiter)
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

  (setq fuzzy-finder--window-configuration
        (current-window-configuration))

  (let* ((buf (fuzzy-finder--get-buffer-create))
         (window-height 12)
         (sh-cmd (if input-command
                  (concat input-command " | " command)
                command))
         (output-file (make-temp-file "fzf-el-result"))
         (sh-cmd-with-redirect (concat sh-cmd
                                       " > "
                                       (shell-quote-argument output-file))))

    (fuzzy-finder--display-buffer buf window-height)

    (cd directory)
    (setq-local fuzzy-finder--tmp-output-file
                output-file)
    (setq-local fuzzy-finder--action
                action)
    (setq-local mode-line-format nil)

    (make-term fuzzy-finder--process-name
               "sh"
               nil
               "-c"
               sh-cmd-with-redirect)

    (linum-mode 0)
    (visual-line-mode 0)
    (setq-local scroll-margin 0)
    (setq-local scroll-conservatively 0)
    (setq-local term-suppress-hard-newline t)  ; for paths wider than the window
    (setq-local show-trailing-whitespace nil)
    (setq-local display-line-numbers nil)
    (face-remap-add-relative 'mode-line '(:box nil))
    (term-char-mode)
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
