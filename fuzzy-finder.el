;;; fuzzy-finder.el --- A Front-End for Fuzzy Finder Applications   -*- lexical-binding: t; -*-

;; Copyright (C) 2020 10sr

;; Some Portions Copyright (C) 2015 Bailey Ling

;; Author: 10sr <8.slashes@gmail.com>
;; Author: Bailey Ling
;; Maintainer: 10sr <8.slashes@gmail.com>
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

;; A front-end for fuzzy finder applications.

;; `fuzzy-finder' command will open a new window and start a fuzzy finder
;; process inside of it, and call an action function with selected items.
;; By default the action function is set to the function that visits given
;; files. so you can use `fuzzy-finder' command to `find-file' existing files.

;; You can customize default values used for `fuzzy-finder' execution including
;; fuzzy finder command, input command, action function and so on.
;; These values can also be given when calling `fuzzy-finder' as a function,
;; which is useful when you want to define new interactive commands that using
;; `fuzzy-finder'.

;;; Code:

(require 'cl-lib)
(require 'nadvice)
(require 'term)

(defgroup fuzzy-finder nil
  "A Front-End for Fuzzy Finder Applications."
  :group 'convenience)

(defcustom fuzzy-finder-default-command "fzf --reverse"
  "Default value for `fuzzy-finder' COMMAND argument."
  :type 'string
  :group 'fuzzy-finder)

(defcustom fuzzy-finder-default-input-command nil
  "Default value for `fuzzy-finder' INPUT-COMMAND argument."
  :type 'string
  :group 'fuzzy-finder)

(defcustom fuzzy-finder-default-action 'fuzzy-finder-action-find-files
  "Default value for `fuzzy-finder' ACTION argument."
  :type 'function
  :group 'fuzzy-finder)

(defcustom fuzzy-finder-default-output-delimiter "\n"
  "Default value for `fuzzy-finder' OUTPUT-DELIMITER argument."
  :type 'string
  :group 'fuzzy-finder)

(defcustom fuzzy-finder-default-window-height 12
  "Default value for `fuzzy-finder' WINDOW-HEIGHT argument."
  :type 'integer
  :group 'fuzzy-finder)

(defcustom fuzzy-finder-init-hook nil
  "Hook run just after initialize fuzzy finder buffer."
  :type 'hook
  :group 'fuzzy-finder)

(defcustom fuzzy-finder-exit-hook nil
  "Hook run just before starting exit process of fuzzy finder."
  :type 'hook
  :group 'fuzzy-finder)

(defvar fuzzy-finder--window-configuration nil
  "Window configuration before showing fuzzy finder buffer.")

(defconst fuzzy-finder--process-name "fuzzy-finder"
  "Process name for fuzzy finder.")

(defvar-local fuzzy-finder--output-file nil
  "File name for output of fuzzy-finder result.")

(defvar-local fuzzy-finder--output-delimiter nil
  "Delimiter for output of fuzzy-finder result.")

(defvar-local fuzzy-finder--action nil
  "Action function given to this fuzzy-finder session.")



(defsubst fuzzy-finder--get-buffer-create (&optional force-recreate)
  "Get or create buffer for fuzzy finder process.

If optional FORCE-RECREATE is set to non-nil and buffer already exists,
destroy it and create new buffer with same name."
  (let* ((name (concat "*" fuzzy-finder--process-name "*"))
         (buf (get-buffer name)))
    (when (and buf
               force-recreate)
      (kill-buffer buf))
    (get-buffer-create name)))

(defun fuzzy-finder--display-buffer (buf height)
  "Display fuzzy finder BUF and set window height to HEIGHT.

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

(cl-defun fuzzy-finder--after-term-handle-exit (_ msg)
  "Call the action function when fuzzy-finder program terminated normally.

Should be hooked to `term-handle-exit'.
Use MSG to check if fuzzy-finder process exited with code 0."
  (unless fuzzy-finder--output-file
    (cl-return-from fuzzy-finder--after-term-handle-exit))

  (run-hooks 'fuzzy-finder-exit-hook)
  (let* ((buf (current-buffer))
         (output-file fuzzy-finder--output-file)
         (output-delimiter fuzzy-finder--output-delimiter)
         (action fuzzy-finder--action)
         (text (with-temp-buffer
                 (insert-file-contents output-file)
                 (buffer-substring-no-properties (point-min) (point-max))))
         (lines (split-string text output-delimiter t)))
    (delete-file output-file)
    (set-window-configuration fuzzy-finder--window-configuration)
    (when (string= "finished\n" msg)
      (with-current-buffer buf
        (funcall action lines)))))
(advice-add 'term-handle-exit :after
            'fuzzy-finder--after-term-handle-exit)

;;;###autoload
(cl-defun fuzzy-finder (&key directory command input-command action output-delimiter window-height)
  "Execute fuzzy-finder application.

This function will open a term buffer and start fuzzy-finder process using
COMMAND argument.  After the process exits successfully call ACTION function
with the result.

All arguments are optional keyword arguments.
There is a variable that defines default value of each argument except for
DIRECTORY: for example `fuzzy-finder-default-command' is for COMMAND argument.

`:directory DIRECTORY'
    Set the directory to start fuzzy-finder application from.
    If not given current `default-directory' will be used.

`:command COMMAND'
    Command-line string of fuzzy-finder application to execute.

`:input-command INPUT-COMMAND'
    When non-empty string is given for INPUT-COMMAND, the stdout of this
    command result will be piped (\"|\") into COMMAND.
    Otherwise, COMMAND will be invoked without any input, thus the application
    default might be used (\"FZF_DEFAULT_COMMAND\" for example).

`:action ACTION'
    Callback function that results of fuzzy-finder selection will be passed to.

    This function shall accept one argument RESULTS.
    RESULTS is a list of string of fuzzy-finder selection: it will be made by
    splitting stdout of command by OUTPUT-DELIMITER.

`:output-delimiter OUTPUT-DELIMITER'
    Regular expression to split result of COMMAND.
    When the stdout is delimited by ASCII NUL characters, this value should be
    \"\\0\".

`:window-height WINDOW-HEIGHT'
    Interger of height of window that displays fuzzy-finder buffer."
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

  ;; Modified from fzf.el: https://github.com/bling/fzf.el
  (setq fuzzy-finder--window-configuration
        (current-window-configuration))

  (let* ((buf (fuzzy-finder--get-buffer-create t))
         (sh-cmd (if (and input-command
                          (not (string= "" input-command)))
                     (concat input-command "|" command)
                   command))
         (output-file (make-temp-file "fzf-el-result"))
         (sh-cmd-with-redirect (concat sh-cmd
                                       ">"
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
    (when (fboundp 'company-mode)
      (company-mode 0))
    (setq-local mode-line-format nil)
    (setq-local scroll-margin 0)
    (setq-local scroll-conservatively 0)
    (setq-local term-suppress-hard-newline t)  ; for paths wider than the window
    (setq-local show-trailing-whitespace nil)
    (setq-local display-line-numbers nil)
    (face-remap-add-relative 'mode-line '(:box nil))

    (run-hooks 'fuzzy-finder-init-hook)
    ))

(defun fuzzy-finder-action-find-files (files)
  "Visit FILES."
  (dolist (file (mapcar 'expand-file-name  files))
    (find-file file)))

(declare-function projectile-project-root "projectile")

;;;###autoload
(defun fuzzy-finder-find-files-projectile ()
  "Execute fuzzy finder and visit resulting files.

If path of root directory is available from projectile, start from that directory."
  (interactive)
  (let ((dir (or (ignore-errors
                   (require 'projectile)
                   (projectile-project-root))
                 default-directory)))
    (fuzzy-finder :directory dir)))

(provide 'fuzzy-finder)

;;; fuzzy-finder.el ends here
