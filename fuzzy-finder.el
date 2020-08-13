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

(require cl-lib)

(cl-defun fuzzy-finder (&key directory command input-command action output-delimiter)
  "Invoke fzf executable and return resulting list."
  (message "%S" directory)
  (message "%S" action)
  )


(defun fuzzy-finder-find-files ()
  "Find file using `fzf-exec'."
  (interactive)
  (fuzzy-finder :action (lambda (files)
                          (dolist (file files)
                            (find-file file)))))


;; (format-spec "%aaa" `((?a . ,default-directory) (?b . "aaa")))
;; (format "%2$s %3$s" "e" "f" "g")
;; (format "hoehoe" "eee")
(provide 'fuzzy-finder)

;;; fuzzy-finder.el ends here
