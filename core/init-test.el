;;; init-test.el --- Initialize test code.           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  roife

;; Author: roife <roife@outlook.com>
;; Keywords:

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

;; Initialize test code.

;;; Code:
(use-package exec-path-from-shell
  :if (eq window-system 'ns)       ; Only for the official Cocoa Emacs
  :init
  (let ((path (eval-when-compile
                (require 'exec-path-from-shell)
                (exec-path-from-shell-getenv "PATH"))))
    (setenv "PATH" path)
    (setq exec-path (append (parse-colon-path path) (list exec-directory)))))

(use-package pyim
  :after ivy
  :config

  (defun eh-ivy-cregexp (str)
    (let ((a (ivy--regex-plus str))
          (b (let ((case-fold-search nil))
               (pyim-cregexp-build str))))
      (if (and a (stringp a))
          (concat a "\\|" b)
        a)))

  (setq ivy-re-builders-alist
        '((t . eh-ivy-cregexp))))

(provide 'init-test)
;;; init-test.el ends here
