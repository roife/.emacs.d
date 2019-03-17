;;; init-define.el --- - Initialize definitions. -*- lexical-binding: t; -*-

;; Copyright (C) 2019  roife

;; Author: roife <roife@outlook.com>
;; Keywords: lisp

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

;; Constants, variables and functions.

;;; Code:

(require 'cl)

;;; const
(defconst roife/sys-macos-p (eq system-type 'darwin))
(defconst roife/sys-linux-p (eq system-type 'gnu/linux))
(defconst roife/sys-win32-p (eq system-type 'windows-nt))
(defconst roife/gui-p (display-graphic-p))

;;; path
(defun roife/shorten-path (path &optional max-len)
  "Shorten PATH to MAX-LEN."
  (unless max-len (setq max-len 0))
  (if (and path (not (eq path "")))
      (let* ((components (split-string (abbreviate-file-name path) "/"))
             (len (+ (1- (length components))
                     (reduce '+ components :key 'length)))
             (str ""))
        (while (and (> len max-len)
                    (cdr components))
          (setq str (concat str (if (= 0 (length (car components)))
                                    "/"
                                  (string (elt (car components) 0) ?/)))
                len (- len (1- (length (car components))))
                components (cdr components)))
        (concat str (reduce (lambda (a b) (concat a "/" b)) components)))
    ""
    )
  )

;;; current-window
(defvar roife/current-window nil)
(add-hook 'post-command-hook '(lambda () (setq roife/current-window (selected-window))))

;;; onekey
(defvar roife/onekey-mode 'insert)

(provide 'init-define)
;;; init-define.el ends here
