;;; init-thing.el --- Initialize thing configuration.  -*- lexical-binding: t; -*-

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

;;

;;; Code:

;;; Require
(eval-when-compile (require 'thingatpt))

;;; Code:

(defgroup roife/things nil
  "Thing edit."
  :group 'faces)

(defface roife/things-font-lock-action
  '((t (:foreground "Gold" :bold t)))
  "Face for action"
  :group 'things)

(defface roife/things-font-lock-flash
  '((t (:inherit highlight)))
  "Face to flash the current line."
  :group 'things)

(defun things-internal (obj-beg obj-end &optional kill-conditional)
  "A fast edit complexes object.
Argument OBJ-BEG the begin position that object.
Argument OBJ-END the end position of object.
Optional argument KILL-CONDITIONAL default is do copy handle, if KILL-CONDITIONAL is non-nil do cut handle."
  (message "[%s] \n %s"
           (propertize
            (if kill-conditional "Kill" "Copy")
            'face 'roife/things-font-lock-action)
           (buffer-substring obj-beg obj-end))
  (if kill-conditional
      (kill-region obj-beg obj-end)
    (kill-ring-save obj-beg obj-end)
    (pulse-momentary-highlight-region obj-beg obj-end 'roife/things-font-lock-flash)))

(defun things (thing &optional kill-conditional)
  "This function is a simple interface for `things-internal'.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (save-excursion
    (things-internal (beginning-of-thing thing)
                     (end-of-thing thing)
                     kill-conditional)))

(defun thing-replace-internal (obj-beg obj-end)
  "A fast replace complexes object.
Argument OBJ-BEG the begin position that object.
Argument OBJ-END the end position of object."
  (interactive)
  (goto-char obj-beg)
  (delete-char (- obj-end obj-beg))
  (yank))

(defun thing-replace (thing)
  "This function is a simple interface for `thing-replace-internal'"
  (save-excursion
    (thing-replace-internal (beginning-of-thing thing)
                            (end-of-thing thing))))

;;; macro
(defmacro things-defcopy (thing)
  "Install funstions of copying for THING."
  `(defun ,(intern (format "things-copy-%s" thing)) (kill-condition)
     ,(format "Copy %s at current point.
With the universal argument, the text will also be killed" thing)
     (interactive "P")
     (things ',thing kill-condition)
     ))

(defmacro things-defcut (thing)
  "Install funstions of cutting for THING"
  `(defun ,(intern (format "things-cut-%s" thing)) ()
     ,(format "Cut %s at current point." thing)
     (interactive)
     (things ',thing t)
     ))

(defmacro things-defreplace (thing)
  "Install funstions of replacing for THING"
  `(defun ,(intern (format "things-replace-%s" thing)) ()
     ,(format "Cut %s at current point." thing)
     (interactive)
     (thing-replace ',thing)
     ))

(defmacro things-defthing (thing)
  "Install THING."
  `(progn (things-defcopy ,thing)
          (things-defcut ,thing)
          (things-defreplace ,thing)))


;; add-on:date-at-point
(defvar date-at-point-regexp
  (let ((separator  (rx (any "-./")))
        (2-digits   (rx (repeat 2 digit)))
        (2-4-digits (rx (repeat 2 4 digit))))
    (concat 2-4-digits separator 2-digits separator 2-4-digits))
  "Regular expression matching a date.")

(defun date-at-point-bounds ()
  "Return the bounds of the date at point."
  (save-excursion
    (when (thing-at-point-looking-at date-at-point-regexp)
      (cons (match-beginning 0) (match-end 0)))))
(put 'date 'bounds-of-thing-at-point 'date-at-point-bounds)

(defun paren-at-point-bounds ()
  (save-excursion
    (let ((pos (thing-at-point-bounds-of-list-at-point)))
      (cons (1+ (car pos))
            (1- (cdr pos))))))
(put 'paren 'bounds-of-thing-at-point 'paren-at-point-bounds)


;;; defthing
(things-defthing symbol)
(things-defthing list)
(things-defthing sexp)
(things-defthing defun)
(things-defthing filename)
(things-defthing url)
(things-defthing email)
(things-defthing uuid)
(things-defthing word)
(things-defthing sentence)
(things-defthing whitespace)
(things-defthing line)
(things-defthing number)
(things-defthing page)
(things-defthing paragraph)
(things-defthing date)
(things-defthing paren)

(provide 'init-thing)
;;; init-thing.el ends here
