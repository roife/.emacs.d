;;; early-init.el --- Early initialization.          -*- lexical-binding: t; -*-

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

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold (* 512 1024 1024)
      gc-cons-percentage 0.7)

;; Faster to disable these here (before they've been initialized)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (setq menu-bar-mode nil))
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)
(modify-all-frames-parameters '((vertical-scroll-bars)))

(provide 'early-init)
;;; early-init.el ends here
