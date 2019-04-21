;;; init.el --- Emacs Configures Init File           -*- lexical-binding: t; -*-

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

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
;;;; speed up initialization
(eval-and-compile
  (defvar roife/default-file-name-handler-alist file-name-handler-alist)

  (setq file-name-handler-alist nil)

  (add-hook 'emacs-startup-hook
            (lambda ()
              "Restore defalut values after init"
              (setq file-name-handler-alist roife/default-file-name-handler-alist
                    gc-cons-threshold 8000000
                    gc-cons-percentage 0.7)
              (add-function :after after-focus-change-function
                            (lambda () (unless (frame-focus-state)
                                    (garbage-collect))))
              ))
  )

;;;; Load path
;; Optimize: Force "core"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "core" user-emacs-directory) load-path)
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;;;; Load configuration
;; Initialize elpa
(require 'init-elpa)
;; basic
(require 'init-define)
(require 'init-basic)

;; Load UI first to speed up
(require 'init-ui)
(if roife/gui-p
    (progn
      (require 'init-gui)
      (require (cond (roife/sys-macos-p 'init-macos)
                     ;; ((roife/sys-linux-p) 'init-linux)
                     ;; ((roife/sys-windows-p) 'init-windows)
                     ))
      (require 'init-modeline)
      )
  )


(require 'init-edit)
(require 'init-window)
(require 'init-kill-ring)

(require 'init-ivy)
(require 'init-company)
;; (require 'init-yasnippet)
(require 'init-calendar)
(require 'init-dired)
(require 'init-shell)
(require 'init-eshell)
(require 'init-treemacs)
(require 'init-git)

(require 'init-org)
(require 'init-markdown)

(require 'init-file)

(require 'init-highlight)

(require 'init-rifkey)

(require 'init-utils)

(require 'init-prog)

(require 'init-test)

;; custom.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :no-error :no-message)

(provide 'init)
;;; init.el ends here
