;;; init-basic.el --- Initialize basic configurations.  -*- lexical-binding: t; -*-

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

;; Initialize basic configurations.

;;; Code:
(eval-when-compile (require 'init-define))

;;;; Personal information
(setq user-full-name "roife"
      user-mail-address "roife@outlook.com")

;;;; Initial configurations
;; (fset 'display-startup-echo-area-message 'ignore)
(setq inhibit-startup-screen t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;;;; Backup & Autosave
(setq make-backup-files nil
      delete-old-versions t
      auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))

;;;; Auto Revert
;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

;;;; Misc
(setq indicate-empty-lines t
      ring-bell-function 'ignore);; use C-y M-y to paste from clipbaord instead of kill-ring
;; Use y-or-n to replace yes-or-no
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'init-basic)
;;; init-basic.el ends here
