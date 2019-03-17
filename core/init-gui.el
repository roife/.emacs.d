;;; init-gui.el --- Initialize graphic user interface configurations.    -*- lexical-binding: t; -*-

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

;; Initialize graphic user interface configurations.

;;; Code:
(eval-when-compile (require 'init-define))

;;;; Theme: [solaire-mode]
;; Make certain buffers grossly incandescent
;; (use-package solaire-mode
;;   :functions persp-load-state-from-file
;;   :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
;;          (minibuffer-setup . solaire-mode-in-minibuffer)
;;          (after-load-theme . solaire-mode-swap-bg))
;;   :config
;;   (solaire-mode-swap-bg)
;;   (advice-add #'persp-load-state-from-file
;;               :after #'solaire-mode-restore-persp-mode-buffers))

;;;; Mouse
;; 支持鼠标中键粘贴
(setq mouse-yank-at-point t
      mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;;;; Font
;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;;;; Dialog
;; don't use dialog box
(setq use-file-dialog nil
      use-dialog-box nil)

;;;; Server
;; (use-package server
;;   :ensure nil
;;   :hook (after-init . server-mode))

(provide 'init-gui)
;;; init-gui.el ends here
