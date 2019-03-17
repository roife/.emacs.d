;;; init-macos.el --- Initialize macOS configurations.  -*- lexical-binding: t; -*-

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

;; Initialize macOS configurations.

;;; Code:
(eval-when-compile (require 'init-define))

;; Appearance
(defun roife/sys-macos/hide-titlebar ()
  "Hide titlebar when using macOS."
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg))))
  )

(defun roife/sys-macos/set-font ()
  "Set font for macOS."
  ;; English
  (set-face-attribute
   'default nil :font "Menlo 14")
  (dolist (charset '( han symbol cjk-misc bopomofo))
                                        ; Chinese
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "STHeiti" :size 16)))
  ;; Japanese
  ;; (set-fontset-font (frame-parameter nil 'font)
  ;;            ;; 'kana
  ;;            (font-spec :family "Hiragino Sans" :size 16))
  ;; ;; Korean
  ;; (set-fontset-font (frame-parameter nil 'font)
  ;;            ;; 'hangul
  ;;            (font-spec :family "Apple SD Gothic Neo" :size 18))
  )
(roife/sys-macos/hide-titlebar)
(roife/sys-macos/set-font)
(setq ns-pop-up-frames nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(global-set-key (kbd "s-r") 'roife/compile)
(add-hook 'compilation-shell-minor-mode-hook (lambda () (define-key compilation-shell-minor-mode-map (kbd "C-g") #'kill-buffer-and-window)))

;;; switch errors
(global-set-key (kbd "M-[") 'next-error)
(global-set-key (kbd "M-]") 'previous-error)

;;; redo
(global-set-key (kbd "s-Z") 'undo-tree-redo)

;;; buffers
(global-set-key (kbd "s-w") 'kill-buffer-and-window)
(global-set-key (kbd "s-b") 'recentf-open-files)

;;; search
(global-set-key (kbd "s-f") 'swiper)
(global-set-key (kbd "s-F") 'query-replace)
(global-set-key (kbd "s-p") 'counsel-rg)
;; org
(global-set-key (kbd "s-P") '(lambda () (interactive) (counsel-rg nil org-agenda-dir nil nil)))

(provide 'init-macos)
;;; init-macos.el ends here
