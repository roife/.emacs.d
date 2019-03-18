;;; init-ui.el --- Initialize user interface configurations.   -*- lexical-binding: t; -*-

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

;; Initialize user interface configurations.

;;; Code:
(eval-when-compile (require 'init-define))

;; (use-package atom-one-dark-theme
;;   :init (load-theme 'atom-one-dark t))

(use-package one-themes
  :init (load-theme 'one-dark t))

;;;; line & Column
(setq-default fill-column 80)

;;;;; Line numbers: display-line-numbers
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode) . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 4))

;;;; Mouse & Scroll & Move
;; 滚动时保留10行，防止页面跳动
(setq scroll-preserve-screen-position t
      scroll-margin 10
      scroll-conservatively 101)
;; 移动时按照屏幕显示而非真实的一行
(setq line-move-visual nil)
;; 光标在行尾移动时始终保持在行尾
(setq track-eol t)

(provide 'init-ui)
;;; init-ui.el ends here
