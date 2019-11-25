;;; init-win32.el --- Initialize win32 configurations.  -*- lexical-binding: t; -*-

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

;; Initialize win32 configurations.

;;; Code:
(eval-when-compile (require 'init-define))

;; Font
(add-to-list 'default-frame-alist '(height . 35))
(add-to-list 'default-frame-alist '(width . 100))

(defun my--set-font (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (if (string-equal system-type "windows-nt")
        ;; 下面是用于Windows的配置。
        (progn
          ;; 设置英文字体为Inconsolata，并指定字号18。
          ;; 因为不同操作系统下字体显示的大小不一样(DPI的问题)，所以分开设置。
          (set-face-attribute 'default nil :font "Consolas 11")

          ;; (add-to-list 'default-frame-alist
          ;;              '(font . "Inconsolata 18"))
          ;; (set-fontset-font "fontset-startup"
          ;;            'ascii (font-spec :family "Inconsolata" :size 24) nil 'prepend)

          ;; 给相应的字符集设置中文字体，这里的字体是冬青黑体简体中文 W3。
          (dolist (charset '(han cjk-misc chinese-gbk))
            (set-fontset-font "fontset-default"
                              charset (font-spec :family "微软雅黑")))

          ;; Emacs 25.2之后的版本无法单独设置符号的字体，这句禁用这个特性恢复原来的行为。
          (setq use-default-font-for-symbols nil)


          ))))

;; 运行一下立即设置字体。
;; (add-hook 'after-init-hook 'my--set-font)
(my--set-font)

;; 对于新建的frame应用设置。
(add-hook 'after-make-frame-functions #'my--set-font)

(defface strike-through
  '((t :strike-through t))
  "Basic strike through face."
  :group 'basic-faces)

(setq menu-bar-mode 0)
(setq tool-bar-mode 0)
(setq scroll-bar-mode 0)
(modify-all-frames-parameters '((vertical-scroll-bars)))



(provide 'init-win32)
;;; init-macos.el ends here
