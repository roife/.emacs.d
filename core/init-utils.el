;;; init-utils.el --- Initialize ultilities.         -*- lexical-binding: t; -*-

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

;; Initialize ultilities.

;;; Code:
(eval-when-compile (require 'init-define))

;;;; Dictionary: [Youdao Dictionay]
(use-package youdao-dictionary
  :bind (("C-c y" . youdao-dictionary-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point-tooltip))
  :config
  ;; Cache documents
  (setq url-automatic-caching t)

  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t))

;;;; Search with google: [google-translate], [google-this]
(use-package google-translate
  :bind (("C-c t" . google-translate-at-point))
  :config
  ;; use translate.google.cn
  (eval-after-load 'google-translate-core
    '(setq google-translate-base-url "http://translate.google.cn/translate_a/single"
           google-translate-listen-url "http://translate.google.cn/translate_tts"
           google-translate-default-target-language "zh-CN"))
  (eval-after-load 'google-translate-tk
    '(setq google-translate--tkk-url "http://translate.google.cn/")))

(use-package google-this
  :bind (("C-c g" . google-this)))

;;;; Export html: [htmlize]
(use-package htmlize)

;;;; paste code to pastebin-like services: [webpaste]
(use-package webpaste)

;;;; post image to imgbb: [imgbb]
(use-package imgbb)

(use-package keyfreq
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(provide 'init-utils)
;;; init-utils.el ends here
