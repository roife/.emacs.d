;;; -*- lexical-binding: t -*-

;; [google this] Google word at point
(use-package google-this
  :straight t
  :bind (("C-, g" . google-this)))


;; [google-translate]
(use-package google-translate
  :straight t
  :bind (("C-c d g" . google-translate-smooth-translate))
  :config
  (setq google-translate-translation-directions-alist '(("en" . "zh-CN")))
  )
