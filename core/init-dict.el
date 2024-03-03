;;; -*- lexical-binding: t -*-

;; [google this] Google word at point
(use-package google-this
  :straight t
  :defines (google-this)
  :bind (("C-, w" . google-this)))


;; [google-translate]
(use-package google-translate
  :straight t
  :defines (google-translate-translation-directions-alist)
  :bind (("C-c d g" . google-translate-smooth-translate))
  :config
  (setq google-translate-translation-directions-alist '(("en" . "zh-CN")))
  )
