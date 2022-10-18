;;; -*- lexical-binding: t -*-

;; [google this] Google word at point
(use-package google-this
  :straight t
  :bind (("C-, g" . google-this)))


;; [go-translate] Online translation service
(use-package go-translate
  :straight t
  :bind (("C-c d g" . gts-do-translate))
  :config
  (setq gts-translate-list '(("en" "zh") ("zh" "en")))
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines
         (list
          (gts-google-engine :parser (gts-google-parser)))
         :render
         ;;(gts-posframe-pop-render :backcolor "#333333" :forecolor "#ffffff")
         (gts-posframe-pop-render)))
  )


;; [osx-dictionary] macOS native dictionary app
(use-package osx-dictionary
  :straight t
  :when (eq system-type 'darwin)
  :bind (("C-c d i" . osx-dictionary-search-input)
         ("C-c d x" . osx-dictionary-search-pointer)))

(provide 'init-dict)
