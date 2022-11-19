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


(provide 'init-dict)
