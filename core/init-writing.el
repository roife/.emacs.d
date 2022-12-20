;;; -*- lexical-binding: t -*-

;; [olivetti] Center align the editing area
(use-package olivetti
  :straight t
  :hook ((markdown-mode . olivetti-mode)))


;; [edit-indirect] Edit code blocks indirectly
(use-package edit-indirect
  :straight t)


;; [pangu] Add pangu spaces
(use-package pangu-spacing
  :straight t)


;; [mardown-mode]
(use-package markdown-mode
  :straight t
  :custom-face
  (markdown-code-face ((t (:inherit nil)))))


(provide 'init-writing)
