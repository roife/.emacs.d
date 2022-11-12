;;; -*- lexical-binding: t -*-

;; [olivetti]
(use-package olivetti
  :straight t
  :hook ((markdown-mode . olivetti-mode)))


;; [edit-indirect] Edit code blocks indirectly
(use-package edit-indirect
  :straight t)

;; [pangu]
(use-package pangu-spacing
  :straight t)

(provide 'init-writing)
