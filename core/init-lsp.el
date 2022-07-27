;;; -*- lexical-binding: t -*-

;; [eglot] LSP support
(use-package eglot
  :straight t
  :hook (((prog-mode yaml-mode conf-mode) . '+lsp-start))
  :init
  (defun +lsp-start()
    "Start eglot session in specific major modes."
    (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode)
      (eglot-ensure)))
  :config
  (setq eglot-events-buffer-size 0)
  )

(provide 'init-lsp)
