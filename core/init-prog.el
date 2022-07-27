;;; -*- lexical-binding: t -*-

(use-package prog-mode
  :hook ((prog-mode . global-prettify-symbols-mode)
         (emacs-lisp-mode . (lambda () (push '("<=" . ?≤) prettify-symbols-alist)))))

;; [xref] Cross reference
(use-package xref
  :init
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))

;; [dumb-jump] Jump to definition (integrated with xref)
(use-package dumb-jump
  :straight t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'ido))

;; [editorconfig] Code styles
(use-package editorconfig
  :straight t
  :hook ((prog-mode . editorconfig-mode)))

;; [quickrun] Run commands quickly
(use-package quickrun
  :straight t
  :bind (("C-<f5>" . quickrun)
         ("C-c X"  . quickrun)))

;; [flymake] On-the-fly syntax checker
(use-package flymake
  :hook ((prog-mode . flymake-mode))
  :bind (("C-c ]" . flymake-goto-next-error)
         ("C-c [" . flymake-goto-prev-error)
         ("C-c C-b" . flymake-show-diagnostics-buffer)))

;; PL mode
(use-package csv-mode
  :straight t)
(use-package cmake-mode
  :straight t)
(use-package csharp-mode
  :straight t)
(use-package lua-mode
  :straight t)
(use-package plantuml-mode
  :straight t)
(use-package rmsbolt          ; A compiler output viewer
  :straight t)
(use-package scala-mode
  :straight t)
(use-package swift-mode
  :straight t)
(use-package rustic
  :straight t
  :config
  (setq rustic-lsp-client 'eglot))
(use-package rust-playground
  :straight t)

(provide 'init-prog)
