;;; -*- lexical-binding: t -*-

(use-package prog-mode
  :hook ((prog-mode . global-prettify-symbols-mode)))


;; [xref] Cross reference
(use-package xref
  :init
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read)
  )


;; [dumb-jump] Jump to definition (integrated with xref)
(use-package dumb-jump
  :straight t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'ido
        dumb-jump-aggressive nil)
  )


;; [editorconfig] Code styles
(use-package editorconfig
  :straight t
  :hook ((prog-mode . editorconfig-mode)))

;; [quickrun] Run commands quickly
(use-package quickrun
  :straight t
  :bind (("C-c r"  . quickrun)))

;; [flymake] On-the-fly syntax checker
(use-package flymake
  :straight t
  :hook ((prog-mode . flymake-mode))
  :bind (("C-c ]" . flymake-goto-next-error)
         ("C-c [" . flymake-goto-prev-error)
         ("C-c C-b" . flymake-show-buffer-diagnostics))
  :config
  (setq-default flymake-diagnostic-functions nil)
  ;; Check only on save
  (setq flymake-no-changes-timeout 1)
  )

;; PL mode
(use-package csv-mode
  :straight t)


(use-package cmake-mode
  :straight t)


(use-package json-mode
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


(use-package haskell-mode
  :straight t)


(use-package verilog-mode
  :straight t)


;; Major mode for editing web templates
(use-package web-mode
  :straight t
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))


;; [skewer-mode] Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :straight t
  :hook (((js-mode js2-mode)   . skewer-mode)
         (css-mode             . skewer-css-mode)
         ((html-mode web-mode) . skewer-html-mode)))


;; [ts]
(use-package typescript-mode
  :straight t
  :mode ("\\.ts[x]\\'" . typescript-mode))


;; [prettier]
(use-package prettier
    :straight t
    :hook ((js-mode js2-mode css-mode sgml-mode web-mode) . prettier-mode)
    :init (setq prettier-pre-warm 'none))


;; [Eglot] LSP support
(use-package eglot
  :straight t
  :hook ((c-mode c++-mode rust-mode python-mode) . eglot-ensure)
  :config
  (setq eldoc-echo-area-use-multiline-p 1
        eldoc-echo-area-display-truncation-message nil
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 2
        eglot-autoshutdown t
        )
  )


(use-package consult-eglot
  :straight t)

(provide 'init-prog)
