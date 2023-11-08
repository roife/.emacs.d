;;; -*- lexical-binding: t -*-

(use-package prog-mode
  :hook ((prog-mode . prettify-symbols-mode)))


;; [compile]
(use-package compile
  :config
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)

  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer)
  )


;; [comment]
;; comment over empty lines
(setq comment-empty-lines t)


;; [xref] Cross reference
(use-package xref
  :config
  (setq
   xref-search-program 'ripgrep
   xref-show-definitions-function #'xref-show-definitions-completing-read
   xref-show-xrefs-function #'xref-show-definitions-completing-read
   xref-history-storage 'xref-window-local-history)

  (defadvice! +xref--push-marker-stack-a (&rest rest)
    :before '(find-function consult-imenu consult-ripgrep citre-jump)
    (xref-push-marker-stack (point-marker)))
  )


;; [Eglot] LSP support
(use-package eglot
  :hook ((c-mode c++-mode rust-mode python-mode haskell-mode) . eglot-ensure)
  :custom-face (eglot-highlight-symbol-face ((t (:underline t))))
  :bind (:map eglot-mode-map
         ("M-<return>" . eglot-code-actions))
  :config
  (setq eglot-events-buffer-size 0
        eglot-connect-timeout 10
        eglot-autoshutdown t)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  )


;; [Eldoc]
(use-package eldoc
  :config
  (setq eldoc-echo-area-display-truncation-message t
        eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil
        eglot-extend-to-xref t))


;; [consult-eglot] Eglot support for consult
(use-package consult-eglot
  :after consult eglot
  :straight t
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . consult-eglot-symbols)))


;; [copilot]
(use-package copilot
  :when (executable-find "node")
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook ((prog-mode . +copilot-check-and-auto-activate))
  :config
  (setq copilot-indent-warning-suppress t)

  (defun +copilot-check-and-auto-activate ()
    (interactive)
    (when (and (not (+temp-buffer-p (current-buffer)))
               (project-current))
      (copilot-mode)))

  (defun +copilot-complete ()
    (interactive)
    (or (copilot-accept-completion)
        (mwim-end-of-code-or-line)))

  (with-eval-after-load 'copilot
    (define-key copilot-mode-map (kbd "C-e") #'+copilot-complete))

  (defun +copilot-complete-word ()
    (interactive)
    (or (copilot-accept-completion-by-word 1)
        (forward-word)))

  (with-eval-after-load 'copilot
    (define-key copilot-mode-map (kbd "M-f") #'+copilot-complete-word))
  )


;; [webpaste] Web Pastebin
(use-package webpaste
  :straight t
  :commands webpaste-paste-buffer-or-region
  :config
  (setq webpaste-paste-confirmation t
        webpaste-add-to-killring t
        webpaste-provider-priority '("paste.mozilla.org" "dpaste.org" "ix.io")))


;; [dumb-jump] Jump to definition (integrated with xref, a fallback of lsp)
(use-package dumb-jump
  :straight t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'completing-read
        dumb-jump-aggressive t
        dumb-jump-default-project user-emacs-directory)
  )


;; [citre] Ctags-infra
(use-package citre
  :straight t
  :bind (:map prog-mode-map
              ("C-c c j" . +citre-jump)
              ("C-c c k" . +citre-jump-back)
              ("C-c c p" . citre-peek)
              ("C-c c a" . citre-ace-peek)
              ("C-c c u" . citre-update-this-tags-file))
  :init
  (require 'citre-config)
  :config
  (setq citre-auto-enable-citre-mode-modes '(prog-mode)
        citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t
        citre-enable-capf-integration t)

  (defun +citre-jump ()
    "Jump to the definition of the symbol at point. Fallback to `xref-find-definitions'."
    (interactive)
    (condition-case _
        (citre-jump)
      (error (call-interactively #'xref-find-definitions))))

  (defun +citre-jump-back ()
    "Go back to the position before last `citre-jump'. Fallback to `xref-go-back'."
    (interactive)
    (condition-case _
        (citre-jump-back)
      (error (call-interactively #'xref-go-back))))

  ;; Use Citre xref backend as a [fallback]
  (defadvice! +citre--xref-fallback-a (fn &rest args)
    :around #'xref--create-fetcher
    (let ((fetcher (apply fn args))
          (citre-fetcher
           (let ((xref-backend-functions '(citre-xref-backend t)))
             (ignore xref-backend-functions)
             (apply fn args))))
      (lambda ()
        (or (with-demoted-errors "%s, fallback to citre"
              (funcall fetcher))
            (funcall citre-fetcher)))))
  )


;; [quickrun] Run commands quickly
(use-package quickrun
  :straight t
  :bind (("C-c r"  . quickrun))
  :config
  (setq quickrun-focus-p nil))


;; [flymake] On-the-fly syntax checker
(use-package flymake
  :hook ((prog-mode . flymake-mode))
  :bind (("C-c f ]" . flymake-goto-next-error)
         ("C-c f [" . flymake-goto-prev-error)
         ("C-c f b" . flymake-show-buffer-diagnostics))
  :config
  (setq
   flymake-diagnostic-functions nil)
  )

;; Langs
(use-package cc-mode
  :config
  (setq c-basic-offset 4)
  (c-set-offset 'case-label '+))


(use-package csv-mode
  :straight t)


(use-package cmake-mode
  :straight t)


(use-package rmsbolt ; A compiler output viewer
  :straight t)


(use-package scala-mode
  :straight t
  :config
  (setq
   scala-indent:align-parameters t
   ;; indent block comments to first asterix, not second
   scala-indent:use-javadoc-style t)
  )


(use-package firrtl-mode
  :straight t)


(use-package llvm-mode
  :straight (:host github :repo "nverno/llvm-mode" :files ("*.el")))


(use-package swift-mode
  :straight t)


(use-package js
  :config
  (setq js-indent-level 2))


(use-package css-mode
  :config
  (setq css-indent-offset 2))


(use-package rustic
  :straight t
  :config
  (setq
   rustic-lsp-client 'eglot
   rustic-indent-method-chain t
   rust-prettify-symbols-alist nil)
  )


(use-package rust-playground
  :straight t)


(use-package haskell-mode
  :straight t
  :config
  (setq
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t)
  )


(use-package verilog-mode
  :straight t
  :config
  (setq verilog-align-ifelse t
        verilog-auto-delete-trailing-whitespace t
        verilog-auto-inst-param-value t
        verilog-auto-inst-vector nil
        verilog-auto-lineup (quote all)
        verilog-auto-newline nil
        verilog-auto-save-policy nil
        verilog-auto-template-warn-unused t
        verilog-case-indent 4
        verilog-cexp-indent 4
        verilog-highlight-grouping-keywords t
        verilog-highlight-modules t
        verilog-indent-level 4
        verilog-indent-level-behavioral 4
        verilog-indent-level-declaration 4
        verilog-indent-level-module 4
        verilog-tab-to-comment t))


;; [yaml]
(use-package yaml-mode
  :straight t)


;; [graphviz-dot]
(use-package graphviz-dot-mode
  :straight t
  :config
  (setq graphviz-dot-indent-width 4))


;; [Proof General] Proof General is a generic front-end for proof assistants
(use-package proof-general
  :straight t
  :init
  (setq proof-splash-enable nil)
  )


;; Major mode for editing web templates
(use-package web-mode
  :straight t
  :mode "\\.[px]?html?\\'"
  :mode "\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'"
  :mode "\\.erb\\'"
  :mode "\\.[lh]?eex\\'"
  :mode "\\.jsp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.ejs\\'"
  :mode "\\.hbs\\'"
  :mode "\\.mustache\\'"
  :mode "\\.svelte\\'"
  :mode "\\.twig\\'"
  :mode "\\.jinja2?\\'"
  :mode "\\.eco\\'"
  :mode "wp-content/themes/.+/.+\\.php\\'"
  :mode "templates/.+\\.php\\'"
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-enable-html-entities-fontification t
   web-mode-auto-close-style 1)
  )


;; [skewer-mode] Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :straight t
  :hook ((js-mode              . skewer-mode)
         (css-mode             . skewer-css-mode)
         ((html-mode web-mode) . skewer-html-mode)))


;; [ts]
(use-package typescript-mode
  :straight t
  :mode ("\\.ts[x]\\'" . typescript-mode))


;; [agda]
(use-package agda
  :no-require t
  :when (executable-find "agda-mode")
  :init
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))
