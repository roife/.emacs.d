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
   ;; TODO: https://github.com/oantolin/embark/issues/162#issuecomment-785039305
   ;; Maybe a bug?
   ;; xref-show-definitions-function #'xref-show-definitions-completing-read
   ;; xref-show-xrefs-function #'xref-show-definitions-completing-read
   xref-history-storage 'xref-window-local-history)

  (defadvice! +xref--push-marker-stack-a (&rest rest)
    :before '(find-function consult-imenu consult-ripgrep citre-jump)
    (xref-push-marker-stack (point-marker)))
  )


;; [imenu]
(use-package imenu
  :hook (prog-mode . imenu--make-index-alist)
  :config
  (setq imenu-auto-rescan t))


;; [Eglot] LSP support
(use-package eglot
  :hook ((c-mode c++-mode rust-mode python-mode java-mode c-ts-mode c++-ts-mode rust-ts-mode python-ts-mode) . eglot-ensure)
  :custom-face (eglot-highlight-symbol-face ((t (:underline t))))
  :bind (:map eglot-mode-map
              ("M-<return>" . eglot-code-actions)
              ("M-/" . eglot-find-typeDefinition))
  :config
  (setq eglot-events-buffer-config '(:size 0 :format full)
        eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-report-progress 'messages)

  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; eglot has it's own strategy by default
  (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
              completion-at-point-functions (cl-nsubst
                                             (cape-capf-noninterruptible
                                              (cape-capf-buster #'eglot-completion-at-point
                                                                #'string-prefix-p))
                                             'eglot-completion-at-point
                                             completion-at-point-functions))
  (setq-default eglot-workspace-configuration
                '((:pyls . (:plugins (:jedi_completion (:fuzzy t))))
                  (:rust-analyzer . (:cargo (:allFeatures t :allTargets t :features "full")
                                            :checkOnSave :json-false
                                            :diagnostics (:enable :json-false)
                                            :completion (:termSearch (:enable t)
                                                                     :fullFunctionSignatures (:enable t))
                                            :hover (:memoryLayout (:size "both")
                                                                  :show (:traitAssocItems 5)
                                                                  :documentation (:keywords (:enable :json-false)))
                                            :inlayHints(:bindingModeHints (:enable t)
                                                                          :lifetimeElisionHints (:enable "skip_trivial" :useParameterNames t)
                                                                          :closureReturnTypeHints (:enable "always")
                                                                          :discriminantHints (:enable t)
                                                                          :genericParameterHints (:lifetime (:enable t)))
                                            ;; :lens (:references (:adt (:enable t)
                                            ;;                          :enumVariant (:enable t)
                                            ;;                          :trait (:enable t)
                                            ;;                          :method (:enable t)))
                                            ;; :semanticHighlighting (:operator (:specialization (:enable t))
                                            ;;                                  :punctuation (:enable t :specialization (:enable t)))
                                            :workspace (:symbol (:search (:kind "all_symbols"
                                                                                :scope "workspace_and_dependencies")))
                                            :lru (:capacity 1024)))
                  (:typescript . (:preferences (:importModuleSpecifierPreference "non-relative")))
                  (:gopls . ((staticcheck . t)
                             (matcher . "CaseSensitive")))))

  ;; we call eldoc manually by C-h .
  (add-hook! eglot-managed-mode-hook
    (defun +eglot-disable-eldoc-mode ()
      (when (eglot-managed-p)
        (eldoc-mode -1))))
  )


(use-package eglot-tempel
  :straight t
  :after (eglot tempel)
  :init
  (eglot-tempel-mode)
  )


(use-package eglot-x
  :straight (:host github :repo "nemethf/eglot-x")
  :hook (eglot-managed-mode . eglot-x-setup))


;; [Eldoc]
(use-package eldoc
  :bind (("C-h ." . eldoc))
  :config
  (setq eldoc-echo-area-display-truncation-message t
        eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil
        eglot-extend-to-xref t))


;; [help]
(use-package help
  :bind (("s-?" . display-local-help)))


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
  :hook ((prog-mode org-mode markdown-mode) . +copilot-activate)
  :bind (:map copilot-mode-map
         ("C-e" . +copilot-complete)
         ("M-f" . +copilot-complete-word))
  :config
  (copilot--start-agent)

  (setq copilot-indent-warning-suppress t)

  (defun +copilot-activate ()
    (interactive)
    (when (and (not (+temp-buffer-p (current-buffer)))
               (not (or (string-prefix-p "*GPTel-" (buffer-name))
                        (string= "ChatGPT" (buffer-name)))))
      (copilot-mode)))

  (defun +copilot-complete ()
    (interactive)
    (or (copilot-accept-completion)
        (mwim-end-of-code-or-line)))

  (defun +copilot-complete-word ()
    (interactive)
    (or (copilot-accept-completion-by-word 1)
        (forward-word)))
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
;; (use-package flymake
;;   :hook ((prog-mode . flymake-mode))
;;   :bind (("C-c f ]" . flymake-goto-next-error)
;;          ("C-c f [" . flymake-goto-prev-error)
;;          ("C-c f b" . flymake-show-buffer-diagnostics))
;;   :config
;;   (setq
;;    flymake-diagnostic-functions nil)
;;   )

;; Langs
(use-package cc-mode
  :config
  (setq c-basic-offset 4)
  (c-set-offset 'case-label '+))


(use-package elisp-mode)


(use-package csv-mode
  :straight t)


(use-package rainbow-csv
  :straight (:host github :repo "emacs-vs/rainbow-csv"))


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
   scala-indent:use-javadoc-style t))


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


(use-package rust-mode
  :straight t
  :init
  (setq rust-mode-treesitter-derive t
        rust-format-goto-problem nil)
  )

(use-package cargo
  :straight t
  :hook ((rust-mode rust-ts-mode) . cargo-minor-mode))


(use-package rust-playground
  :straight t)


(use-package go-mode
  :straight t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  )


(use-package haskell-mode
  :straight t
  :config
  (setq
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t))


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


;; [toml]
(use-package toml-mode
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
  (setq proof-splash-enable nil))


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
   web-mode-auto-close-style 1))


(use-package js-ts-mode
  :mode ("\\.js[x]?\\'" . js-ts-mode)
  :config
  (setq js-ts-indent-level 2))


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


(use-package moonbit-mode
  :straight (:host github :repo "tonyfettes/moonbit-mode"))


;; [treesit]
(use-package treesit
  :when (treesit-available-p)
  :init
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (python-mode . python-ts-mode)
          (javascript-mode . javascript-ts-mode)
          (typescript-mode . typescript-ts-mode)
          ;; (rust-mode . rust-ts-mode)
          ))

  (setq treesit-language-source-alist
        '((c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (typst "https://github.com/uben0/tree-sitter-typst")
          (moonbit "https://github.com/moonbitlang/tree-sitter-moonbit")))

  (dolist (lang treesit-language-source-alist)
  (unless (treesit-language-available-p (car lang))
    (treesit-install-language-grammar (car lang))))

  (setq treesit-font-lock-level 4))


;; [dash] api docset
(use-package dash-docs
  :straight t)


;; [indent-bars] Show indent guides
(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  :config
  (setq indent-bars-display-on-blank-lines nil
        indent-bars-width-frac 0.2
        indent-bars-color '(highlight :face-bg t :blend 0.2)
        indent-bars-zigzag nil
        indent-bars-highlight-current-depth nil
        indent-bars-pattern "|"
        indent-bars-prefer-character t)
  )
