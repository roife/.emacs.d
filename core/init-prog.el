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
  (add-hook! compilation-filter-hook
    (defun +compilation--truncate-buffer-h (&optional _string)
      "Rate-limit `comint-truncate-buffer' in compilation buffers."
      (require 'comint)
      (when (> (buffer-size)
               (* 80 comint-buffer-maximum-size))
        (let ((gc-cons-threshold most-positive-fixnum)
              (gc-cons-percentage 1.0))
          (with-silent-modifications
            (comint-truncate-buffer))))))

  (add-hook! compilation-filter-hook
    (defun +compilation--colorize-h ()
      "Apply ANSI color codes to the compilation buffer."
      (require 'ansi-color)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point)))))
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


;; [Eglot] LSP support
(use-package eglot
  :straight (:type built-in)
  :commands (eglot eglot-ensure)
  :preface
  (defconst +eglot-auto-start-modes
    '(c-mode c++-mode rust-mode python-mode java-mode
             c-ts-mode c++-ts-mode rust-ts-mode python-ts-mode)
    "Major modes where Eglot should start automatically.")
  :init
  (dolist (mode +eglot-auto-start-modes)
    (add-hook (intern (format "%s-hook" mode)) #'eglot-ensure))
  :custom-face (eglot-highlight-symbol-face ((t (:underline t))))
  :bind (:map eglot-mode-map
              ("M-<return>" . eglot-code-actions)
              ("M-/" . eglot-find-typeDefinition)
              ("M-?" . xref-find-references))
  :config
  (setq eglot-events-buffer-config '(:size 0 :format full)
        eglot-autoshutdown t
        ;; eglot-report-progress 'messages
        eglot-documentation-renderer 'markdown-ts-view-mode
        eglot-code-action-indications nil)

  ;; eglot has it's own strategy by default
  (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (setq-default eglot-workspace-configuration
                '((:pyls . (:plugins (:jedi_completion (:fuzzy t))))
                  (:rust-analyzer . (:cargo (:allFeatures t :allTargets t :features "full")
                                            :checkOnSave :json-false
                                            :completion (:termSearch (:enable t)
                                                                     :fullFunctionSignatures (:enable t))
                                            :hover (:memoryLayout (:size "both")
                                                                  :show (:traitAssocItems 5)
                                                                  :documentation (:keywords (:enable :json-false)))
                                            :inlayHints (:lifetimeElisionHints (:enable "skip_trivial" :useParameterNames t)
                                                                               :closureReturnTypeHints (:enable "always")
                                                                               :discriminantHints (:enable t)
                                                                               :genericParameterHints (:lifetime (:enable t)))
                                            :semanticHighlighting (:operator (:specialization (:enable t))
                                                                             :punctuation (:enable t :specialization (:enable t)))
                                            :workspace (:symbol (:search (:kind "all_symbols"
                                                                                :scope "workspace_and_dependencies")))
                                            :references (:excludeImports t
                                                                         :excludeTests t)
                                            :lru (:capacity 1024)
                                            :diagnostics (:enable :json-false)))
                  (:typescript . (:preferences (:importModuleSpecifierPreference "non-relative")))
                  (:java . (:configuration
                            (:runtimes [(:name "JavaSE-17"
                                               :path "/opt/homebrew/opt/openjdk@17/libexec/openjdk.jdk/Contents/Home/")
                                        (:name "JavaSE-21"
                                               :path "/opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home/"
                                               :default t)])
                            :import (:gradle (:enabled t
                                                       :wrapper (:enabled t)))
                            :autobuild (:enabled :json-false)
                            :extendedClientCapabilities (:classFileContentsSupport t)))))

  (defun jdtls-command-contact (&optional interactive)
    (let* ((jdtls-java-home (getenv "JDTLS_JAVA_HOME"))
           (project-root (project-root (project-current t)))
           (data-dir
            (file-name-concat
             (no-littering-expand-var-file-name "lsp-cache/")
             (md5 (expand-file-name project-root)))))
      `("env" ,(concat "JAVA_HOME=" jdtls-java-home)
        "jdtls" "--jvm-arg=-Xmx16G" "-data" ,data-dir)))
  (push '(java-mode . jdtls-command-contact) eglot-server-programs)

  ;; we call eldoc manually
  (add-hook! eglot-managed-mode-hook
    (defun +eglot-disable-eldoc-mode ()
      (when (eglot-managed-p)
        (eldoc-mode -1))))
  )


(use-package eglot-tempel
  :straight t
  :after (eglot tempel)
  :config
  (eglot-tempel-mode 1))


(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode)
  (setq eglot-booster-io-only t))


;; [Eldoc]
(use-package eldoc
  :bind (("C-h h" . eldoc))
  :config
  (setq eldoc-echo-area-display-truncation-message t
        eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil
        eglot-extend-to-xref t
        eldoc-help-at-pt t))


;; [help]
(use-package help
  :bind (("s-?" . display-local-help)))


;; [consult-eglot] Eglot support for consult
(use-package consult-eglot
  :after consult eglot
  :straight t
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . consult-eglot-symbols)))


;; [webpaste] Web Pastebin
(use-package webpaste
  :straight t
  :commands webpaste-paste-buffer-or-region
  :config
  (setq webpaste-paste-confirmation t
        webpaste-add-to-killring t
        webpaste-provider-priority '("paste.rs")))


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
        citre-edit-ctags-options-manually t
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
  :config
  (setq quickrun-focus-p nil))


;; [dape] Debug Adapter Protocol client
(use-package dape
  :straight t
  :commands (dape)
  :bind (:map prog-mode-map
              ("C-c D" . dape))
  :preface
  (defun +dape-save-buffers-h ()
    "Save file-visiting buffers before starting a debug session."
    (save-some-buffers t t))
  :init
  (setq dape-buffer-window-arrangement 'right)
  :config
  (add-hook 'dape-start-hook #'+dape-save-buffers-h))


;; [flymake] On-the-fly syntax checker
(use-package flymake
  :straight (:type built-in)
  :preface
  (defun +flymake-mode-unless-eglot-auto-starts ()
    "Enable Flymake unless Eglot will enable it after connecting."
    (unless (memq major-mode +eglot-auto-start-modes)
      (flymake-mode 1)))

  :hook ((prog-mode . +flymake-mode-unless-eglot-auto-starts))
  :bind (("C-c f ]" . flymake-goto-next-error)
         ("C-c f [" . flymake-goto-prev-error)
         ("C-c f b" . flymake-show-buffer-diagnostics)
         :map flymake-mode-map
         ("<left-fringe> <mouse-1>" . nil)
         ("<right-fringe> <mouse-1>" . nil))
  :config
  (setq flymake-show-diagnostics-at-end-of-line 'short))

;; Langs
(use-package cc-mode
  :config
  (setq c-basic-offset 4)
  (c-set-offset 'case-label '+))


(use-package csv-mode
  :straight t)


(use-package rainbow-csv
  :straight (:host github :repo "emacs-vs/rainbow-csv"))


(use-package rmsbolt
  :straight t)


(use-package llvm-mode
  :straight (:host github :repo "nverno/llvm-mode" :files ("*.el")))


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


(use-package rust-playground
  :straight t)


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


;; [treesit]
(use-package treesit
  :when (treesit-available-p)
  :init
  (setq treesit-enabled-modes t
        treesit-auto-install-grammar 'always
        treesit-font-lock-level 4))


;; [indent-bars] Show indent guides
(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  :config
  (setq indent-bars-display-on-blank-lines nil
        indent-bars-width-frac 0.1
        indent-bars-color '(highlight :face-bg t :blend 0.2)
        indent-bars-zigzag nil
        indent-bars-highlight-current-depth nil
        indent-bars-pattern "."))


;; [direnv] Buffer-local project environments
(use-package envrc
  :straight t
  :hook (emacs-startup . envrc-global-mode))


;; [minuet-ai] AI-powered inline code completion
(use-package minuet
  :straight (:host github :repo "milanglacier/minuet-ai.el")
  :hook (prog-mode . minuet-auto-suggestion-mode)
  :bind (("M-i" . #'minuet-complete-with-minibuffer)
         :map minuet-active-mode-map
         ("M-p" . #'minuet-previous-suggestion)
         ("M-n" . #'minuet-next-suggestion)
         ("C-e" . #'minuet-accept-suggestion)
         ("C-g" . #'minuet-dismiss-suggestion))
  :custom-face
  (minuet-suggestion-face ((t (:inherit font-lock-comment-face :slant italic :weight normal :underline nil))))
  :config
  (setq minuet-provider 'openai-fim-compatible)

  (plist-put minuet-openai-fim-compatible-options :end-point "https://api.deepseek.com/beta/completions")
  (plist-put minuet-openai-fim-compatible-options :model "deepseek-v4-flash")
  (plist-put minuet-openai-fim-compatible-options :name "Deepseek")
  (plist-put minuet-openai-fim-compatible-options :api-key
             (lambda ()
               (require 'gptel)
               (gptel-api-key-from-auth-source "api.deepseek.com" "apikey")))

  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 56)
  (minuet-set-optional-options minuet-openai-fim-compatible-options :top_p 0.9))
