;;; -*- lexical-binding: t -*-

;; [compile]
(use-package compile
  :preface
  (defvar +compilation-flymake-diagnostics nil
    "Flymake diagnostics parsed from the latest compilation buffer.")

  (defun +compilation-flymake--file (location)
    "Return the file named by compilation LOCATION."
    (let* ((file-struct (compilation--loc->file-struct location))
           (file-spec (compilation--file-struct->file-spec file-struct))
           (file (car file-spec)))
      (if (bufferp file)
          (buffer-file-name file)
        (compilation--expand-fn
         (or (cadr file-spec) default-directory)
         (format (or (car (compilation--file-struct->formats file-struct))
                     "%s")
                 file)))))

  (defun +compilation-flymake--message-start (message position)
    "Return the beginning of parsed MESSAGE around POSITION.
Compilation text properties may cover only a hyperlink inside the full
parser match, so recover the complete match from MESSAGE's parser rule."
    (let* ((rule (compilation--message->rule message))
           (item (and rule
                      (cdr (assq rule compilation-error-regexp-alist-alist))))
           (regexp (car-safe item)))
      (or (and (stringp regexp)
               (save-excursion
                 (goto-char position)
                 (end-of-line)
                 (when (and (re-search-backward regexp nil t)
                            (<= (match-beginning 0) position)
                            (<= position (match-end 0)))
                   (match-beginning 0))))
          (save-excursion
            (goto-char position)
            (line-beginning-position)))))

  (defun +compilation-flymake--collect ()
    "Convert the current compilation buffer's parsed message cache."
    (save-excursion
      (let ((entries
             (progn
               (goto-char (point-min))
               (cl-loop
                with seen = (make-hash-table :test #'eq)
                for match = (text-property-search-forward
                             'compilation-message nil nil t)
                while match
                for message = (prop-match-value match)
                for location = (compilation--message->loc message)
                for type = (compilation--message->type message)
                for position = (+compilation-flymake--message-start
                                message (prop-match-beginning match))
                ;; Informational locations usually provide context for the
                ;; preceding warning or error, rather than a separate
                ;; diagnostic.  Keep them in that diagnostic's text range.
                unless (or (zerop type) (gethash message seen))
                collect (progn
                          (puthash message t seen)
                          (vector location type position))))))
        (cl-loop
         for tail on entries
         for entry = (car tail)
         for next = (cadr tail)
         for location = (aref entry 0)
         for type = (aref entry 1)
         for position = (aref entry 2)
         for end = (if next (aref next 2) (point-max))
         collect
         (flymake-make-diagnostic
          (+compilation-flymake--file location)
          (cons (or (compilation--loc->line location) 1)
                (compilation--loc->col location))
          nil
          (if (= type 1) 'flymake-warning 'flymake-error)
          (save-excursion
            (goto-char position)
            (string-trim
             (buffer-substring-no-properties
              (line-beginning-position) end)))
          '+compilation-flymake)))))

  (defun +compilation-flymake--publish-project-diagnostics ()
    "Replace Compilation-owned project diagnostics."
    (setq flymake-list-only-diagnostics
          (cl-loop for (file . diagnostics) in flymake-list-only-diagnostics
                   for others = (cl-remove '+compilation-flymake diagnostics
                                           :key #'flymake-diagnostic-data)
                   when others collect (cons file others)))
    (dolist (diagnostic +compilation-flymake-diagnostics)
      (push diagnostic
            (alist-get (flymake-diagnostic-buffer diagnostic)
                       flymake-list-only-diagnostics nil nil #'equal))))

  (defun +compilation-flymake-backend (report-fn &rest _args)
    "Report compilation diagnostics belonging to the current buffer.
Diagnostics for all files are published separately for project listings."
    (let (diagnostics)
      (dolist (cached +compilation-flymake-diagnostics)
        (when (and buffer-file-name
                   (file-equal-p buffer-file-name
                                 (flymake-diagnostic-buffer cached)))
          (when-let* ((position (flymake-diagnostic-beg cached))
                      (region (flymake-diag-region
                               (current-buffer)
                               (car position) (cdr position))))
            (push (flymake-make-diagnostic
                   (current-buffer) (car region) (cdr region)
                   (flymake-diagnostic-type cached)
                   (flymake-diagnostic-message cached))
                  diagnostics))))
      (funcall report-fn (nreverse diagnostics))))

  (defun +compilation-flymake-finish-h (buffer _status)
    "Publish the parsed messages from compilation BUFFER."
    (require 'flymake)
    (let ((old-files (mapcar #'flymake-diagnostic-buffer
                             +compilation-flymake-diagnostics)))
      (setq +compilation-flymake-diagnostics
            (with-current-buffer buffer
              (+compilation-flymake--collect)))
      (+compilation-flymake--publish-project-diagnostics)
      (dolist (file (delete-dups
                     (append old-files
                             (mapcar #'flymake-diagnostic-buffer
                                     +compilation-flymake-diagnostics))))
        (when-let* ((source (find-buffer-visiting file)))
          (with-current-buffer source
            (when flymake-mode
              (flymake-start)))))))

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

  (add-hook 'compilation-finish-functions #'+compilation-flymake-finish-h)
  )


;; [comment]
;; comment over empty lines
(setq comment-empty-lines t)


;; [xref] Cross reference
(use-package xref
  :init
  (defadvice! +xref--push-marker-stack-a (&rest _)
    :before '(find-function consult-imenu consult-ripgrep citre-jump)
    (require 'xref)
    (xref-push-marker-stack (point-marker)))
  :config
  (setq
   xref-search-program 'ripgrep
   ;; TODO: https://github.com/oantolin/embark/issues/162#issuecomment-785039305
   ;; Maybe a bug?
   ;; xref-show-definitions-function #'xref-show-definitions-completing-read
   ;; xref-show-xrefs-function #'xref-show-definitions-completing-read
   xref-history-storage 'xref-window-local-history))


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
      (remove-hook 'xref-backend-functions #'eglot-xref-backend t)
      (add-hook 'xref-backend-functions #'eglot-xref-backend -50 t)

      (setq-local eldoc-documentation-strategy
                  'eldoc-documentation-compose-eagerly)
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
  :init
  (setq eglot-booster-io-only t)
  :config
  (eglot-booster-mode 1))


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
  (add-hook! xref-backend-functions :depth 80 #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'completing-read
        dumb-jump-aggressive t
        dumb-jump-default-project user-emacs-directory)
  )


;; [citre] Ctags-infra
;; (use-package citre
;;   :straight t
;;   :commands (citre-update-this-tags-file)
;;   :preface
;;   (defun +citre-manage-xref-backend ()
;;     "Register Citre's xref backend at the configured priority."
;;     (remove-hook 'xref-backend-functions #'citre-xref-backend t)
;;     (add-hook 'xref-backend-functions #'citre-xref-backend -25 t))
;;   :bind (:map prog-mode-map
;;               ("C-c r c" . citre-update-this-tags-file))
;;   :hook ((find-file . citre-auto-enable-citre-mode)
;;          (citre-mode . +citre-manage-xref-backend))
;;   :config
;;   (setq citre-default-create-tags-file-location 'global-cache
;;         citre-edit-ctags-options-manually t
;;         citre-auto-enable-citre-mode-modes '(prog-mode))
;;   (setq-default citre-enable-xref-integration nil
;;                 citre-enable-capf-integration t)
;;
;;   (with-eval-after-load 'cc-mode (require 'citre-lang-c))
;;   (with-eval-after-load 'dired (require 'citre-lang-fileref))
;;   (with-eval-after-load 'verilog-mode (require 'citre-lang-verilog))
;;   )


;; [quickrun] Run commands quickly
(use-package quickrun
  :straight t
  :bind (("C-c r r" . quickrun))
  :config
  (setq quickrun-focus-p nil))


;; [dape] Debug Adapter Protocol client
(use-package dape
  :straight t
  :commands (dape)
  :preface
  (defun +dape-save-buffers-h ()
    "Save file-visiting buffers before starting a debug session."
    (save-some-buffers t t))
  :init
  (setq dape-buffer-window-arrangement 'right)
  :config
  (add-hook! dape-start-hook #'+dape-save-buffers-h))


;; [flymake] On-the-fly syntax checker
(use-package flymake
  :straight (:type built-in)
  :preface
  (defun +flymake-show-buffer-diagnostics-single-a (args)
    "Pass at most one diagnostic to `flymake-show-buffer-diagnostics'.
Emacs 31's interactive form returns every diagnostic at point as a
separate argument, although the command accepts only one."
    (if (cdr args) (list (car args)) args))

  :hook ((prog-mode . flymake-mode))
  :bind (("C-c f ]" . flymake-goto-next-error)
         ("C-c f [" . flymake-goto-prev-error)
         ("C-c f b" . flymake-show-buffer-diagnostics)
         ("C-c f p" . flymake-show-project-diagnostics)
         :map flymake-mode-map
         ("<left-fringe> <mouse-1>" . nil)
         ("<right-fringe> <mouse-1>" . nil))
  :config
  (advice-add 'flymake-show-buffer-diagnostics :filter-args
              #'+flymake-show-buffer-diagnostics-single-a)
  (add-hook 'flymake-diagnostic-functions
            #'+compilation-flymake-backend)
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
        rust-format-goto-problem nil))


(use-package fish-mode
  :straight t)


(use-package rust-playground
  :straight t)


(use-package verilog-mode
  :straight (:type built-in)
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
        treesit-auto-install-grammar 'always))


;; [indent-bars] Show indent guides
(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  :config
  ;; Prevent terminal display properties from leaking into inserted text.
  (setf (alist-get 'indent-bars-display
                   (default-value 'text-property-default-nonsticky))
        t)
  (setq indent-bars-display-on-blank-lines nil
        indent-bars-depth-update-delay 0.15
        indent-bars-width-frac 0.1
        indent-bars-color '(highlight :face-bg t :blend 0.2)
        indent-bars-zigzag nil
        indent-bars-highlight-current-depth nil
        indent-bars-pattern "."))


;; [direnv] Buffer-local project environments
(use-package envrc
  :straight t
  :hook (emacs-startup . envrc-global-mode))


;; [log-view-mode]
(use-package logview
  :straight t
  :custom
  (logview-additional-level-mappings
   '(("Pipeline levels" . ((error       "ERROR")
                           (warning     "WARN ")
                           (information "INFO ")
                           (debug       "DEBUG")
                           (trace       "TRACE")))))
  (logview-additional-submodes
   '(("Pipeline" . ((format . "[TIMESTAMP] [LEVEL] [NAME] MESSAGE")
                    (levels . "Pipeline levels"))))))
