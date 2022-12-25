;;; -*- lexical-binding: t -*-

(use-package prog-mode
  :hook ((prog-mode . global-prettify-symbols-mode)))


;; [comint]
(use-package comint
  :config
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 2048))


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
        dumb-jump-aggressive nil
        dumb-jump-default-project user-emacs-directory)
  )


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
        citre-prompt-language-for-ctags-command t)

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
      (error (if (fboundp #'xref-go-back)
                 (call-interactively #'xref-go-back)
               (call-interactively #'xref-pop-marker-stack)))))

  ;; Use Citre xref backend as a [fallback]
  (define-advice xref--create-fetcher (:around (fn &rest args) fallback)
    (let ((fetcher (apply fn args))
          (citre-fetcher
           (let ((xref-backend-functions '(citre-xref-backend t)))
             (ignore xref-backend-functions)
             (apply fn args))))
      (lambda ()
        (or (with-demoted-errors "%s, fallback to citre"
              (funcall fetcher))
            (funcall citre-fetcher)))))

  ;; Integration with [eglot] as super capf
  ;; (defalias #'+eglot-citre-capf
  ;;   (cape-super-capf #'eglot-completion-at-point #'citre-completion-at-point))
  ;;
  ;; (add-hook 'eglot-managed-mode-hook
  ;;           (lambda () (if (eglot-managed-p)
  ;;                     (add-to-list 'completion-at-point-functions #'+eglot-citre-capf))))

  (require 'xref)
  (dolist (func '(find-function
                  consult-imenu
                  consult-ripgrep
                  citre-jump))
    (advice-add func :before
                (lambda (&rest r) (xref-push-marker-stack (point-marker)))))
  )


;; [smartparens] Better handle for parenthesis
(use-package smartparens
  :straight t
  :hook ((prog-mode conf-mode yaml-mode) . smartparens-strict-mode)
  :config
  (require 'smartparens-config)

  (setq
   ;; TODO:
   ;; Overlays are too distracting and not terribly helpful. show-parens does
   ;; this for us already (and is faster), so...
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil
   ;; The default is 100, but smartparen's scans are expensive, so reduce it
   sp-max-prefix-length 25
   ;; No pair has any business being longer than 4 characters, or set them buffer locally
   sp-max-pair-length 4
   )

  ;; TODO: Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  ;; [minibuffer]
  ;; Enable `smartparens-mode' in the minibuffer for `eval-expression'.
  (add-hook 'eval-expression-minibuffer-setup-hook
            (lambda () (when smartparens-global-mode
                    (smartparens-mode +1))))

  ;; We're likely writing lisp in the minibuffer, therefore, disable these quote pair
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)

  ;; Expand {|} => { | }
  ;; Expand {|} => {
  ;;   |
  ;; }
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))


  ;; In lisps ( should open a new form if before another parenthesis
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

  ;; Major-mode specific fixes
  (sp-local-pair 'ruby-mode "{" "}"
                 :pre-handlers '(:rem sp-ruby-pre-handler)
                 :post-handlers '(:rem sp-ruby-post-handler))

  ;; Don't eagerly escape Swift style string interpolation
  (sp-local-pair 'swift-mode "\\(" ")" :when '(sp-in-string-p))

  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))

  ;; Reasonable default pairs for HTML-style comments
  (sp-local-pair (append sp--html-modes '(markdown-mode gfm-mode))
                 "<!--" "-->"
                 :unless '(sp-point-before-word-p sp-point-before-same-p)
                 :actions '(insert) :post-handlers '(("| " "SPC")))

  ;; Disable electric keys in C modes because it interferes with smartparens
  ;; and custom bindings. We'll do it ourselves (mostly).
  (with-eval-after-load 'cc-mode
    (setq-default c-electric-flag nil)
    (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")" "\177"))
      (define-key c-mode-base-map key nil))

    ;; Smartparens and cc-mode both try to autoclose angle-brackets
    ;; intelligently. The result isn't very intelligent (causes redundant
    ;; characters), so just do it ourselves.
    (bind-key :map
              c++-mode-map
              ("<" nil)
              (">" nil))

    (defun +default-cc-sp-point-is-template-p (id action context)
      "Return t if point is in the right place for C++ angle-brackets."
      (and (sp-in-code-p id action context)
           (cond ((eq action 'insert)
                  (sp-point-after-word-p id action context))
                 ((eq action 'autoskip)
                  (/= (char-before) 32)))))

    (defun +default-cc-sp-point-after-include-p (id action context)
      "Return t if point is in an #include."
      (and (sp-in-code-p id action context)
           (save-excursion
             (goto-char (line-beginning-position))
             (looking-at-p "[ 	]*#include[^<]+"))))

    ;; ...and leave it to smartparens
    (sp-local-pair '(c++-mode objc-mode)
                   "<" ">"
                   :when '(+default-cc-sp-point-is-template-p
                           +default-cc-sp-point-after-include-p)
                   :post-handlers '(("| " "SPC")))

    (sp-local-pair '(c-mode c++-mode objc-mode java-mode)
                   "/*!" "*/"
                   :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))

  ;; Expand C-style comment blocks.
  (defun +default-open-doc-comments-block (&rest _ignored)
    (save-excursion
      (newline)
      (indent-according-to-mode)))
  (sp-local-pair
   '(js2-mode typescript-mode rjsx-mode rust-mode c-mode c++-mode objc-mode
              csharp-mode java-mode php-mode css-mode scss-mode less-css-mode
              stylus-mode scala-mode)
   "/*" "*/"
   :actions '(insert)
   :post-handlers '(("| " "SPC")
                    (" | " "*")
                    ("|[i]\n[i]" "RET")))

  (with-eval-after-load 'smartparens-ml
    (sp-with-modes '(tuareg-mode fsharp-mode)
      (sp-local-pair "(*" "*)" :actions nil)
      (sp-local-pair "(*" "*"
                     :actions '(insert)
                     :post-handlers '(("| " "SPC") ("|[i]*)[d-2]" "RET")))))

  (with-eval-after-load 'smartparens-markdown
    (sp-with-modes '(markdown-mode gfm-mode)
      (sp-local-pair "```" "```" :post-handlers '(:add ("||\n[i]" "RET")))

      ;; The original rules for smartparens had an odd quirk: inserting two
      ;; asterixex would replace nearby quotes with asterixes. These two rules
      ;; set out to fix this.
      (sp-local-pair "**" nil :actions :rem)
      (sp-local-pair "*" "*"
                     :actions '(insert skip)
                     :unless '(:rem sp-point-at-bol-p)
                     ;; * then SPC will delete the second asterix and assume
                     ;; you wanted a bullet point. * followed by another *
                     ;; will produce an extra, assuming you wanted **|**.
                     :post-handlers '(("[d1]" "SPC") ("|*" "*"))))

    ;; This keybind allows * to skip over **.
    (bind-key :map markdown-mode-map
              "*" (general-predicate-dispatch
                   nil
                   (looking-at-p "\\*\\* *")
                   (lambda (&rest _) (interactive) (forward-char 2)))))

  ;; Removes haskell-mode trailing braces
  (with-eval-after-load 'smartparens-haskell
    (sp-with-modes '(haskell-mode haskell-interactive-mode)
      (sp-local-pair "{-" "-}" :actions :rem)
      (sp-local-pair "{-#" "#-}" :actions :rem)
      (sp-local-pair "{-@" "@-}" :actions :rem)
      (sp-local-pair "{-" "-")
      (sp-local-pair "{-#" "#-")
      (sp-local-pair "{-@" "@-")))

  (with-eval-after-load 'smartparens-python
    (sp-with-modes 'python-mode
      ;; Automatically close f-strings
      (sp-local-pair "f\"" "\"")
      (sp-local-pair "f\"\"\"" "\"\"\"")
      (sp-local-pair "f'''" "'''")
      (sp-local-pair "f'" "'"))
    ;; Original keybind interferes with smartparens rules
    (define-key python-mode-map (kbd "DEL") nil)
    ;; Interferes with the def snippet in doom-snippets
    ;; TODO Fix this upstream, in doom-snippets, instead
    (setq sp-python-insert-colon-in-function-definitions nil))

  (with-eval-after-load 'web-mode
    (defun +web-is-auto-close-style-3 (_id action _context)
      (and (eq action 'insert)
           (eq web-mode-auto-close-style 3)))
    (sp-local-pair 'web-mode "<" ">" :unless '(:add +web-is-auto-close-style-3))

    ;; let smartparens handle these
    (setq web-mode-enable-auto-quoting nil
          web-mode-enable-auto-pairing t)

    ;; 1. Remove web-mode auto pairs whose end pair starts with a latter
    ;;    (truncated autopairs like <?p and hp ?>). Smartparens handles these
    ;;    better.
    ;; 2. Strips out extra closing pairs to prevent redundant characters
    ;;    inserted by smartparens.
    (dolist (alist web-mode-engines-auto-pairs)
      (setcdr alist
              (cl-loop for pair in (cdr alist)
                       unless (string-match-p "^[a-z-]" (cdr pair))
                       collect (cons (car pair)
                                     (string-trim-right (cdr pair)
                                                        "\\(?:>\\|]\\|}\\)+\\'")))))
    (setq web-mode-auto-pairs (delq nil web-mode-auto-pairs)))
  )


;; [quickrun] Run commands quickly
(use-package quickrun
  :straight t
  :bind (("C-c r"  . quickrun)))


;; [flymake] On-the-fly syntax checker
(use-package flymake
  :hook ((prog-mode . flymake-mode))
  :bind (("C-c f ]" . flymake-goto-next-error)
         ("C-c f [" . flymake-goto-prev-error)
         ("C-c f b" . flymake-show-buffer-diagnostics))
  :config
  (setq
   flymake-diagnostic-functions nil
   ;; Check only on save
   flymake-no-changes-timeout 1)
  )


;; Langs
(use-package csv-mode
  :straight t)


(use-package cmake-mode
  :straight t)


(use-package json-mode
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
  :straight (:host github :repo "nverno/llvm-mode" :files ("dist" "*.el")))


(use-package swift-mode
  :straight t)


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


(use-package yaml-mode
  :straight t)


;; [Proof General] Proof General is a generic front-end for proof assistants
(use-package proof-general
  :straight t
  :config
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


;; [Eglot] LSP support
(use-package eglot
  :straight t
  :hook ((c-mode c++-mode rust-mode python-mode haskell-mode) . eglot-ensure)
  :config
  (setq eldoc-echo-area-display-truncation-message nil
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 2
        eglot-connect-timeout 10
        eglot-autoshutdown t
        ;; use global completion styles
        completion-category-defaults nil)
  )


;; [consult-eglot] Eglot support for consult
(use-package consult-eglot
  :after consult eglot
  :straight t
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . consult-eglot-symbols)))
