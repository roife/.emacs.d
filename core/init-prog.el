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

  ;; Integration with [eglot]
  (defalias #'+eglot-citre-capf
    (cape-super-capf #'eglot-completion-at-point #'citre-completion-at-point))

  (add-hook 'eglot-managed-mode-hook
            (lambda () (if (eglot-managed-p)
                      (add-to-list 'completion-at-point-functions #'+eglot-citre-capf))))

  (dolist (func '(find-function
                  consult-imenu
                  consult-ripgrep
                  citre-jump))
    (advice-add func :before (lambda (&rest r)
                               (xref-push-marker-stack (point-marker)))))
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
  :straight t)


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
        eglot-autoshutdown t)
  )


;; [copilot] Copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook ((markdown-mode org-mode) . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion))
  )


;; [consult-eglot] Eglot support for consult
(use-package consult-eglot
  :after consult eglot
  :straight t
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . consult-eglot-symbols)))
