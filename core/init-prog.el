;;; -*- lexical-binding: t -*-

(use-package prog-mode
  :hook ((prog-mode . global-prettify-symbols-mode)))


;; [xref] Cross reference
(use-package xref
  :init
  (setq xref-search-program 'ripgrep)
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


(use-package citre
  :straight t
  :bind (:map prog-mode-map
              ("C-c c j" . citre-jump+)
              ("C-c c k" . citre-jump-back+)
              ("C-c c p" . citre-peek)
              ("C-c c a" . citre-ace-peek)
              ("C-c c u" . citre-update-this-tags-file))
  :init
  (require 'citre-config)
  (setq citre-auto-enable-citre-mode-modes '(prog-mode)
        citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t)

  (defun citre-jump+ ()
    "Jump to the definition of the symbol at point. Fallback to `xref-find-definitions'."
    (interactive)
    (condition-case _
        (citre-jump)
      (error (call-interactively #'xref-find-definitions))))

  (defun citre-jump-back+ ()
    "Go back to the position before last `citre-jump'. Fallback to `xref-go-back'."
    (interactive)
    (condition-case _
        (citre-jump-back)
      (error (if (fboundp #'xref-go-back)
                 (call-interactively #'xref-go-back)
               (call-interactively #'xref-pop-marker-stack)))))
  :config
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

  (defalias #'my-eglot-citre-capf
    (cape-super-capf #'eglot-completion-at-point #'citre-completion-at-point))

  (add-hook 'eglot-managed-mode-hook
            (defun my-toggle-citre-eglot-capf ()
              (if (eglot-managed-p)
                  (add-to-list 'completion-at-point-functions #'my-eglot-citre-capf))))
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
  (setq-default flymake-diagnostic-functions nil)
  ;; Check only on save
  (setq flymake-no-changes-timeout 1)
  )


;; Langs
(use-package csv-mode
  :straight t)


(use-package cmake-mode
  :straight t)


(use-package json-mode
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
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))


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


;; [prettier]
(use-package prettier
    :straight t
    :hook ((js-mode css-mode sgml-mode web-mode) . prettier-mode)
    :init (setq prettier-pre-warm 'none))


;; [Eglot] LSP support
(use-package eglot
  :straight t
  :hook ((c-mode c++-mode rust-mode python-mode haskell-mode) . eglot-ensure)
  :config
  (setq eldoc-echo-area-use-multiline-p 1
        eldoc-echo-area-display-truncation-message nil
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 2
        eglot-autoshutdown t
        )
  )


;; [copilot] Copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook ((markdown-mode org-mode) . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  )

;; [consult-eglot] Eglot support for consult
(use-package consult-eglot
  :straight t)

(provide 'init-prog)
