;;; -*- lexical-binding: t -*-

;; [visual-fill-column] Center text in markdown and org
(use-package visual-fill-column
  :straight t
  :config
  (setq-default visual-fill-column-center-text t))

;; [visual-line-mode] Soft line-wrapping
(add-hook! (org-mode-hook markdown-ts-mode-hook) #'visual-line-mode)

;; [pangu] Add pangu spaces
(use-package pangu-spacing
  :straight t)

(use-package markdown-ts-mode
  :straight (:type built-in)
  :init
  ;; Packages such as gptel also load Markdown's file associations.
  (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode-maybe))
  :config
  (setq-default markdown-ts-hide-markup t
                markdown-ts-inline-images t)
  ;; Fold bodies on open, keeping all heading levels visible.
  (setq markdown-ts-default-folding 'fold-headings)

  ;; markdown-ts-mode gives all 6 heading levels the same face; inherit the
  ;; Org level faces instead so they differ and follow the current theme.
  (dotimes (i 6)
    (set-face-attribute (intern (format "markdown-ts-heading-%d" (1+ i))) nil
                        :inherit (intern (format "org-level-%d" (1+ i)))
                        :weight 'unspecified)))


;; [md-babel] Execute Markdown fenced blocks through Org Babel
(use-package md-babel
  :straight (:host github :repo "roife/md-babel")
  :hook (markdown-ts-mode . md-babel-mode))


;; [typst-ts-mode]
(use-package typst-ts-mode
  :straight (:host sourcehut :repo "meow_king/typst-ts-mode")
  :init
  (setq typst-ts-lsp-download-path
        (no-littering-expand-var-file-name "lsp/tinymist/tinymist"))
  (make-directory (file-name-directory typst-ts-lsp-download-path) t)
  :custom
  (typst-ts-watch-options "--open"))

;; [auctex]
(use-package tex
  :straight auctex
  :config
  (setq TeX-parse-self t             ; parse on load
        TeX-auto-save t              ; parse on save
        ;; Use hidden directories for AUCTeX files.
        TeX-auto-local ".auctex-auto"
        TeX-style-local ".auctex-style"
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        ;; Don't start the Emacs server when correlating sources.
        TeX-source-correlate-start-server nil
        ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
        TeX-electric-sub-and-superscript t
        ;; Just save, don't ask before each compilation.
        TeX-save-query nil))


;; [cdlatex]
(use-package cdlatex
  :straight t)


;; [reftex]
(use-package reftex
  :straight (:type built-in))


(use-package mermaid-mode
  :straight t
  :config
  (setq mermaid-output-format ".svg"))
