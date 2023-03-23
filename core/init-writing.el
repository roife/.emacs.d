;;; -*- lexical-binding: t -*-

(use-package visual-fill-column
  :straight t
  :hook ((markdown-mode org-mode) . +center-text)
  :config
  (defun +center-text ()
    (visual-fill-column-mode)
    (setq visual-fill-column-center-text t)))


;; [edit-indirect] Edit code blocks indirectly
(use-package edit-indirect
  :straight t)


;; [pangu] Add pangu spaces
(use-package pangu-spacing
  :straight t)


;; [mardown-mode]
(use-package markdown-mode
  :straight t
  :custom-face
  (markdown-code-face ((t (:inherit nil))))
  :config
  (setq markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-nested-imenu-heading-index t
        markdown-fontify-code-blocks-natively t)
  (add-to-list 'markdown-code-lang-modes '("rust" . rustic-mode))
  (add-to-list 'markdown-code-lang-modes '("verilog" . verilog-mode))
  (add-to-list 'markdown-code-lang-modes '("agda" . agda2-mode)))

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
  :straight t)
