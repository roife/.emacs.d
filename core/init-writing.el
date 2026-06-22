;;; -*- lexical-binding: t -*-


;; [visual-fill-column] Center text in markdown and org
;; (use-package visual-fill-column
;;   :straight t
;;   :hook (text-mode . visual-fill-column-mode)
;;   :config
;;   (setq-default visual-fill-column-center-text t))


;; [visual-line-mode] Soft line-wrapping
(add-hook 'text-mode-hook 'visual-line-mode)


;; [edit-indirect] Edit code blocks indirectly
(use-package edit-indirect
  :straight t)


;; [pangu] Add pangu spaces
(use-package pangu-spacing
  :straight t)


;; [typst-ts-mode]
(use-package typst-ts-mode
  :straight (:host sourcehut :repo "meow_king/typst-ts-mode")
  :custom
  (typst-ts-mode-watch-options "--open"))

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
