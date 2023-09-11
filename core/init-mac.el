;;; -*- lexical-binding: t -*-

(setq mac-option-modifier 'meta
      mac-command-modifier 'super
      mac-right-command-modifier 'hyper)


(define-key global-map [?\s-a] #'mark-whole-buffer)
(define-key global-map [?\s-x] #'kill-region)
(define-key global-map [?\s-s] #'save-buffer)
(define-key global-map [?\s-v] #'yank)
(define-key global-map [?\s-c] #'copy-region-as-kill)
(define-key global-map [?\s-z] #'undo)
(define-key global-map [?\s-Z] #'undo-redo)
(define-key global-map [?\s-f] #'isearch-forward)
(define-key global-map [?\s-w] #'tab-close)
(define-key global-map [?\s-t] #'tab-new)
(define-key global-map [?\H-o] #'other-window)

;; [osx-dictionary] macOS native dictionary app
(use-package osx-dictionary
  :straight t
  :bind (("C-c d i" . osx-dictionary-search-input)
         ("C-c d d" . osx-dictionary-search-pointer)))


;; Automatically switch theme based on the theme of macOS
(add-hook 'mac-effective-appearance-change-hook #'(lambda (_) (+load-theme)))
