;;; -*- lexical-binding: t -*-

;; Ligature
(mac-auto-operator-composition-mode t)

(setq mac-option-modifier 'meta)

(setq mac-command-modifier 'super)
(define-key global-map [?\s-a] #'mark-whole-buffer)
(define-key global-map [?\s-x] #'kill-region)
(define-key global-map [?\s-s] #'save-buffer)
(define-key global-map [?\s-v] #'yank)
(define-key global-map [?\s-c] #'copy-region-as-kill)
(define-key global-map [?\s-z] #'undo)
(define-key global-map [?\s-Z] #'undo-redo)
(define-key global-map [?\s-f] #'isearch-forward)
(define-key global-map [?\s-w] #'delete-window)

(provide 'init-mac)
