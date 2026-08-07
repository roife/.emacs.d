;;; -*- lexical-binding: t -*-

;; (setq mac-option-modifier 'meta
;;       mac-command-modifier 'super
;;       mac-right-command-modifier 'left)

;; [osx-dictionary] macOS native dictionary app
(use-package osx-dictionary
  :straight t
  :bind (("C-c d i" . osx-dictionary-search-input)
         ("C-c d d" . osx-dictionary-search-pointer)))

(use-package emt
  :straight (:host github :repo "roife/emt"
                   :files ("*.el" "module/*" "module"))
  :init
  (setq emt-lib-path
        (concat (no-littering-expand-var-file-name "modules/libEMT")
                module-file-suffix))
  :hook (after-init . emt-mode))

(add-hook! ns-system-appearance-change-functions
  (defun +mac-auto-change-theme-with-system (&rest _)
    (+load-theme)))

;; Prevent accidental touch
(unbind-key "C-<wheel-down>")
(unbind-key "C-<wheel-up>")

(global-set-key (kbd "s-a") #'mark-whole-buffer)
(global-set-key (kbd "s-x") #'kill-region)
(global-set-key (kbd "s-s") #'save-buffer)
(global-set-key (kbd "s-v") #'yank)
(global-set-key (kbd "s-c") #'copy-region-as-kill)
(global-set-key (kbd "s-z") #'undo)
(global-set-key (kbd "s-Z") #'undo-redo)
(global-set-key (kbd "s-f") #'isearch-forward)
(global-set-key (kbd "s-w") #'tab-close)
(global-set-key (kbd "s-t") #'tab-new)
(global-set-key (kbd "s-o") #'other-window)
(global-set-key (kbd "s-,") nil)
