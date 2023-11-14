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
  :straight (:host github :repo "roife/emt")
  :hook (after-init . emt-mode))

(add-hook! ns-system-appearance-change-functions
  (defun +mac-auto-change-theme-with-system (&rest _)
    (+load-theme)))

;; Selects the most-recently-used ASCII-capable keyboard input source when in chineses input mode
;; (mac-auto-ascii-mode)

;; (setq mac-pass-control-to-system nil)
;; (setq mac-pass-command-to-system nil)
