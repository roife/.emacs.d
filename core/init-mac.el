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
  :hook (after-init . emt-mode))

(use-package exec-path-from-shell
  :straight t
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "EDITOR"))

  (setq exec-path-from-shell-check-startup-files nil) ;
  (setq exec-path-from-shell-arguments '("-l" )) ;remove -i read form .zshenv
  (exec-path-from-shell-initialize))

(add-hook! ns-system-appearance-change-functions
  (defun +mac-auto-change-theme-with-system (&rest _)
    (+load-theme)))

;; Prevent accidental touch
(unbind-key "C-<wheel-down>")
(unbind-key "C-<wheel-up>")

;; Selects the most-recently-used ASCII-capable keyboard input source when in chineses input mode
;; (mac-auto-ascii-mode)

;; (setq mac-pass-control-to-system nil)
;; (setq mac-pass-command-to-system nil)
