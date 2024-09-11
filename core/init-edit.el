;;; -*- lexical-binding: t -*-

;; [kill-ring]
(setq
 ;; Do not add the duplicates that the same as the last one to kill-ring
 kill-do-not-save-duplicates t
 ;; Save clipboard contents into kill-ring before replace them
 save-interprogram-paste-before-kill t)


;; [autorevert] TODO: Add hooks as what doom has done?
(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :config
                                        ; Only prompts for confirmation when buffer is unsaved.
  (setq revert-without-query (list "."))
  )


;; [ws-butler] Remove trailing whitespace with lines touched
(use-package ws-butler
  :straight t
  :hook ((prog-mode markdown-mode) . ws-butler-mode))


;; [ediff] Diff & patch
(use-package ediff
  :hook ((ediff-before-setup . +ediff-save-window-config)
         ((ediff-quit ediff-suspend) . +ediff-restore-window-config))
  :config
  ;; unfold outlines when using ediff
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'outline-show-all))

  ;; Restore window config after quitting ediff
  (defvar +ediff-saved-window-config nil)
  (defun +ediff-save-window-config ()
    (setq +ediff-saved-window-config (current-window-configuration)))
  (defun +ediff-restore-window-config ()
    (when (window-configuration-p +ediff-saved-window-config)
      (set-window-configuration +ediff-saved-window-config)))

  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally
        ediff-highlight-all-diffs t
        ;; turn off whitespace checking
        ediff-diff-options "-w")
  )


;; [elec-pair] Automatic parenthesis pairing
(use-package elec-pair
  :hook ((prog-mode conf-mode yaml-mode org-mode markdown-mode minibuffer-mode) . electric-pair-mode)
  :config
  (setq electric-pair-inhibit-predicate 'electric-pair-default-inhibit)
  )


;; [mwim] Better C-a C-e for programming
(use-package mwim
  :straight t
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))


;; [beginend] Better M-< M-> for programming
(use-package beginend
  :straight t
  :hook (after-init . beginend-global-mode))


;; Alternatives to [hungry-delete]
(setq backward-delete-char-untabify-method 'hungry)


;; [subword] Handling capitalized subwords
(use-package subword
  :hook (((prog-mode minibuffer-setup) . subword-mode)))


;; [easy-kill] Kill & Mark things easily, extends functionality of M-w
(use-package easy-kill
  :straight t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))


;; [sudo-edit] edit file with su permissions
(use-package sudo-edit
  :straight t
  :config
  (sudo-edit-indicator-mode t)
  )


;; [puni]
(use-package puni
  :straight t
  :hook ((prog-mode sgml-mode nxml-mode tex-mode eval-expression-minibuffer-setup) . puni-mode)
  :bind (:map puni-mode-map
              ("DEL" . +puni-hungry-delete))
  :config
  (defun +puni-hungry-delete ()
    (interactive)
    (if (looking-back "^[[:blank:]]+")
        (let* ((puni-mode nil)
               (original-func (key-binding (kbd "DEL"))))
          ;; original-func is what `DEL' would be if puni-mode were disabled
          (if (eq original-func 'delete-backward-char)
              (backward-delete-char-untabify 1)
            (call-interactively original-func)))
      (puni-backward-delete-char)))
  )


;; [embrace] Add/change/delete pairs of symbol
(use-package embrace
  :straight t
  :bind ("C-." . embrace-commander)
  :hook (org-mode . embrace-org-mode-hook)
  )

;; [dogears] Jump to the last edit location
(use-package dogears
  :straight t
  :hook (after-init . dogears-mode)
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar))
  :config
  (setq dogears-idle 1
        dogears-limit 200
        dogears-position-delta 20)
  (setq dogears-functions '(find-file recenter-top-bottom
                                      other-window switch-to-buffer
                                      aw-select toggle-window-split
                                      windmove-do-window-select
                                      pager-page-down pager-page-up
                                      tab-bar-select-tab
                                      pop-to-mark-command
                                      pop-global-mark
                                      goto-last-change
                                      xref-go-back
                                      xref-find-definitions
                                      xref-find-references)))
