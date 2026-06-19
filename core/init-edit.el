;;; -*- lexical-binding: t -*-

;; [kill-ring]
(setq
 ;; Do not add the duplicates that the same as the last one to kill-ring
 kill-do-not-save-duplicates t
 ;; Save clipboard contents into kill-ring before replace them
 save-interprogram-paste-before-kill t)

(kill-ring-deindent-mode)


;; Make script file executable with `chmod +x' after save
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)


;; [autorevert]
(use-package autorevert
  :straight nil
  :hook (find-file . +auto-revert-mode)
  :config
  (setq auto-revert-verbose t
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        ;; Only prompts for confirmation when buffer is unsaved.
        revert-without-query (list "."))

  (defun +visible-buffers ()
    "Return visible buffers across all frames."
    (let (buffers)
      (walk-windows (lambda (window)
                      (push (window-buffer window) buffers))
                    'no-minibuf t)
      (delete-dups buffers)))

  (defun +auto-revert-buffer-h (&rest _)
    "Auto revert current buffer when it is stale."
    (unless (or (active-minibuffer-window)
                (and (not auto-revert-remote-files)
                     buffer-file-name
                     (file-remote-p buffer-file-name)))
      (let ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun +auto-revert-window-buffer-h (window &rest _)
    "Auto revert WINDOW's buffer when it is stale."
    (when (window-live-p window)
      (with-current-buffer (window-buffer window)
        (+auto-revert-buffer-h))))

  (defun +auto-revert-selected-window-h (&optional frame)
    "Auto revert the selected window's buffer in FRAME."
    (+auto-revert-window-buffer-h
     (frame-selected-window (or frame (selected-frame)))))

  (defun +auto-revert-visible-buffers-h (&rest _)
    "Auto revert visible stale buffers."
    (dolist (buffer (+visible-buffers))
      (with-current-buffer buffer
        (+auto-revert-buffer-h))))

  (define-minor-mode +auto-revert-mode
    "A lazy alternative to `global-auto-revert-mode'."
    :global t
    (when global-auto-revert-mode
      (setq +auto-revert-mode nil))
    (let ((fn (if +auto-revert-mode #'add-hook #'remove-hook)))
      (funcall fn 'window-buffer-change-functions #'+auto-revert-window-buffer-h)
      (funcall fn 'window-selection-change-functions #'+auto-revert-selected-window-h)
      (funcall fn 'focus-in-hook #'+auto-revert-visible-buffers-h)
      (funcall fn 'after-save-hook #'+auto-revert-visible-buffers-h)
      (funcall fn 'server-switch-hook #'+auto-revert-buffer-h))))


;; [ws-butler] Remove trailing whitespace with lines touched
(use-package ws-butler
  :straight t
  :hook ((prog-mode markdown-mode) . ws-butler-mode))


;; [editorconfig] Respect project-local formatting rules
(use-package editorconfig
  :hook (find-file . editorconfig-mode))


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
  (setq dogears-functions '(find-file
                            recenter-top-bottom
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
