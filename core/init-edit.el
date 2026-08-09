;;; -*- lexical-binding: t -*-

;; [kill-ring]
(setq
 ;; Do not add the duplicates that the same as the last one to kill-ring
 kill-do-not-save-duplicates t
 ;; Save clipboard contents into kill-ring before replace them
 save-interprogram-paste-before-kill t)

(kill-ring-deindent-mode)


;; Make script file executable with `chmod +x' after save
(add-hook! after-save-hook #'executable-make-buffer-file-executable-if-script-p)


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
  :hook ((prog-mode markdown-ts-mode) . ws-butler-mode))


;; [editorconfig] Respect project-local formatting rules
(use-package editorconfig
  :hook (find-file . editorconfig-mode))


;; [apheleia] Format buffers asynchronously without moving point
(use-package apheleia
  :straight t
  :commands (apheleia-format-buffer)
  :bind (("C-c f f" . apheleia-format-buffer))
  :hook (after-init . apheleia-global-mode)
  :config
  (setq apheleia-hide-log-buffers t))


;; [jinx] Spell checker
(use-package jinx
  :straight t
  :hook ((text-mode . jinx-mode)
         (prog-mode . jinx-mode))
  :bind (:map jinx-mode-map
              ("C-c f >" . jinx-next)
              ("C-c f <" . jinx-previous)
              ("C-c f ." . jinx-correct)
              ("C-c f /" . jinx-correct-all))
  :config
  (add-to-list 'jinx-exclude-regexps '(t "\\cc")))


;; [ediff] Diff & patch
(use-package ediff
  :hook ((ediff-before-setup . +ediff-save-window-config)
         ((ediff-quit ediff-suspend) . +ediff-restore-window-config))
  :config
  ;; unfold outlines when using ediff
  (with-eval-after-load 'outline
    (add-hook! ediff-prepare-buffer-hook #'outline-show-all))

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
  :hook ((prog-mode conf-mode yaml-mode org-mode markdown-ts-mode minibuffer-mode) . electric-pair-mode)
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
  :hook (after-init . beginend-global-mode)
  :config
  (beginend-define-mode telega-root-mode
    (progn
      (goto-char (max (point-min)
                      (1- telega-root-view--ewocs-marker)))
      (telega-button-forward 1 nil 'no-error))
    (progn
      (telega-button-backward 1 nil 'no-error)))

  ;; Install explicitly in case `beginend-global-mode' is already enabled.
  (add-hook! telega-root-mode-hook #'beginend-telega-root-mode))


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
  (sudo-edit-indicator-mode t))


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

;; [dtdr-indent] Detect indentation size
(use-package dtrt-indent
  :straight t
  :config
  (dtrt-indent-global-mode 1))

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
                            aw-select
                            windmove-do-window-select
                            pager-page-up
                            tab-bar-select-tab
                            pop-to-mark-command
                            pop-global-mark
                            xref-go-back
                            xref-find-definitions
                            xref-find-references))
  (add-to-list 'dogears-ignore-modes 'gnus-article-mode)

  (defadvice! +dogears--keep-record-style-only-a (fn &rest args)
    :around #'dogears--place
    (mapcar (lambda (item)
              (cond ((stringp item) (substring-no-properties item))
                    ((stringp (cdr item))
                     (cons (car item) (substring-no-properties (cdr item))))
                    (item)))
            (apply fn args)))

  (defadvice! +dogears--format-record-a (record)
    :override #'dogears--format-record
    (apply #'format
           "%s %-3.3s %-30.30s %-30.30s %-0.15s %0.0s %0.0s%-s"
           (dogears--format-record-list record)))

  (defadvice! +dogears--relevance-without-remote-access-a (fn record)
    :around #'dogears--relevance
    (let ((filename (map-elt (cdr record) 'filename)))
      (if (and (stringp filename) (file-remote-p filename))
          ;; In particular, do not ask Tramp to expand the remote `~' or
          ;; let `project-current' inspect the remote directory.
          (let (file-name-handler-alist)
            (funcall fn record))
        (funcall fn record))))

  (defvar consult--source-dogears
    (list :name "Dogears"
          :narrow ?d
          :category 'dogears
          :items (lambda ()
                   (mapcar (lambda (place)
                             (propertize (dogears--format-record place)
                                         'consult--candidate place))
                           dogears-list))
          :action (lambda (cand)
                    (dogears-go (get-text-property 0 'consult--candidate cand)))))
  (with-eval-after-load 'consult
    (add-to-list 'consult-buffer-sources 'consult--source-dogears 'append)))


;; [expreg]
(use-package expreg
  :straight t
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))
