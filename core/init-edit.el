;;; -*- lexical-binding: t -*-

;; Donot add the duplicates that the same as the last one to kill-ring
(setq kill-do-not-save-duplicates t)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)


;; [autorevert] TODO: Add hooks as what doom has done?
(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :config
  ; Only prompts for confirmation when buffer is unsaved.
  (setq revert-without-query (list "."))
  )


;; [browse-url] Pass a URL to browser
(use-package browse-url
  :defines dired-mode-map
  :bind (("C-, o" . browse-url-at-point)
         ("C-, e" . browse-url-emacs))
  )


;; [goto-addr] Click to open URL
(use-package goto-addr
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))


;; [avy] Jump with several key strock
(use-package avy
  :straight t
  :bind (("C-, ," . avy-goto-char-2)
         ("C-, l" . avy-goto-line))
  :hook (after-init . avy-setup-default)
  )


;; [avy-pinyin] Avy support for pinyin
(use-package ace-pinyin
  :straight t
  :after avy
  )


;; [avy-link] Avy support for links
(use-package ace-link
  :straight t
  :after avy
  :bind (("C-, j" . ace-link-addr))
  :init
  (ace-link-setup-default (kbd "C-, j"))
  )


;; [ws-butler] Remove trailing whitespace with lines touched
(use-package ws-butler
  :straight t
  :hook ((prog-mode markdown-mode) . ws-butler-mode))


;; Use [isearch] to replace anzu
(use-package isearch
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char))
  :config
  (setq isearch-lazy-count t
        lazy-highlight-cleanup nil))


;; [ediff] Diff & patch
(use-package ediff
  :hook ((ediff-before-setup . +ediff-save-window-config)
         ((ediff-quit ediff-suspend) . +ediff-restore-window-config))
  :functions (outline-show-all)
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
        ;; turn off whitespace checking
        ediff-diff-options "-w")
  )


;; [elec-pair] Automatic parenthesis pairing
(use-package elec-pair
  :hook ((prog-mode conf-mode yaml-mode) . electric-pair-mode)
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


;; [expand-region] Select a region quickly
(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))


;; [hungry-delete] Hungry deletion
;; (use-package hungry-delete
;;   :straight t
;;   :hook (after-init . global-hungry-delete-mode)
;;   :config
;;   (setq hungry-delete-chars-to-skip " \t\f\v"
;;         hungry-delete-except-modes
;;         '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))
;; (setq backward-delete-char-untabify-method 'all)


;; [subword] Handling capitalized subwords
(use-package subword
  :hook (((prog-mode minibuffer-setup) . subword-mode)))


;; [ialign] Interactive align
(use-package ialign
  :straight t)


;; [hideshow] Code folding
(use-package hideshow
  :hook ((prog-mode conf-mode yaml-mode) . hs-minor-mode)
  :bind (("C-c h TAB" . hs-toggle-hiding)
         ("C-c h ~" . hs-toggle-all))
  :config
  ;; More functions
  ;; @see https://karthinks.com/software/simple-folding-with-hideshow/
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-toggle-all ()
    "Toggle hide/show all."
    (interactive)
    (pcase last-command
      ('hs-toggle-all
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
        (_ (hs-hide-all))))

  ;; Display line counts
  (defun +hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (concat
                    " "
                    (propertize
                     (format "... [%d lines]"
                             (count-lines (overlay-start ov) (overlay-end ov)))
                     'face '(:inherit shadow :height 0.8))
                    " "))))
  (setq hs-set-up-overlay #'+hs-display-code-line-counts)

  ;; hide-show by indentation
  (defun +fold--hideshow-empty-line-p (_)
  (string= "" (string-trim (thing-at-point 'line 'no-props))))

  (defun +fold--hideshow-geq-or-empty-p (base-indent)
    (or (+fold--hideshow-empty-line-p base-indent)
        (>= (current-indentation) base-indent)))

  (defun +fold--hideshow-g-or-empty-p (base-indent)
    (or (+fold--hideshow-empty-line-p base-indent)
        (> (current-indentation) base-indent)))

  (defun +fold--hideshow-seek (start direction before skip predicate base-indent)
    "Seeks forward (if direction is 1) or backward (if direction is -1) from start, until predicate
fails. If before is nil, it will return the first line where predicate fails, otherwise it returns
the last line where predicate holds."
    (save-excursion
      (goto-char start)
      (goto-char (point-at-bol))
      (let ((bnd (if (> 0 direction)
                     (point-min)
                   (point-max)))
            (pt (point)))
        (when skip (forward-line direction))
        (cl-loop while (and (/= (point) bnd) (funcall predicate base-indent))
                 do (progn
                      (when before (setq pt (point-at-bol)))
                      (forward-line direction)
                      (unless before (setq pt (point-at-bol)))))
        pt)))

  (defun +fold-hideshow-indent-range (&optional point)
    "Return the point at the begin and end of the text block with the same (or
greater) indentation. If `point' is supplied and non-nil it will return the
begin and end of the block surrounding point."
    (save-excursion
      (when point
        (goto-char point))
      (let ((base-indent (current-indentation))
            (begin (point))
            (end (point)))
        (setq begin (+fold--hideshow-seek begin -1 t nil #'+fold--hideshow-geq-or-empty-p base-indent)
              begin (+fold--hideshow-seek begin 1 nil nil #'+fold--hideshow-g-or-empty-p base-indent)
              end   (+fold--hideshow-seek end 1 t nil #'+fold--hideshow-geq-or-empty-p base-indent)
              end   (+fold--hideshow-seek end -1 nil nil #'+fold--hideshow-empty-line-p base-indent))
        (list begin end base-indent))))

  (defun +fold-hideshow-forward-block-by-indent-fn (_arg)
    (let ((start (current-indentation)))
      (forward-line)
      (unless (= start (current-indentation))
        (let ((range (+fold-hideshow-indent-range)))
          (goto-char (cadr range))
          (end-of-line)))))

  ;; support for special modes
  (setq hs-special-modes-alist
        (append
         '((yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
                      ""
                      "#"
                      +fold-hideshow-forward-block-by-indent-fn nil)
           (ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
                      "end\\|[]}]"
                      "#\\|=begin"
                      ruby-forward-sexp)
           (matlab-mode "if\\|switch\\|case\\|otherwise\\|while\\|for\\|try\\|catch"
                        "end"
                        nil (lambda (_arg) (matlab-forward-sexp)))
           (nxml-mode "<!--\\|<[^/>]*[^/]>"
                      "-->\\|</[^/>]*[^/]>"
                      "<!--" sgml-skip-tag-forward nil)
           (latex-mode
            ;; LaTeX-find-matching-end needs to be inside the env
            ("\\\\begin{[a-zA-Z*]+}\\(\\)" 1)
            "\\\\end{[a-zA-Z*]+}"
            "%"
            (lambda (_arg)
              ;; Don't fold whole document, that's useless
              (unless (save-excursion
                        (search-backward "\\begin{document}"
                                         (line-beginning-position) t))
                (LaTeX-find-matching-end)))
            nil))
         hs-special-modes-alist
         '((t))))
  )


;; [project] Project manager
(use-package project
  :bind (:map project-prefix-map
              ("m" . magit-status)
              ("s" . shell))
  :config
  (setq project-switch-commands '((project-find-file "Find file")
                                  (project-find-regexp "Find regexp")
                                  (project-dired "Dired")
                                  (project-eshell "Eshell")
                                  (project-search "Search")
                                  (shell "Shell")
                                  (magit-status "Magit")))

  (defun +project-previous-buffer (arg)
    "Toggle to the previous buffer that belongs to current project."
    (interactive "P")
    (unless arg
      (if-let ((pr (project-current)))
          (switch-to-buffer
           (->> (project--buffer-list pr)
                (--remove (or (minibufferp it)
                              (get-buffer-window-list it)))
                (car))))))

  ;; Use [fd] to find file in project
  (defun +search-project-files-with-fd (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -H -t f -0 . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))
  (cl-defmethod project-files ((project (head local)) &optional dirs)
    "Override `project-files' to use `fd' in local projects."
    (mapcan #'+search-project-files-with-fd
            (or dirs (list (project-root project)))))
  )


;; [bookmark] Bookmarks for files and directories
(use-package bookmark)


;; [easy-kill] Kill & Mark things easily, extends functionality of M-w
(use-package easy-kill
  :straight t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))


;; [wgrep] Edit a grep buffer and apply changes to the file buffer
(use-package wgrep
  :straight t
  :config
  (setq wgrep-auto-save-buffer t)
  )


;; [rg] support for ripgrep
(use-package rg
  :straight t)


;; [multiple-cursors] Multi-cursor
(use-package multiple-cursors
  :straight t
  :bind (("C-c m l"       . mc/edit-lines)
         ("C-c m n"       . mc/mark-next-like-this)
         ("C-c m p"       . mc/mark-previous-like-this)
         ("C-c m A"       . mc/mark-all-like-this)
         ("C-c m N"       . mc/skip-to-next-like-this)
         ("C-c m P"       . mc/skip-to-previous-like-this)
         ("s-S-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (setq mc/cmds-to-run-for-all '(mwim-beginning-of-code-or-line
                                 mwim-end-of-code-or-line))
  )


;; [vundo] Undo tree
(use-package vundo
  :straight t
  :config
  (setq vundo-compact-display t)
  )


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
  )
