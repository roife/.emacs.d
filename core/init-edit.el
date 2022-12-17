;;; -*- lexical-binding: t -*-

;; Kill ring
(setq kill-ring-max 200)
;;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)


;; [delsel] delete the whole region when selected
;; (use-package delsel
;;   :hook (after-init . delete-selection-mode))


;; [autorevert] Automatically reload files
(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :custom
  ; Only prompts for confirmation when buffer is unsaved.
  (revert-without-query (list "."))
  )


;; [browse-url] Pass a URL to browser
(use-package browse-url
  :defines dired-mode-map
  :bind (("C-, o" . browse-url-at-point)
         ("C-, e" . browse-url-emacs))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-, f" #'browse-url-of-file dired-mode-map))
  )


;; [goto-addr] Click to open URL
(use-package goto-addr
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))


;; [avy] Jump with several key strock
(use-package avy
  :straight t
  :bind (("C-, ," . avy-goto-char)
         ("C-, l" . avy-goto-line))
  :hook (after-init . avy-setup-default)
  :custom
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  (avy-background t)
  ; Do not jump directly even if there is only one candidate, which is confusing
  (avy-single-candidate-jump nil)
  )


;; [avy-pinyin] Avy support for pinyin
(use-package ace-pinyin
  :straight t
  :hook (after-init . ace-pinyin-global-mode))

;; [avy-link] Avy support for links
(use-package ace-link
  :straight t
  :bind (("C-, j" . ace-link-addr))
  :init
  (ace-link-setup-default (kbd "C-, j"))
  )


;; [goto-chg] Goto last change
(use-package goto-chg
  :straight t
  :bind ("C-, ." . goto-last-change))


;; [whitespace] Show visualize TAB, (HARD) SPC, newline
(use-package whitespace
  :hook ((prog-mode conf-mode yaml-mode) . whitespace-mode)
  :custom
  ;; limit line length
  (whitespace-line-column nil)
  ;; automatically clean up bad whitespace
  (whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (whitespace-style '(face lines-tail
                           trailing space-before-tab
                           indentation space-after-tab))
  )


;; [ws-butler] Remove trailing whitespace with lines touched
(use-package ws-butler
  :straight t
  :hook ((prog-mode markdown-mode) . ws-butler-mode))


;; [anzu] Show number of matches in mode-line while searching
(use-package anzu
  :straight t
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode)
  :config
  ;; Ensure anzu state is cleared when searches are done
  (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
  ;; Fix matches segment mirroring across all buffers
  (mapc #'make-variable-buffer-local
        '(anzu--total-matched
          anzu--current-position anzu--state anzu--cached-count
          anzu--cached-positions anzu--last-command
          anzu--last-isearch-string anzu--overflow-p))

  ;; manage modeline segment ourselves
  (setq anzu-cons-mode-line-p nil)
  (defun +modeline-fix-anzu-count (positions here)
    "Calulate anzu count via POSITIONS and HERE."
    (cl-loop for (start . end) in positions
             collect t into before
             when (and (>= here start) (<= here end))
             return (length before)
             finally return 0))

  (advice-add #'anzu--where-is-here :override #'+modeline-fix-anzu-count)
  )


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
        ;; turn off whitespace checking
        ediff-diff-options "-w")
  )


;; [elec-pair] Automatic parenthesis pairing
(use-package elec-pair
  :hook ((prog-mode conf-mode) . electric-pair-mode)
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))


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
;; HACK: Hungry delete doesn't work with paredit
;; See: https://emacs.stackexchange.com/questions/33734/how-to-get-hungry-delete-working-in-paredit-mode
(setq backward-delete-char-untabify-method 'hungry)


;; [subword] Handling capitalized subwords
(use-package subword
  :hook (((prog-mode minibuffer-setup) . subword-mode)))

;; [ialign] Interactive align
(use-package ialign
  :straight t)


;; [paredit] Better paren editing
(use-package paredit
  :straight t
  :hook (((prog-mode conf-mode) . paredit-mode))
  :bind (:map paredit-mode-map
              ("M-<up>" . nil)
              ("M-<down>" . nil)
              (";" . nil))
  :config
  ;; Don't insert space automatically
  (add-to-list 'paredit-space-for-delimiter-predicates
               '(lambda (_ _) (derived-mode-p 'lisp-data-mode)))
  )


;; [hideshow] Code folding
(use-package hideshow
  :hook ((prog-mode conf-mode) . hs-minor-mode)
  :bind (("C-c h `" . hs-toggle-hiding)
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

  ;; Support for more langauges
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
         hs-special-modes-alist))

  ;; Display line counts
  (defun hs-display-code-line-counts (ov)
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
  (setq hs-set-up-overlay #'hs-display-code-line-counts)
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
  )


;; [bookmark] Bookmarks for files and directories
(use-package bookmark
  :config
  (define-derived-mode bookmark-bmenu-mode tabulated-list-mode "Bookmark Menu"
    (setq truncate-lines t)
    (setq buffer-read-only t)
    (setq tabulated-list-format
          `[("" 1) ;; Space to add "*" for bookmark with annotation
            ("" ,(if (icon-displayable-p) 2 0)) ;; Icons
            ("Bookmark" ,bookmark-bmenu-file-column bookmark-bmenu--name-predicate)
            ("Type" 9)
            ,@(if bookmark-bmenu-toggle-filenames
                  '(("File" 0 bookmark-bmenu--file-predicate)))])
    (setq tabulated-list-padding bookmark-bmenu-marks-width)
    (setq tabulated-list-sort-key '("Bookmark" . nil))
    (add-hook 'tabulated-list-revert-hook #'bookmark-bmenu--revert nil t)'
    (setq revert-buffer-function #'bookmark-bmenu--revert)
    (tabulated-list-init-header))
  )


;; [easy-kill] Kill & Mark things easily, extends functionality of M-w
(use-package easy-kill
  :straight t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))


;; [wgrep] Edit a grep buffer and apply changes to the file buffer
(use-package wgrep
  :straight t
  :custom
  (wgrep-auto-save-buffer t))


;; [rg] support for ripgrep
(use-package rg
  :straight t)


;; [fd] support for fd
(use-package fd-dired
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
  :custom
  (mc/cmds-to-run-for-all '(mwim-beginning-of-code-or-line
                            mwim-end-of-code-or-line))
  )


;; [vundo] Undo tree
(use-package vundo
  :straight t
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t)
  )

(provide 'init-edit)
