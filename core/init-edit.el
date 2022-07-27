;;; -*- lexical-binding: t -*-

;; Kill ring
(setq kill-ring-max 200)
;;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; [delsel] delete the whole region when selected
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; [autorevert] Automatically reload files
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;; [browse-url] Pass a URL to browser
(use-package browse-url
  :defines dired-mode-map
  :bind (("C-, o" . browse-url-at-point)
         ("C-, e" . browse-url-emacs))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-, f" #'browse-url-of-file dired-mode-map)))

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
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

;; [avy-link] Avy support for links
(use-package ace-link
  :straight t
  :bind (("C-, L" . ace-link-addr))
  :hook (after-init . ace-link-setup-default))

;; [avy-pinyin] Avy support for pinyin
(use-package ace-pinyin
  :straight t
  :hook (after-init . ace-pinyin-global-mode))

;; [goto-chg] Goto last change
(use-package goto-chg
  :straight t
  :bind ("C-, ." . goto-last-change))

;; [aggressive-indent] Aggressively keep code always indented
(use-package aggressive-indent
  :straight t
  :hook (((prog-mode conf-mode yaml-mode) . aggressive-indent-mode)
         ;; WORKAROUND: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(gitconfig-mode asm-mode web-mode html-mode css-mode go-mode scala-mode prolog-inferior-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c-mode 'c++-mode 'csharp-mode
                                     'java-mode 'go-mode 'swift-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line))))))

;; [whitespace] Show visualize TAB, (HARD) SPC, newline
(use-package whitespace
  :hook ((prog-mode conf-mode yaml-mode) . whitespace-mode)
  :config
  (setq
   ;; limit line length
   whitespace-line-column fill-column
   ;; automatically clean up bad whitespace
   whitespace-action '(auto-cleanup)
   ;; only show bad whitespace
   whitespace-style '(face lines-tail
                           trailing space-before-tab
                           indentation space-after-tab)))

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
  (setq anzu-cons-mode-line-p nil) ; manage modeline segment ourselves
  (defun +modeline-fix-anzu-count (positions here)
    "Calulate anzu count via POSITIONS and HERE."
    (cl-loop for (start . end) in positions
             collect t into before
             when (and (>= here start) (<= here end))
             return (length before)
             finally return 0))

  (advice-add #'anzu--where-is-here :override #'+modeline-fix-anzu-count)
  )

;; [drag-stuff] Drag words & lines
(use-package drag-stuff
  :straight t
  :commands drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (setq drag-stuff-modifier 'super)
  (drag-stuff-define-keys))

;; [ediff] Diff & patch
(use-package ediff
  :hook((ediff-prepare-buffer . outline-show-all)   ;; show org ediffs unfolded
        (ediff-quit . winner-undo))                 ;; restore window layout when done
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

;; [elec-pair] Automatic parenthesis pairing
(use-package elec-pair
  :hook (prog-mode . electric-pair-mode)
  :config
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; [mwim] Better C-a C-e for programming
(use-package mwim
  :straight t
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; [beginend] Better M-< M-> for programming
(use-package beginend
  :straight t
  :hook ( . beginend-global-mode))

;; [expand-region] Select a region quickly
(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

;; [smart-region]
;; Call multiple times at the same position, it expands selected region (er/expand-region)
;; Else, if mark and call, it select the region rectangular (rectangle-mark-mode)
;; Else, if mark and call at same column as mark, it add cursor to each line (mc/edit-lines)
(use-package smart-region
  :straight t
  :hook (after-init . smart-region-on)
  :bind ("C-+" . smart-region))

;; TODO: [multiple-cursors] Multi-cursor
(use-package multiple-cursors
  :straight t
  :bind (("s-S-c s-S-c"   . mc/edit-lines)
         ("s->"           . mc/mark-next-like-this)
         ("s-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("s-M->"         . mc/skip-to-next-like-this)
         ("s-M-<"         . mc/skip-to-previous-like-this)
         ("s-S-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (setq mc/cmds-to-run-for-all '(mwim-beginning-of-code-or-line
                                 mwim-end-of-code-or-line)))

;; [hungry-delete] Hungry deletion
;; ISSUE: Hungry delete doesn't work with paredit
;; See: https://emacs.stackexchange.com/questions/33734/how-to-get-hungry-delete-working-in-paredit-mode
(setq backward-delete-char-untabify-method 'all)

;; [subword] Handling capitalized subwords
(use-package subword
  :hook (((prog-mode minibuffer-setup) . subword-mode)))

;; [paredit] Better paren editing
(use-package paredit
  :straight t
  :hook ((prog-mode . paredit-mode))
  :bind (:map paredit-mode-map
              ("M-<up>" . nil)
              ("M-<down>" . nil)
              (";" . nil)
              ("DEL" . +paredit-backward-delete))
  :config
  ;; Don't insert space automatically
  (add-to-list 'paredit-space-for-delimiter-predicates
               '(lambda (endp delim)
                  (derived-mode-p 'lisp-mode
                                  'lisp-interaction-mode
                                  'emacs-lisp-mode)))
  ;; Delete region
  (defun +paredit-backward-delete ()
    "Make paredit delete region correctly"
    (interactive)
    (if (use-region-p)
        (paredit-delete-region (region-beginning) (region-end))
      (paredit-backward-delete))
    )
  )

;; [zoom] Managing the window sizes automatically
(use-package zoom
  :straight t
  :bind ("C-x `" . zoom-mode))

;; [hideshow] Code folding
(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :bind (("C-c `" . hs-toggle-hiding)
         ("C-c <tab>" . hs-cycle)
         ("C-c ~" . hs-toggle-all))
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
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (propertize
                    (format " ... [%d lines] "
                            (count-lines (overlay-start ov)
                                         (overlay-end ov)))
                    'face '(:inherit shadow :height 0.8)))))
  (setq hs-set-up-overlay #'hs-display-code-line-counts))

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
                (car)))))))

;; [bookmark] Bookmarks for files and directories
(use-package bookmark
  :config
  (with-no-warnings
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
      (tabulated-list-init-header))))

;; [easy-kill] Kill & Mark things easily, extends functionality of M-w
(use-package easy-kill
  :straight t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(provide 'init-edit)
