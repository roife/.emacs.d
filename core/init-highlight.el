;;; -*- lexical-binding: t -*-

;; [hl-line] Highlight current line
(use-package hl-line
  :hook ((prog-mode text-mode
                    yaml-mode conf-mode
                    special-mode org-agenda-mode dired-mode) . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight EOF
  (setq hl-line-range-function (lambda ()
                               (cons
                                (line-end-position)
                                (line-beginning-position 2)))))


;; [show-paren-mode] Highlight matching parens
(use-package paren
  ;; after emacs 27, show-paren-mode is enabled by default
  ;; :hook ((prog-mode conf-mode yaml-mode) . show-paren-mode)
  :custom-face (show-paren-match ((t (:underline t))))
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen t)
  )


;; [whitespace] Show visualize TAB, (HARD) SPC, newline
(use-package whitespace
  :hook ((prog-mode conf-mode yaml-mode) . whitespace-mode)
  :init
  :config
  (setq
   ;; only show bad whitespace
   whitespace-style '(face trailing empty indentation space-before-tab space-after-tab)))


;; [rainbow-delimiters] Highlight brackets according to their depth
(use-package rainbow-delimiters
  :straight t
  :hook ((prog-mode conf-mode yaml-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5)
  )


;; [highlight-parentheses] Highlight surrounding parentheses
(use-package highlight-parentheses
  :straight t
  :hook ((minibuffer-setup . highlight-parentheses-minibuffer-setup)
         (prog-mode . highlight-parentheses-mode))
  :config
  (setq highlight-parentheses-colors '("SpringGreen3" "IndianRed1" "IndianRed3" "IndianRed4")
        highlight-parentheses-attributes '((:underline t :weight bold)
                                           (:underline t :weight bold)
                                           (:underline t :weight bold))
        highlight-parentheses-delay 0.2))


;; [rainbow-mode] Colorize color names in buffers
(use-package rainbow-mode
  :straight t
  :functions (rainbow-turn-off rainbow-colorize-match rainbow-x-color-luminance)
  :bind (:map help-mode-map
              ("r" . rainbow-mode))
  :hook ((html-mode css-mode) . rainbow-mode)
  :config
  ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
  ;; @see https://emacs.stackexchange.com/questions/36420
  )


;; [hl-todo] Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :straight t
  :custom-face
  (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :inverse-video t))))
  :hook ((prog-mode conf-mode yaml-mode) . hl-todo-mode)
  :config
  (setq hl-todo-require-punctuation t
        hl-todo-highlight-punctuation ":")

  (defun +hl-todo-add-keywords (keys color)
    (dolist (keyword keys)
      (if-let ((item (assoc keyword hl-todo-keyword-faces)))
          (setf (cdr item) color)
        (push `(,keyword . ,color) hl-todo-keyword-faces))))

  ;; HACK: `hl-todo' won't update face when changing theme, so we must add a hook for it
  (defun +hl-update-keyword-faces (&rest _)
    (+hl-todo-add-keywords '("BUG" "DEFECT" "ISSUE") (face-foreground 'error))
    (+hl-todo-add-keywords '("WORKAROUND" "HACK" "TRICK") (face-foreground 'warning)))
  (+hl-update-keyword-faces)
  (advice-add #'enable-theme :after #'+hl-update-keyword-faces)
  )


;; ;; [diff-hl] Highlight uncommitted changes using VC
(use-package diff-hl
  :straight t
  :defines desktop-minor-mode-table
  :hook ((find-file    . diff-hl-mode)
         (vc-dir-mode  . diff-hl-dir-mode)
         (dired-mode   . diff-hl-dired-mode))
  ; Fall back to the display margin since the fringe is unavailable in tty
  :hook ((diff-hl-mode diff-hl-dir-mode diff-hl-dired-mode) .
         (lambda ()
           (diff-hl-update-once)
           (unless (display-graphic-p) (diff-hl-margin-local-mode 1))))
  :hook ((focus-in . diff-hl-update-once))
  :config
  (setq
   diff-hl-draw-borders nil
   ;; Reduce load on remote
   diff-hl-disable-on-remote t
   ;; A slightly faster algorithm for diffing
   vc-git-diff-switches '("--histogram"))

  ;; The gutter looks less cramped with some space between it and  buffer.
  (setq-default fringes-outside-margins t)

  ;; Make fringes look better
  (setq diff-hl-fringe-bmp-function
        (lambda (&rest _)
          (define-fringe-bitmap 'my-diff-hl-bmp
            (vector #b00000000)
            1 8
            '(center t))))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  ;; Integration with flymake
  (with-eval-after-load 'flymake
    (setq flymake-fringe-indicator-position 'right-fringe))

  ;; WORKAROUND: Integration with ws-butler
  (advice-add #'ws-butler-after-save :after #'diff-hl-update-once)
  )


;; [goggles] Highlight modified region
(use-package goggles
  :straight t
  :hook ((prog-mode conf-mode yaml-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse nil)
  )


;; [indent-bars] Highlight indentions effectively
(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  :config
  (setq indent-bars-display-on-blank-lines nil
        indent-bars-width-frac 0.2)

  ;; HACK: `indent-bars' calculates its faces from the current theme,
  ;; but is unable to do so properly in terminal Emacs
  (defun +indent-bars-auto-set-faces (&rest _)
    (when indent-bars-mode
      (indent-bars-reset)))
  (advice-add #'enable-theme :after #'+indent-bars-auto-set-faces)
  )


;; [symbol-overlay] Highlight symbols
(use-package symbol-overlay
  :straight t
  :bind (("C-c s i" . symbol-overlay-put)
         ("C-c s n" . symbol-overlay-switch-forward)
         ("C-c s p" . symbol-overlay-switch-backward)
         ("C-c s c" . symbol-overlay-remove-all)
         :map symbol-overlay-map
         ;; conflits with `meow'
         ("h" . nil)
         ("q" . nil)
         ("R" . symbol-overlay-query-replace)
         ("?" . symbol-overlay-map-help)
         ("c" . symbol-overlay-put)
         ("C" . symbol-overlay-remove-all)
         ("i" . nil)
         )
  :hook (((prog-mode yaml-mode) . symbol-overlay-mode))
  )
