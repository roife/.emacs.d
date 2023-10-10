;;; -*- lexical-binding: t -*-

;; [hl-line] Highlight current line
(use-package hl-line
  :hook (after-init . global-hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight EOF
  (setq hl-line-range-function (lambda ()
                               (cons
                                (line-end-position)
                                (line-beginning-position 2)))))


;; [show-paren-mode] Highlight matching parens
(use-package paren
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
  :config
  (add-function :after after-focus-change-function #'diff-hl-update-once)
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
          (define-fringe-bitmap '+diff-hl-bmp
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
  :hook (after-init . goggles-mode)
  :config
  (setq-default goggles-pulse nil)
  )


;; [beacon] Highlight line at cursor after switching window
(use-package pulse
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region :background unspecified))))
  (pulse-highlight-face ((t (:inherit region :background unspecified :extend t))))
  :hook (((dumb-jump-after-jump imenu-after-jump) . +recenter-and-pulse)
         ((bookmark-after-jump magit-diff-visit-file next-error) . +recenter-and-pulse-line))
  :init
  (defun +pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun +pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (+pulse-momentary-line)))

  (defun +recenter-and-pulse(&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (+pulse-momentary))

  (defun +recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (+pulse-momentary-line))

  (dolist (cmd '(recenter-top-bottom
                 other-window switch-to-buffer
                 aw-select toggle-window-split
                 windmove-do-window-select
                 pager-page-down pager-page-up
                 treemacs-select-window))
    (advice-add cmd :after #'+pulse-momentary-line))

  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 goto-last-change))
    (advice-add cmd :after #'+recenter-and-pulse))

  (dolist (cmd '(symbol-overlay-basic-jump))
    (advice-add cmd :after #'+recenter-and-pulse-line))
  )


;; [highlight-indent-guides] Highlight indentions
;; (use-package highlight-indent-guides
;;   :straight t
;;   :functions (macrostep-collapse macrostep-expand)
;;   :hook ((prog-mode yaml-mode) . (lambda () (unless (> (car (buffer-line-statistics)) 3000) (highlight-indent-guides-mode 1))))
;;   :config
;;   (setq highlight-indent-guides-method 'character
;;               highlight-indent-guides-responsive 'top
;;               highlight-indent-guides-suppress-auto-error t
;;               highlight-indent-guides-delay 0.3)
;;
;;   ;; Don't display first level of indentation
;;   (defun +indent-guides-for-all-but-first-column (level responsive display)
;;     (unless (< level 1)
;;       (highlight-indent-guides--highlighter-default level responsive display)))
;;   (setq highlight-indent-guides-highlighter-function
;;         #'+indent-guides-for-all-but-first-column)
;;
;;
;;   ;; HACK: `highlight-indent-guides' acalculates its faces from the current theme,
;;   ;; but is unable to do so properly in terminal Emacs
;;   (defun +highligh-indent-guides-auto-set-faces (&rest _)
;;     (when (display-graphic-p)
;;             (highlight-indent-guides-auto-set-faces)))
;;   (if (daemonp)
;;       (add-hook 'server-after-make-frame-hook
;;                 #'+highligh-indent-guides-auto-set-faces)
;;     (advice-add #'enable-theme :after #'+highligh-indent-guides-auto-set-faces))
;;   )


;; [indent-bars] Highlight indentions effectively
(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  :config
  (setq indent-bars-display-on-blank-lines nil
        indent-bars-width-frac 0.2
        indent-bars-color '(highlight :face-bg t :blend 0.2)
        indent-bars-zigzag nil
        indent-bars-highlight-current-depth nil
        indent-bars-pattern ".")

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
