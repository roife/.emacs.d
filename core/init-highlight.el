;;; -*- lexical-binding: t -*-

;; [hl-line] Highlight current line
(use-package hl-line
  :hook (after-init . global-hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda ()
                               (cons
                                (line-end-position)
                                (line-beginning-position 2))))
  )


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
  ;; only show bad whitespace
  (setq whitespace-style
        '(face trailing empty indentation space-before-tab space-after-tab)))


;; [display-fill-column-indicator] Show a line at 80 char
(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode))


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
        highlight-parentheses-delay 0.2)
  )


;; [rainbow-mode] Colorize color names in buffers
(use-package rainbow-mode
  :straight t
  :functions (rainbow-turn-off rainbow-colorize-match rainbow-x-color-luminance)
  :bind (:map help-mode-map
              ("r" . rainbow-mode))
  :hook ((html-mode css-mode) . rainbow-mode)
  :config
  ;; removedHACK: Use overlay instead of text properties to override `hl-line' faces.
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
  (add-hook! '+theme-changed-hook :call-immediately
             (defun +hl-update-keyword-faces ()
               (+hl-todo-add-keywords '("BUG" "DEFECT" "ISSUE") (face-foreground 'error))
               (+hl-todo-add-keywords '("WORKAROUND" "HACK" "TRICK") (face-foreground 'warning))))
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
  (setq pulse-delay 0.02)

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
  (add-hook! '+theme-changed-hook
             (defun +indent-bars-auto-set-faces ()
               (when indent-bars-mode (indent-bars-reset))))
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
         ("i" . nil)
         ("R" . symbol-overlay-query-replace)
         ("?" . symbol-overlay-map-help)
         ("c" . symbol-overlay-put)
         ("C" . symbol-overlay-remove-all))
  :hook (((prog-mode yaml-mode) . symbol-overlay-mode))
  )
