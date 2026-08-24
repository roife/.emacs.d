;;; -*- lexical-binding: t -*-

;; [hl-line] Highlight current line
(use-package hl-line
  :hook (after-init . global-hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil
        ;; Highlight starts from EOL, to avoid conflicts with other overlays
        hl-line-range-function (lambda () (cons (line-end-position)
                                                (line-beginning-position 2)))))


;; [show-paren-mode] Highlight matching parens
(use-package paren
  :custom-face (show-paren-match ((t (:foreground "SpringGreen3" :underline t :weight bold))))
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen t
        blink-matching-paren-highlight-offscreen t
        show-paren-delay 0.2)
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
  (setq rainbow-delimiters-max-face-count 5))


;; [highlight-parentheses] Highlight surrounding parentheses
(use-package highlight-parentheses
  :straight t
  :hook ((minibuffer-setup . highlight-parentheses-minibuffer-setup)
         (prog-mode . highlight-parentheses-mode))
  :config
  (setq highlight-parentheses-colors '("green3" "firebrick1" "orange1")
        highlight-parentheses-attributes '((:underline t) (:underline t) (:underline t))
        highlight-parentheses-delay 0.25)
  )


;; [rainbow-mode] Colorize color names in buffers
(use-package rainbow-mode
  :straight t
  :functions (rainbow-turn-off rainbow-colorize-match rainbow-x-color-luminance)
  :bind (:map help-mode-map
              ("r" . rainbow-mode))
  :hook ((html-mode css-mode) . rainbow-mode)
  :config
  ;; removed HACK: Use overlay instead of text properties to override `hl-line' faces.
  ;; @see https://emacs.stackexchange.com/questions/36420
  )


;; [hl-todo] Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :straight t
  :custom-face
  (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :inverse-video t))))
  :hook (((prog-mode conf-mode yaml-mode) . hl-todo-mode))
  :config
  (setq hl-todo-require-punctuation t
        hl-todo-highlight-punctuation ":")

  (defun +hl-todo-add-keywords (keys color)
    (dolist (keyword keys)
      (if-let* ((item (assoc keyword hl-todo-keyword-faces)))
          (setf (cdr item) color)
        (push `(,keyword . ,color) hl-todo-keyword-faces))))

  ;; HACK: `hl-todo' won't update face when changing theme, so we must add a hook for it
  (add-hook! enable-theme-functions :unless-daemonp-call-immediately
    (defun +hl-update-keyword-faces (&rest _)
      (+hl-todo-add-keywords '("BUG" "DEFECT" "ISSUE") (face-foreground 'error))
      (+hl-todo-add-keywords '("WORKAROUND" "HACK" "TRICK") (face-foreground 'warning))))
  )


;; [goggles] Highlight modified region
(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse nil)
  )


;; [pulse] Highlight line at cursor after switching window
(use-package pulse
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region :background unspecified))))
  (pulse-highlight-face ((t (:inherit region :background unspecified :extend t))))
  :hook (((dumb-jump-after-jump imenu-after-jump) . +recenter-and-pulse)
         ((bookmark-after-jump magit-diff-visit-file next-error) . +recenter-and-pulse-line))
  :init
  (setq pulse-delay 0.2
        pulse-iterations 1)

  (defadvice! +pulse-momentary-line (&rest _)
    :after '(recenter-top-bottom
             other-window switch-to-buffer
             aw-select
             windmove-do-window-select
             pager-page-up
             tab-bar-select-tab
             +tab-bar-echo)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun +pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (xref-pulse-momentarily))

  (defadvice! +recenter-and-pulse (&rest _)
    :after '(pop-to-mark-command pop-global-mark)
    "Recenter and pulse the region or the current line."
    (recenter)
    (+pulse-momentary))

  (defadvice! +recenter-and-pulse-line (&rest _)
    :after '(symbol-overlay-basic-jump compile-goto-error)
    "Recenter and pulse the current line."
    (recenter)
    (+pulse-momentary-line))

  ;; Pulse only in current window
  (defadvice! +pulse-window-local-a (fn &rest args)
    :around #'pulse-momentary-highlight-region
    (let ((window (selected-window)))
      (prog1 (apply fn args)
        (when (overlayp pulse-momentary-overlay)
          (overlay-put pulse-momentary-overlay 'window window)))))
  )


;; [symbol-overlay] Highlight symbols
(use-package symbol-overlay
  :straight (:host github :repo "roife/symbol-overlay" :branch "master")
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
  :config
  (setq symbol-overlay-temp-highlight-on-region t)
  )


;; [highlight]
(use-package hilit-chg
  :preface
  (defun +highlight-changes-mode-turn-on ()
    (highlight-changes-mode 1)
    (highlight-changes-visible-mode -1))
  (defun +highlight-changes-mode-turn-off ()
    (and highlight-changes-mode (highlight-changes-mode -1)))
  (defun +highlight-changes-auto ()
    (when (buffer-file-name)
      (+highlight-changes-mode-turn-on)
      (add-hook! after-save-hook :local #'+highlight-changes-mode-turn-on)
      (add-hook! before-save-hook :local #'+highlight-changes-mode-turn-off)))
  :hook ((prog-mode conf-mode text-mode) . +highlight-changes-auto))
