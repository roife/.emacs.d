;;; -*- lexical-binding: t -*-

;; [hl-line] Highlight current line
(use-package hl-line
  :hook ((prog-mode text-mode
                    yaml-mode conf-mode
                    special-mode org-agenda-mode dired-mode) . hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil))


;; [show-paren-mode] Highlight matching parens
(use-package paren
  :hook ((prog-mode conf-mode yaml-mode) . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  )


;; [rainbow-delimiters] Highlight brackets according to their depth
(use-package rainbow-delimiters
  :straight t
  :hook ((prog-mode conf-mode yaml-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5)
  )


;; [rainbow-mode] Colorize color names in buffers
(use-package rainbow-mode
  :straight t
  :functions (rainbow-turn-off rainbow-colorize-match rainbow-x-color-luminance)
  :bind (:map help-mode-map
              ("w" . rainbow-mode))
  :hook ((html-mode css-mode) . rainbow-mode)
  :config
  ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
  ;; @see https://emacs.stackexchange.com/questions/36420
  (defun +rainbow-colorize-match (color &optional match)
    (let* ((match (or match 0))
           (ov (make-overlay (match-beginning match) (match-end match))))
      (overlay-put ov 'ov-rainbow t)
      (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                "white"
                                              "black"))
                              (:background ,color)))))
  (advice-add #'rainbow-colorize-match :override #'+rainbow-colorize-match)

  ;; Clear overlays when exit
  (defun +rainbow-clear-overlays ()
    "Clear all rainbow overlays."
    (remove-overlays (point-min) (point-max) 'ov-rainbow t))
  (advice-add #'rainbow-turn-off :after #'+rainbow-clear-overlays)
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

  (dolist (keyword '("BUG" "DEFECT" "ISSUE" "FIXME"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces))
  )


;; [diff-hl] Highlight uncommitted changes using VC
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
            (vector (if (eq system-type 'darwin) #b11100000 #b11111100))
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


;; [highlight-indent-guides] Highlight indentions
(use-package highlight-indent-guides
  :straight t
  :functions (macrostep-collapse macrostep-expand)
  :hook ((prog-mode yaml-mode) . (lambda () (unless (> (car (buffer-line-statistics)) 3000) (highlight-indent-guides-mode 1))))
  :config
  (setq highlight-indent-guides-method 'character
              highlight-indent-guides-responsive 'top
              highlight-indent-guides-suppress-auto-error t
              highlight-indent-guides-delay 0.3)

  ;; Don't display first level of indentation
  (defun +indent-guides-for-all-but-first-column (level responsive display)
    (unless (< level 1)
      (highlight-indent-guides--highlighter-default level responsive display)))
  (setq highlight-indent-guides-highlighter-function
        #'+indent-guides-for-all-but-first-column)

  ;; Disable in `macrostep' expanding
  (with-eval-after-load 'macrostep
    (advice-add #'macrostep-expand
                :after (lambda (&rest _)
                         (when highlight-indent-guides-mode
                           (highlight-indent-guides-mode -1))))
    (advice-add #'macrostep-collapse
                :after (lambda (&rest _)
                         (when (derived-mode-p 'prog-mode 'yaml-mode)
                           (highlight-indent-guides-mode 1)))))

  ;; HACK: `highlight-indent-guides' acalculates its faces from the current theme,
  ;; but is unable to do so properly in terminal Emacs
  (defun +highligh-indent-guides-auto-set-faces ()
    (when (display-graphic-p)
            (highlight-indent-guides-auto-set-faces)))
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook
                #'+highligh-indent-guides-auto-set-faces)
    (advice-add #'enable-theme :after #'+highligh-indent-guides-auto-set-faces))
  )


;; [symbol-overlay] Highlight symbols
(use-package symbol-overlay
  :straight t
  :bind (("C-c s i" . symbol-overlay-put)
         ("C-c s n" . symbol-overlay-switch-forward)
         ("C-c s p" . symbol-overlay-switch-backward)
         ("C-c s c" . symbol-overlay-remove-all))
  :hook (((prog-mode yaml-mode) . symbol-overlay-mode))
  )
