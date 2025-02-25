;;; -*- lexical-binding: t -*-

;; [ace-window] Add number for each window
(use-package ace-window
  :straight t
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
  (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
  :hook ((window-configuration-change . aw-update)) ;; For modeline
  :config
  (setq aw-scope 'global
        aw-background nil
        aw-ignore-current t)

  ;; Select widnow via `M-1'...`M-9'
  (defun +aw--select-window (number)
    "Select the specified window."
    (let* ((window-list (aw-window-list))
           (target-window nil))
      (cl-loop for win in window-list
               when (and (window-live-p win)
                         (eq number
                             (string-to-number
                              (window-parameter win 'ace-window-path))))
               do (setq target-window win)
               finally return target-window)

      ;; Select the target window if found
      (if target-window
          (aw-switch-to-window target-window)
        (message "No specified window: %d" number))))

  (defmacro +define-avy-switch-to-window (i)
    (let ((fn-name (intern (concat "+avy-switch-to-window-" (number-to-string i)))))
      `(progn
         (defun ,fn-name ()
           (interactive)
           (+aw--select-window ,i))
         (bind-key (concat "M-" (number-to-string ,i))
                   #',fn-name)))
    )

  ;; Select window via `M-1'...`M-9'
  (cl-loop for i from 1 to 9
           do (eval `(+define-avy-switch-to-window ,i)))
  )


;; [winner] Restore old window configurations
(use-package winner
  :commands (winner-undo winner-redo)
  :init
  (setq winner-dont-bind-my-keys t)
  :hook (after-init . winner-mode)
  :config
  (setq winner-boring-buffers
        '("*Completions*" "*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
          "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
          "*esh command on file*"))
  )


;; [popper] Enforce rules for popup windows like *Help*
(use-package popper
  :straight t
  :bind (:map popper-mode-map
              ("C-M-<tab>"   . popper-cycle)
              ("M-`" . popper-toggle-type))
  :hook (emacs-startup . popper-mode)
  :init
  (setq +popper-reference-buffers-select
        '("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Calendar\\*"
          "\\*Embark Actions\\*"
          "\\*Finder\\*"
          "\\*Kill Ring\\*"
          "\\*Go-Translate\\*"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          ibuffer-mode
          help-mode
          tabulated-list-mode
          Buffer-menu-mode
          flymake-diagnostics-buffer-mode

          grep-mode occur-mode rg-mode
          osx-dictionary-mode

          "^\\*Process List\\*" process-menu-mode
          list-environment-mode cargo-process-mode

          "^\\*eshell.*\\*.*$" eshell-mode
          "^\\*shell.*\\*.*$"  shell-mode
          "^\\*terminal.*\\*.*$" term-mode
          "^\\*vterm.*\\*.*$"  vterm-mode
          "^\\*eldoc.*\\*.*$" eldoc-mode

          "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*quickrun\\*$"
          "\\*vc-.*\\*$"
          "^\\*macro expansion\\**"
          reb-mode

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Graphviz Preview: .*\\*"

          "^GPTel-popup: .*"

          (lambda (buffer) (with-current-buffer buffer (derived-mode-p 'compilation-mode)))
          ))
  (setq +popper-reference-buffer-no-select
        '("\\*Warnings\\*"))
  (setq popper-reference-buffers (append +popper-reference-buffers-select
                                         +popper-reference-buffer-no-select))
  :config
  ;; mode-line indicator
  (with-eval-after-load 'popper
    (setq popper-mode-line
          '(:eval `(:propertize " POP |"
                                face (:inherit ,(+mode-line-get-window-name-face)
                                               :inverse-video ,(mode-line-window-selected-p))))))

  ;; Enable indicator in minibuffer
  (popper-echo-mode 1)

  ;; HACK: close popper with `C-g'
  (defadvice! +popper-close-window-hack (&rest _)
    :before #'keyboard-quit
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))

  ;; HACK: do not select window in `+popper-reference-buffer-no-select'
  (defvar +popper-unpacked-vars '(popper--reference-names
                                  popper--reference-modes
                                  popper--reference-predicates
                                  popper--suppressed-names
                                  popper--suppressed-modes
                                  popper--suppressed-predicates))
  (defvar +popper-unpacked-vars-no-select '())

  (dolist (var +popper-unpacked-vars)
    (let ((var-name (intern (concat "+" (symbol-name var) "-no-select"))))
      (eval
       `(progn
          (defvar ,var-name nil)
          (push ',var-name +popper-unpacked-vars-no-select)))))
  (setq +popper-unpacked-vars-no-select (reverse +popper-unpacked-vars-no-select))

  (cl-progv `(popper-reference-buffers ,@+popper-unpacked-vars)
      (list +popper-reference-buffer-no-select)
    (popper--set-reference-vars)
    (cl-loop for var in +popper-unpacked-vars
             for var-no-select in +popper-unpacked-vars-no-select
             do (eval `(setq ,var-no-select ',(symbol-value var))))
    )

  (defun +popper-smart-popup-at-bottom (buffer &optional alist)
    (let ((window (popper-display-popup-at-bottom buffer alist)))
      (unless (cl-progv +popper-unpacked-vars
                  (mapcar #'symbol-value +popper-unpacked-vars-no-select)
                (popper-popup-p buffer))
        (select-window window))))
  (setq popper-display-function #'+popper-smart-popup-at-bottom)
  )


;; [zoom] Managing the window sizes automatically
(use-package zoom
  :straight t
  :hook (window-setup . zoom-mode))


;; [auto-dim-other-buffers] Dim non-active buffers
(use-package auto-dim-other-buffers
  :straight t
  :hook ((after-init . auto-dim-other-buffers-mode)
         (auto-dim-other-buffers-mode . +auto-dim-other-buffers-auto-set-face))
  :config
  (setq auto-dim-other-buffers-dim-on-focus-out nil
        auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)

  (defadvice! +auto-dim-other-buffers-auto-set-face (&rest _)
    :after #'enable-theme
    (set-face-background 'auto-dim-other-buffers-face (face-background 'mode-line)))
  )

;; [transient for window operations]
;; (transient-define-prefix +frame-window-management ()
;;   "Frame Window Management"
;;   :transient-suffix     'transient--do-stay
;;   ;; :transient-non-suffix 'transient--do-warn
;;   [["Actions"
;;     ("TAB" "switch" other-window)
;;     ("d" "delete" ace-delete-window)
;;     ("D" "delete other" ace-delete-other-windows)
;;     ("s" "swap" ace-swap-window)
;;     ("a" "select" ace-select-window)]
;;    ["Resize"
;;     (""  "←" shrink-window-horizontally)
;;     ("j" "→" enlarge-window)
;;     ("k" "→" shrink-window)
;;     ("l" "→" enlarge-window-horizontally)
;;     ("n" "balance" balance-windows)]
;;    ["Split"
;;     ("" "right" split-window-right)
;;     ("" "below" split-window-below)]
;;    ["Winner"]]
;;   [["Popper"
;;     ]
;;    ["Move"
;;     ]
;;    ["Font"
;;     ("C-=" "larger" text-scale-increase)
;;     ("C--" "smaller" text-scale-decrease)
;;     ("C-0" "reset" (lambda () (interactive) (text-scale-increase 0)))]
;;    ["Zoom"
;;     ("z" "zoom-mode" zoom-mode)
;;     ("Z" "zoom" zoom)]])
