;;; -*- lexical-binding: t -*-

(defadvice! +window-rotate-stacked-after-maximize-a (&rest _)
  :after #'toggle-frame-maximized
  (let* ((frame (selected-frame))
         (wins (window-list frame 'no-minibuf)))
    (when (and (eq (frame-parameter frame 'fullscreen) 'maximized)
               (= (length wins) 2)
               (window-combined-p (car wins) nil))
      (with-selected-frame frame
        (window-layout-rotate-anticlockwise (frame-root-window frame))))))

;; [ace-window] Add number for each window
(use-package ace-window
  :straight t
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
  (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
  ;; (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :hook ((window-configuration-change . aw-update)) ;; For modeline
  ;; (add-hook 'after-make-frame-functions #'aw--after-make-frame t)
  :config
  (setq aw-scope 'global
        aw-background nil
        aw-ignore-current t)

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

  (dotimes (n 9)
    (bind-key (format "C-%d" (1+ n))
              (lambda ()
                (interactive)
                (+aw--select-window (1+ n))))))


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
          ;; "\\*Embark Actions\\*"
          "\\*Finder\\*"
          ;; `ibuffer' displays the buffer before enabling `ibuffer-mode',
          ;; so the first display has to match by name.
          "^\\*Ibuffer\\*$"
          "\\*Kill Ring\\*"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          ibuffer-mode
          help-mode
          tabulated-list-mode
          Buffer-menu-mode
          flymake-diagnostics-buffer-mode

          ;; grep-mode occur-mode rg-mode
          osx-dictionary-mode

          "^\\*Process List\\*" process-menu-mode

          ;; `+eshell-toggle' displays this buffer before `eshell-mode'
          ;; is active, so the first display has to match by name.
          "^Eshell-popup: .*$" eshell-mode
          "^\\*shell.*\\*.*$"  shell-mode
          "^\\*terminal.*\\*.*$" term-mode
          "^\\*eldoc.*\\*.*$" eldoc-mode

          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$"
          "\\*gud-debug\\*$"
          "\\*quickrun\\*$"
          "\\*vc-.*\\*$"
          "^\\*macro expansion\\**"
          reb-mode

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Graphviz Preview: .*\\*"

          gptel-mode
          ghostel-mode

          (lambda (buffer)
            (with-current-buffer buffer
              (and (derived-mode-p 'compilation-mode)
                   (not (derived-mode-p 'grep-mode)))))
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

  (defun +popper-smart-popup (buffer &optional alist)
    (let ((window (display-buffer-in-direction buffer
                                               (append alist '((direction . below)
                                                               (window-height . 0.5))))))
      (unless (cl-progv +popper-unpacked-vars
                  (mapcar #'symbol-value +popper-unpacked-vars-no-select)
                (popper-popup-p buffer))
        (select-window window))))
  (setq popper-display-function #'+popper-smart-popup)
  )


;; [zoom] Managing the window sizes automatically
(use-package zoom
  :straight t
  :hook (window-setup . zoom-mode)
  :config
  (setq zoom-minibuffer-preserve-layout nil
        zoom-ignored-major-modes '(ediff-mode vundo-mode minibuffer-mode speedbar-mode))

  (defun +zoom-fix-window-size-h ()
    (setq-local window-size-fixed t))

  (defun +zoom-fix-window-width-h ()
    (setq-local window-size-fixed 'width))

  (defun +zoom-fix-window-height-h ()
    (setq-local window-size-fixed 'height))

  (add-hook 'vundo-mode-hook #'+zoom-fix-window-size-h)
  (add-hook 'speedbar-mode-hook #'+zoom-fix-window-width-h)
  (add-hook 'ediff-mode-hook #'+zoom-fix-window-height-h))

;; [auto-dim-other-buffers] Dim non-active buffers
(use-package auto-dim-other-buffers
  :straight t
  :hook ((after-init . auto-dim-other-buffers-mode))
  :config
  (setq auto-dim-other-buffers-dim-on-focus-out nil
        auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)

  ;; `adob--rescan-windows' does not honor this option.
  (defadvice! +auto-dim-other-buffers-respect-minibuffer-option-a (fn)
    :around #'adob--rescan-windows
    (when (or auto-dim-other-buffers-dim-on-switch-to-minibuffer
              (not (window-minibuffer-p)))
      (funcall fn)))

  (add-hook! (auto-dim-other-buffers-mode-hook enable-theme-functions server-after-make-frame-hook) :unless-daemonp-call-immediately
    (defun +auto-dim-other-buffers-auto-set-face (&rest _)
      (let ((dim (or (face-background 'mode-line)
                     'unspecified)))
        (set-face-background 'auto-dim-other-buffers-face dim)
        (set-face-attribute 'auto-dim-other-buffers-hide nil
                            :foreground dim
                            :background dim))))
  )
