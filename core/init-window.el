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
  :hook ((window-configuration-change . aw-update)) ;; For modeline
  :config
  (setq aw-scope 'global
        aw-background nil
        aw-ignore-current t)

  (defun +aw--select-window (number)
    "Select the window labeled NUMBER."
    (if-let* ((win (seq-find (lambda (win)
                               (equal (window-parameter win 'ace-window-path)
                                      (number-to-string number)))
                             (aw-window-list))))
        (aw-switch-to-window win)
      (message "No specified window: %d" number)))

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
          "*esh command on file*")))


;; [popper] Enforce rules for popup windows like *Help*
(use-package popper
  :straight t
  :bind (:map popper-mode-map
              ("C-M-<tab>"   . popper-cycle)
              ("M-`" . popper-toggle-type))
  :hook (emacs-startup . popper-mode)
  :init
  (defvar +popper-reference-buffers-no-select '("\\*Warnings\\*")
    "Popup buffers that should be displayed without being selected.")
  (setq popper-reference-buffers
        `("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Calendar\\*"
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
          ,@+popper-reference-buffers-no-select))
  :config
  ;; mode-line indicator
  (setq popper-mode-line
        '(:eval `(:propertize " POP |"
                              face (:inherit +mode-line-meta-face
                                             :inverse-video ,(mode-line-window-selected-p)))))

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

  ;; Display popups below, selecting them unless in `+popper-reference-buffers-no-select'.
  (defun +popper-smart-popup (buffer &optional alist)
    (let ((window (or (display-buffer-reuse-window buffer alist)
                      (display-buffer-in-direction
                       buffer
                       (append alist '((direction . below)
                                       (window-height . 0.5)))))))
      (unless (buffer-match-p `(or ,@+popper-reference-buffers-no-select) buffer)
        (select-window window))
      window))
  (setq popper-display-function #'+popper-smart-popup))


;; [zoom] Managing the window sizes automatically
(use-package zoom
  :straight t
  :init
  (add-hook! vundo-mode-hook
    (defun +zoom-fix-window-size-h ()
      (setq-local window-size-fixed t)))

  (add-hook! speedbar-mode-hook
    (defun +zoom-fix-window-width-h ()
      (setq-local window-size-fixed 'width)))

  (add-hook! ediff-mode-hook
    (defun +zoom-fix-window-height-h ()
      (setq-local window-size-fixed 'height)))
  :hook (window-setup . zoom-mode)
  :config
  (setq zoom-minibuffer-preserve-layout nil
        zoom-ignored-major-modes '(ediff-mode vundo-mode minibuffer-mode speedbar-mode)))
