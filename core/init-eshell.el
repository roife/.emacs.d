;;; -*- lexical-binding: t -*-

;; [eshell] Emacs command shell
(use-package esh-mode
  :defines eshell-prompt-function
  :functions eshell/alias
  :hook (eshell-mode . +eshell/define-alias)
  :config
  (bind-keys
   :map eshell-mode-map
   ("C-l" . eshell/clear)
   ("M-s" . consult-history))

  (setq
   eshell-banner-message
   '(format "%s %s\n"
            (propertize (format " %s " (string-trim (buffer-name)))
                        'face 'mode-line-highlight)
            (propertize (current-time-string)
                        'face 'font-lock-keyword-face))
   ;; scrolling
   eshell-scroll-to-bottom-on-input 'all
   eshell-scroll-to-bottom-on-output 'all

   eshell-kill-processes-on-exit t
   eshell-hist-ignoredups t

   eshell-input-filter #'eshell-input-filter-initial-space

   ;; em-glob
   eshell-glob-case-insensitive t
   eshell-error-if-no-glob t)

  ;; [UI]
  (add-hook 'eshell-mode-hook
            (lambda ()
              (set-window-fringes nil 0 0)
              (set-window-margins nil 1 nil)
              (visual-line-mode +1)
              (set-display-table-slot standard-display-table 0 ?\ )))

  (setq
   ;; Visual commands require a proper terminal. Eshell can't handle that
   eshell-visual-commands '("top" "htop" "less" "more" "bat" "vim"))

  (defun +eshell/define-alias ()
    "Define alias for eshell"
    ;; Aliases
    (eshell/alias "f" "find-file $1")
    (eshell/alias "fo" "find-file-other-window $1")
    (eshell/alias "d" "dired $1")
    (eshell/alias "l" "ls -lah $*")
    (eshell/alias "q" "exit")
    (eshell/alias "git" "git -P $*")
    (eshell/alias "rg" "rg --color=always $*")
    (eshell/alias "clear" "clear-scrollback"))

  (defun eshell/clear ()
    "Clear the eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (defun eshell/emacs (&rest args)
    "Open a file (ARGS) in Emacs."
    (if (null args)
        ;; If I just ran "emacs"
        (bury-buffer)
      ;; We have to expand the file names or else naming a directory in an
      ;; argument causes later arguments to be looked for in that directory,
      ;; not the starting directory
      (mapc #'find-file (mapcar #'expand-file-name (flatten-tree (reverse args))))))
  (defalias 'eshell/e #'eshell/emacs)
  (defalias 'eshell/ec #'eshell/emacs)

  (defun eshell/ebc (&rest args)
    "Compile a file (ARGS) in Emacs. Use `compile' to do background make."
    (if (eshell-interactive-output-p)
        (let ((compilation-process-setup-function
               (list 'lambda nil
                     (list 'setq 'process-environment
                           (list 'quote (eshell-copy-environment))))))
          (compile (eshell-flatten-and-stringify args))
          (pop-to-buffer compilation-last-buffer))
      (throw 'eshell-replace-command
             (let ((l (eshell-stringify-list (flatten-tree args))))
               (eshell-parse-command (car l) (cdr l))))))
  (put 'eshell/ebc 'eshell-no-numeric-conversions t)

  (defun eshell-view-file (file)
    "View FILE.  A version of `view-file' which properly rets the eshell prompt."
    (interactive "fView file: ")
    (unless (file-exists-p file) (error "%s does not exist" file))
    (let ((buffer (find-file-noselect file)))
      (if (eq (get (buffer-local-value 'major-mode buffer) 'mode-class)
              'special)
          (progn
            (switch-to-buffer buffer)
            (message "Not using View mode because the major mode is special"))
        (let ((undo-window (list (window-buffer) (window-start)
                                 (+ (window-point)
                                    (length (funcall eshell-prompt-function))))))
          (switch-to-buffer buffer)
          (view-mode-enter (cons (selected-window) (cons nil undo-window))
                           'kill-buffer)))))

  (defun eshell/less (&rest args)
    "Invoke `view-file' on a file (ARGS).
\"less +42 foo\" will go to line 42 in the buffer for foo."
    (while args
      (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
          (let* ((line (string-to-number (match-string 1 (pop args))))
                 (file (pop args)))
            (eshell-view-file file)
            (forward-line line))
        (eshell-view-file (pop args)))))
  (defalias 'eshell/more #'eshell/less)

  ;; Sync buffer name
  (defun eshell-sync-dir-buffer-name ()
    "Change eshell buffer name by directory change."
    (when (equal major-mode 'eshell-mode)
      (rename-buffer
       (format "Esh: %s" (abbreviate-file-name default-directory))
       t)))

  (add-hook 'eshell-directory-change-hook #'eshell-sync-dir-buffer-name)
  (add-hook 'eshell-mode-hook #'eshell-sync-dir-buffer-name)
  )


;; [esh-syntax-highlighting] Fish-like syntax highlighting
(use-package eshell-syntax-highlighting
  :straight t
  :after eshell
  :hook (eshell-mode . eshell-syntax-highlighting-mode)
  )


;; [esh-help] `eldoc' support
(use-package esh-help
  :straight t
  :after eshell
  :init
  (setup-esh-help-eldoc)
  )


;; [eshell-z] `cd' to frequent directory in `eshell'
(use-package eshell-z
  :straight t
  :commands (eshell/z)
  :after eshell
  )


;; [eshell-up] Quickly navigating to a specific parent directory in eshell
(use-package eshell-up
  :straight t
  :after eshell
  :commands (eshell-up eshell-up-peek)
  :config
  (eshell/alias "up" :"eshell-up")
  (eshell/alias "pk" "eshell-up-peek")
  )


;; [esh-autosuggest]
(use-package esh-autosuggest
  :straight t
  :hook (eshell-mode . esh-autosuggest-mode)
  )
