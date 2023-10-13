;;; -*- lexical-binding: t -*-

;; [eshell] Emacs command shell
(use-package esh-mode
  :defines eshell-prompt-function
  :functions eshell/alias
  :hook ((eshell-mode . compilation-shell-minor-mode))
  :bind (("C-`" . +eshell-toggle)
         :map eshell-mode-map
              ("C-l" . eshell/clear)
              ("M-s" . consult-history))
  :config
  (setq
   ;; banner
   eshell-banner-message
   '(concat (propertize (concat " " (buffer-name) " ") 'face 'mode-line-highlight)
           " "
           (propertize (current-time-string) 'face 'font-lock-keyword-face)
           "\n")
   ;; scrolling
   eshell-scroll-to-bottom-on-input 'all
   eshell-scroll-to-bottom-on-output 'all

   ;; exit
   eshell-kill-processes-on-exit t
   eshell-hist-ignoredups t

   eshell-input-filter #'eshell-input-filter-initial-space

   ;; em-glob
   eshell-glob-case-insensitive t
   eshell-error-if-no-glob t

   ;; prefer eshell functions
   eshell-prefer-lisp-functions t

   ;; Visual commands require a proper terminal. Eshell can't handle that
   eshell-visual-commands '("top" "htop" "less" "more" "bat" "talnet")
   eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show"))

   ;; Completion like bash
   eshell-cmpl-ignore-case t
   eshell-cmpl-cycle-completions nil
   )

  (defun +eshell-toggle ()
    "Toggle a persistent eshell popup window.
If popup is visible but unselected, select it.
If popup is focused, kill it."
    (interactive)
    (require 'eshell)
    (if-let ((win (get-buffer-window "*eshell-popup*")))
        (if (eq (selected-window) win)
            ;; If users attempt to delete the sole ordinary window. silence it.
            (ignore-errors (delete-window win))
          (select-window win))
      (let ((display-comint-buffer-action '(display-buffer-at-bottom
                                            (inhibit-same-window . nil)))
            (eshell-buffer-name "*eshell-popup*"))
        (with-current-buffer (eshell)
          (add-hook 'eshell-exit-hook #'(lambda () (ignore-errors (delete-window win))) nil t)))))

  ;; [UI]
  (add-hook 'eshell-mode-hook
            (lambda ()
              (set-window-fringes nil 0 0)
              (set-window-margins nil 1 nil)
              (visual-line-mode +1)
              (set-display-table-slot standard-display-table 0 ?\ )))


  (defun +eshell/define-alias ()
    "Define alias for eshell"
    ;; Aliases
    (defalias 'eshell-f 'find-file)
    (defalias 'eshell-fo 'find-file-other-window)
    (defalias 'eshell-d 'dired)
    (eshell/alias "l" "ls -lah $*")
    (eshell/alias "ll" "ls -laG $*")
    (defalias 'eshell-q 'eshell/exit)
    (eshell/alias "rg" "rg --color=always $*")
    (defalias 'eshell-clear 'eshell/clear-scrollback)
    ;; Vim
    (defalias 'eshell-vim 'find-file)
    (defalias 'eshell-vi 'find-file)
    ;; Git
    (eshell/alias "git" "git $*")
    (eshell/alias "gst" "git status $*")
    (eshell/alias "ga" "git add $*")
    (eshell/alias "gc" "git commit $*")
    (eshell/alias "gp" "git push $*")
    (eshell/alias "gb" "git branch $*")
    (eshell/alias "gch" "git checkout $*")
    (eshell/alias "gcb" "git checkout -b $*")
    )
  (add-hook 'eshell-first-time-mode-hook #'+eshell/define-alias)
  ;; Don't auto-write our aliases! Let us manage our own `eshell-aliases-file' via elisp
  (advice-add #'eshell-write-aliases-list :override #'ignore)

  ;;; A bunch of eshell functions
  ;; [clear]
  (defun eshell/clear ()
    "Clear the eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  ;; [emacs, e, ec, ecc]
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
  (defalias 'eshell/ecc #'eshell/emacs)

  ;; [ebc]
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

  ;; [less, more]
  (defun eshell/less (&rest args)
    "Invoke `view-file' on a file (ARGS).
\"less +42 foo\" will go to line 42 in the buffer for foo."
    (while args
      (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
          (let* ((line (string-to-number (match-string 1 (pop args))))
                 (file (pop args)))
            (+eshell-view-file file)
            (forward-line line))
        (+eshell-view-file (pop args)))))
  (defalias 'eshell/more #'eshell/less)

  ;; [bat]
  (defun eshell/bat (file)
    "cat FILE with syntax highlight."
    (with-temp-buffer
      (insert-file-contents file)
      (let ((buffer-file-name file))
        (delay-mode-hooks
          (set-auto-mode)
          (font-lock-ensure)))
      (buffer-string)))

  ;; [bd]
  (defun eshell/bd ()
    "cd to parent directory with completions."
    (let ((dir default-directory)
          dirs)
      (while (not (string-empty-p dir))
        (push (file-name-directory dir) dirs)
        (setq dir (substring dir 0 -1)))
      (let ((dir (completing-read "Directory: " dirs nil t)))
        (eshell/cd dir))))


  ;; view file
  (defun +eshell-view-file (file)
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

  ;; Sync buffer name
  (add-hook! (eshell-directory-change-hook eshell-mode-hook)
             (defun +eshell-sync-dir-buffer-name ()
               "Change eshell buffer name by directory change."
               (when (and (equal major-mode 'eshell-mode)
                          ;; avoid renaming buffer name when in `eshell-popup'
                          (not (string-equal (buffer-name) "*eshell-popup*")))
                 (rename-buffer (concat "Esh: " (abbreviate-file-name default-directory)) t))))
  )


;; [esh-syntax-highlighting] Fish-like syntax highlighting
(use-package eshell-syntax-highlighting
  :straight t
  :after eshell
  :hook (eshell-mode . eshell-syntax-highlighting-mode))


;; [esh-help] `eldoc' support
(use-package esh-help
  :straight t
  :after eshell
  :hook (eshell-mode . esh-help-eldoc-setup))


;; [eshell-z] `cd' to frequent directory in `eshell'
(use-package eshell-z
  :straight t
  :after eshell
  :commands (eshell/z))


;; [eshell-up] Quickly navigating to a specific parent directory in eshell
(use-package eshell-up
  :straight t
  :after eshell
  :commands (eshell-up eshell-up-peek)
  :config
  (defalias 'eshell-up 'eshell-up)
  (defalias 'eshell-pk 'eshell-up-peek))


;; [esh-autosuggest]
(use-package esh-autosuggest
  :straight t
  :hook (eshell-mode . esh-autosuggest-mode)
  :bind (:map esh-autosuggest-active-map
              ("C-e" . 'company-complete-selection)))
