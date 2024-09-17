;;; -*- lexical-binding: t -*-

;; [eshell] Emacs command shell
(use-package esh-mode
  :defines eshell-prompt-function
  :functions eshell/alias
  :hook ((eshell-mode . compilation-shell-minor-mode))
  :bind (("C-`" . +eshell-toggle)
         ("C-·" . +eshell-toggle)
         :map eshell-mode-map
         ("C-l" . eshell/clear)
         ("M-s" . consult-history))
  :config
  (setq
   ;; banner
   eshell-banner-message ""

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


  (defun +eshell-toggle (&optional arg)
    "Toggle a persistent Eshell popup window for the current project or directory.
If the popup is visible but unselected, select it.
If the popup is focused, kill it.
If no project is found, create a temporary Eshell instance in the current directory."
    (interactive "P")
    (require 'eshell)
    (require 'project)  ;; Ensure we load project.el
    (let* ((project (project-current)) ;; Get the current project
           (dir-name (directory-file-name default-directory))
           (root-name (if project
                          (file-name-nondirectory (directory-file-name (project-root project))) ;; Use project name
                        (file-name-nondirectory dir-name))) ;; Use current directory name if no project
           (popup-buffer-name (if arg
                                  (format "*GPTel-popup*: %s" root-name)
                                (format "*Eshell-popup*: %s" root-name)))
           (win (get-buffer-window popup-buffer-name)))

      ;; If an argument is provided, you can add some custom behavior, like opening a GPT prompt.
      ;; If Eshell window exists, either focus or kill it.
      (if win
          (if (eq (selected-window) win)
              (ignore-errors (delete-window win)) ;; Close window if already focused
            (select-window win)) ;; Focus the Eshell window if it exists but not selected
        ;; If no Eshell window, create one
        (let ((display-comint-buffer-action '(display-buffer-at-bottom
                                              (inhibit-same-window . nil))))
          (if arg
              ;; Open a GPT prompt
              (with-current-buffer (gptel popup-buffer-name)
                ;; display current buffer
                (display-buffer (current-buffer)))
            ;; Open a eshell
            (let ((eshell-buffer-name popup-buffer-name))
              (with-current-buffer (eshell)
                (unless (string= dir-name (directory-file-name default-directory))
                  (eshell/cd dir-name)
                  (eshell-send-input))
                ;; Add a hook to close the window when Eshell exits
                (add-hook 'eshell-exit-hook
                          (lambda ()
                            (ignore-errors (delete-window (get-buffer-window popup-buffer-name))))
                          nil t))))))))

  ;; [UI]
  (add-hook 'eshell-mode-hook
            (defun +eshell--set-window-ui-h ()
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
      (erase-buffer)))

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
        (delay-mode-hooks (set-auto-mode) (font-lock-ensure)))
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
                 (eq (buffer-name) "*Eshell-pop*"))
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

  :hook (eshell-mode . setup-esh-help-eldoc))


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
  (setq eshell-up-ignore-case nil)
  )
