;;; -*- lexical-binding: t -*-

;; [eshell] Emacs command shell
(use-package esh-mode
  :defines eshell-prompt-function
  :functions eshell/alias
  :hook ((eshell-mode . compilation-shell-minor-mode))
  :bind (("C-`" . +eshell-toggle)
         ("C-·" . +eshell-toggle)
         :map eshell-mode-map
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
                                  (format "GPTel-popup: %s" root-name)
                                (format "Eshell-popup: %s" root-name)))
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

  ;; [ebc]
  (defun eshell/ebc (&rest args)
    "Compile a file (ARGS) in Emacs. Use `compile' to do background make."
    (if (eshell-interactive-output-p)
        (let ((compilation-process-setup-function
               (list 'lambda nil
                     (list 'setq 'process-environment
                           (list 'quote (eshell-copy-environment))))))
          (compile (eshell-flatten-and-stringify args))
          (pop-to-buffer next-error-last-buffer))
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
                 ;; avoid renaming buffer name like Eshell-popup: ...
                 (not (string-match-p "^Eshell-popup: " (buffer-name))))
        (rename-buffer (concat "Esh: " (abbreviate-file-name default-directory)) t))))
  )


;; [esh-syntax-highlighting] Fish-like syntax highlighting
(use-package eshell-syntax-highlighting
  :straight t
  :after eshell
  :hook (eshell-mode . eshell-syntax-highlighting-mode))


;; [eshell-z] `cd' to frequent directory in `eshell'
(use-package eshell-z
  :straight t
  :after eshell
  :commands (eshell/z))


(use-package esh-autosuggest
  :straight t
  :hook (eshell-mode . esh-autosuggest-mode))


(use-package esh-help
  :straight t
  :preface
  (defun +eshell-setup-esh-help-eldoc ()
    "Use `esh-help' as the Eldoc backend in Eshell."
    (require 'esh-help)
    (setq-local eldoc-documentation-function #'esh-help-eldoc-command))
  :hook ((eshell-mode . +eshell-setup-esh-help-eldoc)
         (eshell-mode . eldoc-mode))
  :config
  (defadvice! +eshell-esh-help-eldoc-man-minibuffer-string-a (cmd)
    :override #'esh-help-eldoc-man-minibuffer-string
    (if-let* ((cache-result (gethash cmd esh-help-man-cache)))
        (unless (eql 'none cache-result)
          cache-result)
      (let ((str (split-string (esh-help-man-string cmd) "\n")))
        (if (equal (concat "No manual entry for " cmd) (car str))
            (ignore (puthash cmd 'none esh-help-man-cache))
          (puthash
           cmd (when-let* ((str (seq-drop-while (lambda (s) (not (string-match-p "^SYNOPSIS$" s))) str))
                           (str (nth 1 str)))
                 (substring str (string-match-p "[^\s\t]" str)))
           esh-help-man-cache))))))


;; [esh-tldr] Browse local tldr pages
(use-package esh-tldr
  :load-path "~/code/tldr.el"
  :bind ("C-h t" . esh-tldr-dwim)
  :hook ((shell-mode eshell-mode comint-mode) . esh-tldr-capf-setup)
  :config
  (setq esh-tldr-use-tempel t))


(use-package eshell-did-you-mean
  :straight t
  :after esh-mode
  :init (eshell-did-you-mean-setup)
  ;; HACK: `pcomplete-completions' returns a function, but
  ;;   `eshell-did-you-mean--get-all-commands' unconditionally expects it to
  ;;   return a list of strings, causing wrong-type-arg errors in many cases.
  ;;   `all-completions' handles all these cases.
  (defadvice! +eshell--fix-eshell-did-you-mean-a (&rest _)
    :override #'eshell-did-you-mean--get-all-commands
    (unless eshell-did-you-mean--all-commands
      (setq eshell-did-you-mean--all-commands
            (all-completions "" (pcomplete-completions))))))


(use-package ghostel
  :straight t
  :preface
  (defun +ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))

  :bind (("C-x m" . ghostel)
         :map ghostel-semi-char-mode-map
         ("C-k"  . +ghostel-send-C-k-and-kill)
         ;; ;; I'm used to go up/down the shell history with M-n/p from eshell
         ;; ;; Simulate this behavior in ghostel by sending C-p and C-n
         ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
         ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl")))
         :map project-prefix-map
         ("m" . ghostel-project)
         ("M" . ghostel-project-list-buffers))
  :config
  (setq ghostel-enable-osc52 t)

  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer)))


(use-package ghostel-eshell
  :straight nil
  :hook (eshell-load . ghostel-eshell-visual-command-mode))

(use-package ghostel-compile
  :straight nil
  :hook (after-init . ghostel-compile-global-mode))

(use-package ghostel-comint
  :straight nil
  :hook (after-init . ghostel-comint-global-mode))
