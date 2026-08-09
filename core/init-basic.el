;;; -*- lexical-binding: t -*-

(use-package no-littering
  :straight (:host github :repo "emacscollective/no-littering")
  :demand t
  :config
  ;; `no-littering' intentionally leaves `custom-file' to the user.
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")

        ;; Package-specific paths not covered by no-littering.
        tramp-rpc-deploy-local-cache-directory (no-littering-expand-var-file-name "tramp-rpc/")
        forge-post-fallback-directory (no-littering-expand-var-file-name "forge/drafts/")
        rust-playground-basedir (no-littering-expand-var-file-name "rust-playground/")
        typst-ts-lsp-download-path (no-littering-expand-var-file-name "lsp/tinymist/tinymist")
        chirp-cache-directory (no-littering-expand-var-file-name "chirp/")
        chirp-compose-temporary-directory (no-littering-expand-var-file-name "chirp/compose/")
        ghostel-module-directory (no-littering-expand-var-file-name "ghostel/"))

  ;; The existing configuration enables backups and auto-save.  This moves
  ;; them under `var/' as well.
  (no-littering-theme-backups)

  (dolist (directory
           (list tramp-rpc-deploy-local-cache-directory
                 forge-post-fallback-directory
                 rust-playground-basedir
                 chirp-cache-directory
                 chirp-compose-temporary-directory
                 ghostel-module-directory
                 (file-name-directory typst-ts-lsp-download-path)))
    (make-directory directory t)))

(setq-default
 ;; no client startup messages
 server-client-instructions nil

 ;; Files
 ;; [lockfile]
 create-lockfiles nil
 ;; [backup]
 vc-make-backup-files t
 version-control t
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 ;; [auto-save]
 auto-save-default t
 auto-save-include-big-deletions t ; Don't auto-disable auto-save after deleting big chunks.

 ;; Disable [bidirectional text] scanning for a modest performance
 ;; Will improve long line display performance
 bidi-inhibit-bpa t
 bidi-paragraph-direction 'left-to-right
 bidi-display-reordering 'left-to-right

 ;; smaller threshold to improve long line performance
 long-line-threshold 1000
 large-hscroll-threshold 1000
 syntax-wholeline-max 1000

 ;; Larger process output buffer for LSP module
 read-process-output-max (* 4 1024 1024)

 ;; [Wrapping] words at whitespace, but do not wrap by default
 ;; Wrap words at whitespace, rather than in the middle of a word.
 word-wrap t
 ;; don't do any wrapping by default since it's expensive
 truncate-lines t
 truncate-partial-width-windows nil
 ;; better wrapping for cjk
 word-wrap-by-category t

 ;; Always follow link when visiting a [symbolic link]
 find-file-visit-truename t
 vc-follow-symlinks t

 ;; Case insensitive completion
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t

 ;; disable [bell] completely
 ring-bell-function 'ignore

 ;; Disable copy region blink
 copy-region-blink-delay 0
 delete-pair-blink-delay 0

 ;; set [fill column] indicator to 80
 fill-column 80

 ;; [tab]
 ;; Make `tabify' only affect indentation
 tabify-regexp "^\t* [ \t]+"
 ;; Indent with 4 space by default
 indent-tabs-mode nil
 ;; Indent first, otherwise run completion-at-point. This lets Tempel
 ;; templates expand via TAB when indentation does not apply.
 tab-always-indent t
 tab-width 4

 ;; Sentence end
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil

 ;; Use y-or-n to replace yes-or-no
 use-short-answers t
 ;; Inhibit switching out from `y-or-n-p' and `read-char-choice'
 y-or-n-p-use-read-key t
 read-char-choice-use-read-key t

 ;; Don't ping things that look like domain names.
 ffap-machine-p-known 'reject

 ;; Disable the "same file" warning, just redirect to the existing buffer
 find-file-suppress-same-file-warnings t

 ;; POSIX standard [newline]
 require-final-newline t

 ;; Don't prompt for confirmation when creating a new file or buffer
 confirm-nonexistent-file-or-buffer nil

 ;; Show path/name if names are same
 uniquify-buffer-name-style 'forward

 ;; Fix alignment problem
 truncate-string-ellipsis "⁝"

 ;; Shell command
 shell-command-prompt-show-cwd t

 ;; What-cursor-position
 what-cursor-show-names t

 ;; List only applicable commands
 read-extended-command-predicate #'command-completion-default-include-p
 )

;; Enable [disabled cmds]
;; Enable the disabled narrow commands
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Enable the disabled dired commands
(put 'dired-find-alternate-file 'disabled nil)

;; Enable the disabled `list-timers', `list-threads' commands
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)

;; Quick editing in `describe-variable'
(with-eval-after-load 'help-fns
  (put 'help-fns-edit-variable 'disabled nil))


;; [autosave]
;; TRICK: If a file has autosaved data, `after-find-file' will pause for 1 second to
;; tell about it, which is very annoying. Just disable it.
(defadvice! +disable-autosave-notification-a (fn &rest args)
  :around #'after-find-file
  (cl-letf (((symbol-function #'sit-for) #'ignore))
    (apply fn args)))


;; Encoding & locale
(set-locale-environment "en_US.UTF-8")
(prefer-coding-system 'utf-8-unix)
(setq-default default-input-method nil)
(setq system-time-locale "C")

(add-hook! (tty-setup-hook server-after-make-frame-hook)
  (defun +setup-tty-coding-system (&optional frame)
    "Use UTF-8 for keyboard input and terminal output in TTY frames."
    (let ((frame (or frame (selected-frame))))
      (unless (display-graphic-p frame)
        (set-keyboard-coding-system 'utf-8-unix frame)
        (set-terminal-coding-system 'utf-8-unix frame t)))))


;; [gcmh] Run GC when Emacs is idle, not while commands are active.
;; (use-package gcmh
;;   :straight t
;;   :unless (fboundp 'igc-info)
;;   :hook (emacs-startup . gcmh-mode)
;;   :config
;;   (setq gcmh-idle-delay 'auto
;;         gcmh-auto-idle-delay-factor 10
;;         gcmh-high-cons-threshold (* 128 1024 1024)
;;         gcmh-low-cons-threshold +gc-cons-threshold))


;; History
;;; [save-place-mode] save place lastly visited
(use-package saveplace
  :hook (after-init . save-place-mode)
  :config
  (setq save-place-autosave-interval 1000)

  ;; HACK: `save-place-alist-to-file' uses `pp' to prettify the contents of its
  ;; cache, which is expensive and useless. replace it with `prin1'
  (+advice-pp-to-prin1! 'save-place-alist-to-file)

  ;; recenters the view after the jump
  (defadvice! +save-place-recenter-a (&rest _)
    :after #'save-place-find-file-hook
    (when buffer-file-name (ignore-errors (recenter))))
  )


;;; [recentf] recently visited files
(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-autosave-interval 1000)

  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 200
        recentf-exclude (list "\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                              "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                              "^/tmp/" "^/var/folders/.+$" "^/ssh:"
                              (lambda (file) (file-in-directory-p file package-user-dir))
                              (recentf-expand-file-name no-littering-var-directory)
                              (recentf-expand-file-name no-littering-etc-directory)
                              (expand-file-name recentf-save-file))
        recentf-keep nil)

  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)

  ;; HACK: Text properties inflate the size of recentf's files, and there is
  ;; no purpose in persisting them (Must be first in the list!)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  ;; Add dired directories to recentf file list.
  (add-hook! dired-mode-hook
    (defun +dired--add-to-recentf-h ()
      (recentf-add-file default-directory)))
  )


;;; [savehist] Save variables to file
(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-additional-variables '(kill-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        dogears-list)
        savehist-autosave-interval 1000)

  (with-eval-after-load 'vertico
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

  (defvar dogears-list nil)
  (defun +dogears--record-for-savehist (record)
    (cons (substring-no-properties (car record))
          (mapcar
           (lambda (item)
             (pcase item
               (`(buffer . ,buffer)
                (if (bufferp buffer)
                    (cons 'buffer (buffer-name buffer))
                  item))
               (_ item)))
           (cdr record))))

  (defadvice! +savehist-clean-values-a (save &rest args)
    :around #'savehist-save
    (let ((kill-ring (mapcar #'substring-no-properties kill-ring))
          (search-ring (mapcar #'substring-no-properties search-ring))
          (regexp-search-ring (mapcar #'substring-no-properties regexp-search-ring))
          (dogears-list (mapcar #'+dogears--record-for-savehist dogears-list)))
      (apply save args))))


;; [so-long] Workaround for long one-line file
(use-package so-long
  :hook ((after-init . global-so-long-mode))
  :config
  (dolist (mode '(conf-mode text-mode))
    (add-to-list 'so-long-target-modes mode))

  (dolist (mode '(font-lock-mode
                  eldoc-mode
                  flymake-mode
                  ws-butler-mode
                  auto-composition-mode))
    (add-to-list 'so-long-minor-modes mode))

  (dolist (override '((bidi-display-reordering . nil)
                      (font-lock-maximum-decoration . 1)
                      (save-place-alist . nil)))
    (add-to-list 'so-long-variable-overrides override))
  )


;; [glyphless-display] Don't render glyphs, in case of undisplayable characters.
(use-package glyphless-mode
  :hook (after-init . glyphless-display-mode))

;; [Scrolling]
(setq
 ;; Performant and rapid scrolling
 fast-but-imprecise-scrolling t

 ;; Keep 5 lines when scrolling
 scroll-step 0
 scroll-margin 3
 scroll-up-aggressively 0.01 ; less jumpy
 scroll-down-aggressively 0.01
 scroll-conservatively 101
 ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll' for tall lines.
 auto-window-vscroll nil

 ;; [hscroll]
 auto-hscroll-mode t
 hscroll-step 0
 hscroll-margin 2)

(defvar +scrolling-lines 10)
(defun +scroll-other-window () (interactive) (scroll-other-window +scrolling-lines))
(defun +scroll-other-window-down () (interactive) (scroll-other-window-down +scrolling-lines))
(defun +scroll-window () (interactive) (scroll-up +scrolling-lines))
(defun +scroll-window-down () (interactive) (scroll-down +scrolling-lines))
(bind-keys*
 ("C-M-v" . +scroll-other-window)
 ("M-<down>" . +scroll-other-window)

 ("C-M-S-v" . +scroll-other-window-down)
 ("M-<up>" . +scroll-other-window-down)

 ("C-v" . +scroll-window-down)
 ("M-v" . +scroll-window))


;; [tramp] Edit file remotely
(use-package tramp
  :config
  (setq tramp-default-method "ssh"
        tramp-backup-directory-alist backup-directory-alist
        remote-file-name-inhibit-cache 60))


(use-package tramp-rpc
  :straight (:type git
                   :host github
                   :repo "ArthurHeymans/emacs-tramp-rpc")
  :after tramp
  :config
  (setq tramp-rpc-deploy-auto-deploy t
        tramp-rpc-deploy-git-build-policy 'release))


;; [minibuffer]
(use-package minibuffer
  :hook (after-init . minibuffer-electric-default-mode)
  :config
  (setq minibuffer-default-prompt-format " [%s]" ; shorten " (default %s)" => " [%s]"
        ;; One frame one minibuffer.
        minibuffer-follows-selected-frame nil))


;; [comint] Command interpreter
(use-package comint
  :config
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 2048

        ;; No paging, `eshell' and `shell' will honoring.
        comint-pager "cat"

        ;; better history search
        comint-history-isearch 'dwim))


;; [environment variables]
(use-package exec-path-from-shell
  :straight t
  :unless (or noninteractive (daemonp) (not (display-graphic-p)))
  :hook (after-init . exec-path-from-shell-initialize)
  :init
  (setq exec-path-from-shell-arguments '("-l")
        exec-path-from-shell-variables '("PATH" "HOMEBREW" "JAVA_HOME" "JDTLS_JAVA_HOME" "MANPATH")))


;; [backup walker] A utility to view Emacs backup files.
(use-package backup-walker
  :straight t)


;; [posframe]
(use-package posframe :straight t)


;; [project] Project manager
(use-package project
  :straight (:type built-in)
  :bind (:map project-prefix-map
              ("m" . magit-status))
  :config
  (setq project-switch-commands '((project-find-file "File")
                                  (project-find-regexp "Regexp")
                                  (project-switch-to-buffer "Buffer")
                                  (project-dired "Dired")
                                  (project-eshell "Eshell")
                                  (project-search "Search")
                                  (magit-status "Magit"))))
