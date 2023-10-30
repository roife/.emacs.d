;;; -*- lexical-binding: t -*-

(setq-default
 ;; no client startup messages
 server-client-instructions nil

 ;; Files
 ;; [lockfile]
 create-lockfiles nil
 ;; [backup] Use auto-save, which maintains a copy when a buffer is unsaved
 make-backup-files nil
 ;; [version-control]
 ;; In case I enable it later
 ;; version-control t
 ;; backup-by-copying t
 ;; delete-old-versions t
 ;; kept-new-versions 5
 ;; tramp-backup-directory-alist backup-directory-alist
 ;; [auto-save]
 auto-save-default t
 auto-save-include-big-deletions t ; Don't auto-disable auto-save after deleting big chunks.
 auto-save-list-file-prefix (expand-file-name "autosaves/" user-emacs-directory)
 auto-save-file-name-transforms (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                            ;; Prefix tramp autosaves to prevent conflicts with local ones
                                            (concat auto-save-list-file-prefix "tramp-\\2") t)
                                      (list ".*" auto-save-list-file-prefix t))

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
 read-process-output-max (* 3 1024 1024)

 ;; [Wrapping] words at whitespace, but do not wrap by default
 ;; Wrap words at whitespace, rather than in the middle of a word.
 word-wrap t
 ;; don't do any wrapping by default since it's expensive
 truncate-lines t
 truncate-partial-width-windows nil
 ;; better wrapping for cjk
 word-wrap-by-category t

 ;; Custom file path
 custom-file (expand-file-name "custom.el" user-emacs-directory)

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
 truncate-string-ellipsis "..."

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
(setq-default default-input-method nil)
(setq system-time-locale "C")

;; History
;;; [save-place-mode] save place lastly visited
(use-package saveplace
  :hook (after-init . save-place-mode)
  :config
  ;; HACK: `save-place-alist-to-file' uses `pp' to prettify the contents of its
  ;; cache, which is expensive and useless. replace it with `prin1'
  (+advice-pp-to-prin1! 'save-place-alist-to-file)
  )


;;; [recentf] recently visited files
(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-auto-cleanup 'never
    recentf-max-saved-items 200
    recentf-exclude (list "\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
		  "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
		  "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
		  (lambda (file) (file-in-directory-p file package-user-dir))
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
  (setq savehist-additional-variables '(mark-ring global-mark-ring
			  search-ring
			  regexp-search-ring
			  kill-ring)
    savehist-autosave-interval 300)

  (with-eval-after-load 'vertico
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

  ;; HACK: Remove text properties from variables to reduce savehist cache size.
  (add-hook! savehist-save-hook
    (defun +savehist--remove-string-properties-h ()
      (setq kill-ring (mapcar #'substring-no-properties
                              (cl-remove-if-not #'stringp kill-ring))
            register-alist (cl-loop for (reg . item) in register-alist
                                    if (stringp item)
                                    collect (cons reg (substring-no-properties item))
                                    else collect (cons reg item)))))
  )


;; [so-long] Workaround for long one-line file
(use-package so-long
  :hook ((after-init . global-so-long-mode)
	 ((so-long-mode prog-mode fundamental-mode) . +so-long-settings))
  :config
  ;; improve long line performance
  (defun +so-long-settings ()
    (setq bidi-display-reordering nil))

  ;; Saveplace should not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  )


;; [visual-line-mode] Soft line-wrapping
(use-package visual-line-mode
  :defer nil
  :hook (text-mode . visual-line-mode))


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

(pixel-scroll-precision-mode)

(defvar +scrolling-lines 5)
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


;; [gcmh] Optimize GC
(use-package gcmh
  :straight t
  :hook (emacs-startup . gcmh-mode)
  :config
  (setq gcmh-idle-delay 'auto
    gcmh-auto-idle-delay-factor 10
    gcmh-high-cons-threshold #x64000000)
  )


;; [tramp] Edit file remotely
(use-package tramp
  :config
  (setq tramp-default-method "ssh"
    tramp-auto-save-directory (expand-file-name "tramp-autosaves/" user-emacs-directory)
    tramp-backup-directory-alist backup-directory-alist
    remote-file-name-inhibit-cache 60)
  )


;; [minibuffer]
(use-package minibuffer
  :config
  (setq minibuffer-depth-indicate-mode t
    minibuffer-default-prompt-format " [%s]" ; shorten " (default %s)" => " [%s]"
    minibuffer-electric-default-mode t
    minibuffer-follows-selected-frame nil ; One frame one minibuffer.
    )
  )


;; [comint] Command interpreter
(use-package comint
  :config
  (setq comint-prompt-read-only t
    comint-buffer-maximum-size 2048

    ;; No paging, `eshell' and `shell' will honoring.
    comint-pager "cat"

    ;; better history search
    comint-history-isearch 'dwim))
