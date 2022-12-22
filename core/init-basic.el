;;; -*- lexical-binding: t -*-

(defvar +cache-dir (expand-file-name "emacs/" "~/.cache"))

(setq-default
 ;; no client startup messages
 server-client-instructions nil

 ;; Files
 ;; [lockfile]
 create-lockfiles nil
 ;; [backup] Use auto-save, which maintains a copy when a buffer is unsaved
 make-backup-files nil
 ;; In case I enable it later
 ;; backup-directory-alist `((".*" . ,(concat +cache-dir "backup/")))
 ;;; version-control t
 ;;; backup-by-copying t
 ;;; delete-old-versions t
 ;;; kept-new-versions 5
 ;;; tramp-backup-directory-alist backup-directory-alist
 ;; [auto-save]
 auto-save-default t
 auto-save-include-big-deletions t ; Don't auto-disable auto-save after deleting big chunks.
 auto-save-file-name-transforms `((".*" ,(concat +cache-dir "autosave/") t))
 auto-save-file-name-transforms (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                            ;; Prefix tramp autosaves to prevent conflicts with local ones
                                            (concat auto-save-list-file-prefix "tramp-\\2") t)
                                      (list ".*" auto-save-list-file-prefix t))

 ;; Disable [bidirectional text] scanning for a modest performance
 ;; Will improve long line display performance
 bidi-inhibit-bpa t
 bidi-paragraph-direction 'left-to-right
 bidi-display-reordering 'left-to-right

 ;; Larger process output buffer for LSP module
 read-process-output-max (* 1024 1024)

 ;; [Wrapping] words at whitespace, but do not wrap by default
 ;; Wrapp words at whitespace, rather than in the middle of a word.
 word-wrap t
 ;; don't do any wrapping by default since it's expensive
 truncate-lines t
 truncate-partial-width-windows nil

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

 ;; set [fill column] indicator to 80
 fill-column 80

 ;; [tab]
 ;; Make `tabify' only affect indentation
 tabify-regexp "^\t* [ \t]+"
 ;; Indent with 4 space by default
 indent-tabs-mode nil
 tab-always-indent t
 tab-width 4
 c-basic-offset 4

 ;; [sentence end]
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil

 ;; Use y-or-n to replace yes-or-no
 use-short-answers t

 ;; Don't ping things that look like domain names.
 ffap-machine-p-known 'reject

 ;; Disable the "same file" warning, just redirect to the existing buffer
 find-file-suppress-same-file-warnings t

 ;; POSIX standard [newline]
 require-final-newline t

 ;;Don't prompt for confirmation when creating a new file or buffer
 confirm-nonexistent-file-or-buffer nil

 ;; Show path/name if names are same
 uniquify-buffer-name-style 'forward
 )


;; [autosave]
;; If a file has autosaved data, `after-find-file' will pause for 1 second to
;; tell about it, which is very annoying. Just disable it.
(advice-add #'after-find-file :around
            (lambda (fn &rest args) (cl-letf (((symbol-function #'sit-for) #'ignore))
                      (apply fn args))))


;; Encoding
(set-language-environment "UTF-8")
(setq-default default-input-method nil)


;; History
;;; [save-place-mode] save place lastly visited
(use-package saveplace
  :hook (after-init . save-place-mode)
  :config
  ;; `save-place-alist-to-file' uses `pp' to prettify the contents of its cache, which is expensive and useless.
  ;; replace it with `prin1'
  (advice-add #'save-place-alist-to-file :around
              (lambda (fn) (cl-letf (((symbol-function #'pp) #'prin1))
                        (funcall fn))))
  )


;;; [recentf] recently visited files
(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 200
        recentf-exclude '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                          "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                          (lambda (file) (file-in-directory-p file package-user-dir)))
        recentf-keep nil)
  (push (expand-file-name recentf-save-file) recentf-exclude)

  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)

  ;; Text properties inflate the size of recentf's files, and there is
  ;; no purpose in persisting them (Must be first in the list!)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  ;; Add dired directories to recentf file list.
  (add-hook 'dired-mode-hook (lambda () (recentf-add-file default-directory)))
  )


;;; [savehist] Save variables to file
(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-additional-variables '(mark-ring global-mark-ring
                                                  search-ring regexp-search-ring
                                                  kill-ring)
        savehist-autosave-interval 300)

  ;; Remove text properties from `kill-ring' to reduce savehist cache size.
  (add-hook 'savehist-save-hook
            (lambda () (setq kill-ring
                        (mapcar #'substring-no-properties
                                (cl-remove-if-not #'stringp kill-ring))
                        register-alist
                        (cl-loop for (reg . item) in register-alist
                                 if (stringp item)
                                 collect (cons reg (substring-no-properties item))
                                 else collect (cons reg item)))))
  )


;; Workaround for long one-line file
(use-package so-long
  :hook (after-init . global-so-long-mode)
  :config
  ;; Saveplace should not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  )


;; [visual-line-mode] Soft line-wrapping
(use-package visual-line-mode
  :defer nil
  :hook (text-mode . visual-line-mode))


;; [gcmh] Optimize GC
(use-package gcmh
  :straight t
  :hook (emacs-startup . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold #x64000000)
  )


;; [tramp]
(use-package tramp
  :config
  (setq tramp-default-method "ssh"
        tramp-persistency-file-name (concat +cache-dir "tramp-persist")
        tramp-auto-save-directory (concat +cache-dir "tramp-autosave/")
        tramp-backup-directory-alist backup-directory-alist
        remote-file-name-inhibit-cache 60)
  )
