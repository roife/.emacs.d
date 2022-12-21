;;; -*- lexical-binding: t -*-

(setq-default
 ;; no client startup messages
 server-client-instructions nil

 ;; don't create [lockfile]s
 create-lockfiles nil

 ;; [backup and auto-save]
 backup-directory-alist `((".*" . ,temporary-file-directory))
 backup-by-copying t
 version-control t
 delete-old-versions t
 kept-new-versions 4
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 ;; xref no prompt
 xref-prompt-for-identifier nil
 ;; don't wait for keystrokes display
 echo-keystrokes .01
 ;; overline/underline no margin
 overline-margin 0
 underline-minimum-offset 0
 ;; comment over empty lines
 comment-empty-lines t
 ;; improve long line display performance
 bidi-inhibit-bpa t
 bidi-paragraph-direction 'left-to-right
 bidi-display-reordering 'left-to-right
 ;; allow resize by pixels
 frame-resize-pixelwise t
 x-gtk-resize-child-frames nil
 x-underline-at-descent-line t
 ;; larger process output buffer
 read-process-output-max (* 1024 1024)

 ;; [Wrapping] words at whitespace, but do not wrap by default
 ;; Wrapp words at whitespace, rather than in the middle of a word.
 word-wrap t
 ;; don't do any wrapping by default since it's expensive
 truncate-lines t
 truncate-partial-width-windows nil

 ;; Don't display comp warnings
 warning-suppress-log-types '((comp))
 ;; Custom file path
 custom-file (expand-file-name "custom.el" user-emacs-directory)

 ;; Always follow link when visiting a [symbolic link]
 find-file-visit-truename t
 vc-follow-symlinks t

 ;; case insensitive completion
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 ;; pinentry
 epa-pinentry-mode 'loopback
 ;; disable bell completely
 ring-bell-function 'ignore
 ;; disable copy region blink
 copy-region-blink-delay 0
 ;; hscroll only for current line
 auto-hscroll-mode 'current-lin

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

 ;; Keep 5 lines when scrolling
 scroll-margin 3
 scroll-conservatively 101
 ;; Moving as visual lines
 line-move-visual t
 ;; Show path if names are same
 uniquify-buffer-name-style 'post-forward-angle-brackets

 ;; [sentence end]
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil

 ;; Use y-or-n to replace yes-or-no
 use-short-answers t

 ;; Don't ping things that look like domain names.
 ffap-machine-p-known 'reject
 ;; Allow minibuffer commands while in the minibuffer.
 enable-recursive-minibuffers t

 ;; Disable the "same file" warning, just redirect to the existing buffer
 find-file-suppress-same-file-warnings t

 ;; POSIX standard [newline]
 require-final-newline t
)

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
  (let ((tramp-directory (expand-file-name "tramp" user-emacs-directory)))
    (setq tramp-default-method "ssh"
          tramp-persistency-file-name tramp-directory
          tramp-auto-save-directory tramp-directory
          tramp-backup-directory-alist backup-directory-alist
          remote-file-name-inhibit-cache 60))
  )


;; TODO: better-jump
;; TODO: dtrt-indent


(provide 'init-basic)
