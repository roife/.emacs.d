;;; -*- lexical-binding: t -*-

(setq-default
 ;; major mode for scratch
 initial-major-mode 'fundamental-mode
 ;; no start messages
 inhibit-startup-message t
 ;; no welcome screen
 inhibit-startup-screen t
 ;; no client startup messages
 server-client-instructions nil
 ;; no startup messages
 initial-scratch-message nil
 ;; don't create lockfiles
 create-lockfiles nil
 ;; backup setups and version control
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
 ;; default tab width to 4(instead of 8);  some major modes will override this
 tab-width 4
 c-basic-offset 4
 ;; indent with whitespace by default
 indent-tabs-mode nil
 tab-always-indent 'complete
 ;; comment over empty lines
 comment-empty-lines t
 ;; improve long line display performance
 bidi-inhibit-bpa t
 bidi-paragraph-direction 'left-to-right
 ;; allow resize by pixels
 frame-resize-pixelwise t
 x-gtk-resize-child-frames nil
 x-underline-at-descent-line t
 ;; larger process output buffer
 read-process-output-max (* 1024 1024)
 ;; Wrapping words at whitespace, but do not wrap by default
 word-wrap t
 truncate-lines t
 truncate-partial-width-windows nil
 ;; Don't display comp warnings
 warning-suppress-log-types '((comp))
 ;; Custom file path
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 ;; always follow link
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
 auto-hscroll-mode 'current-line
 ;; set fill column indicator to 80
 fill-column 80
 ;; Keep 5 lines when scrolling
 scroll-margin 3
 scroll-conservatively 101
 ;; Moving as visual lines
 line-move-visual t
 ;; Show path if names are same
 uniquify-buffer-name-style 'post-forward-angle-brackets
 ;; Set sentence end
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil
 ;; Use y-or-n to replace yes-or-no
 use-short-answers t
 ;; require newline
 require-final-newline t
 )


;; Encoding
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setq system-time-locale "C")


;; History
;;; [save-place-mode] save place lastly visited
(use-package save-place
  :hook (after-init . save-place-mode))


;;; [recentf] recently visited files
(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 300
        recentf-exclude '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                          "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                          (lambda (file) (file-in-directory-p file package-user-dir)))
        recentf-keep nil)
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))


;;; [savehist] Save variables to file
(use-package savehist
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffer
              savehist-additional-variables '(mark-ring global-mark-ring
                                              search-ring regexp-search-ring
                                              kill-ring)
              savehist-autosave-interval 300))


;; Workaround for long one-line file
(use-package so-long
  :hook (after-init . global-so-long-mode))


;; [visual-line-mode] Soft line-wrapping
(use-package visual-line-mode
  :hook (text-mode . visual-line-mode))


;; [gcmh] Optimize GC
(use-package gcmh
  :straight t
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x64000000)) ;; 100 MB

(provide 'init-basic)
