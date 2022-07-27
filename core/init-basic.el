;;; -*- lexical-binding: t -*-

(setq-default
 ;; major mode for scratch
 initial-major-mode 'fundamental-mode
 ;; no start messages
 inhibit-startup-message t
 ;; no welcome screen
 inhibit-splash-screen t
 inhibit-startup-screen t
 ;; no client startup messages
 server-client-instructions nil
 ;; no startup messages
 initial-scratch-message nil
 hl-line-sticky-flag t
 ;; don't create lockfiles
 create-lockfiles nil
 ;; add final newline
 require-final-newline t
 ;; backup setups and version control
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 backup-by-copying t
 delete-old-versions t
 version-control t
 kept-new-versions 6
 kept-old-versions 2
 ;; xref no prompt
 xref-prompt-for-identifier nil
 ;; this fix the cursor movement lag
 auto-window-vscroll t
 ;; window divider setup
 window-divider-default-right-width 1
 window-divider-default-bottom-width 1
 ;; don't wait for keystrokes display
 echo-keystrokes 0.01
 show-paren-style 'parenthese
 ;; overline no margin
 overline-margin 0
 ;; underline no margin
 underline-minimum-offset 0
 ;; default tab width to 4(instead of 8)
 ;; some major modes will override this
 tab-width 4
 c-basic-offset 4
 ;; indent with whitespace by default
 indent-tabs-mode nil
 ;; don't show cursor in non selected window.
 cursor-in-non-selected-windows t
 comment-empty-lines t
 visible-cursor nil
 ;; improve long line display performance
 bidi-inhibit-bpa t
 bidi-paragraph-direction 'left-to-right
 ;; allow resize by pixels
 frame-resize-pixelwise t
 x-gtk-resize-child-frames nil
 x-underline-at-descent-line t
 ;; larger process output buffer
 read-process-output-max (* 1024 1024)
 ;; Don't truncate lines in a window narrower than 65 chars.
 truncate-partial-width-windows 65
 ;; Default line number width.
 display-line-numbers-width 3
 ;; Don't display comp warnings
 warning-suppress-log-types '((comp))
 ;; Firefox as default browser
 ;; browse-url-browser-function 'browse-url-firefox
 ;; Custom file path
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 ;; prefer y or n
 y-or-n-p-use-read-key t
 ;; always follow link
 vc-follow-symlinks t
 ;; disable visual line move
 line-move-visual t
 ;; case insensitive completion
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 ;; pinentry
 epa-pinentry-mode 'loopback
 ;; disable input method in pgtk
 pgtk-use-im-context-on-new-connection nil
 ;; disable bell completely
 ring-bell-function 'ignore
 ;; eldoc idle delay
 eldoc-idle-delay 1
 ;; disable copy region blink
 copy-region-blink-delay 0
 ;; hscroll only for current line
 auto-hscroll-mode 'current-line
 ;; set fill column indicator to 80
 display-fill-column-indicator-column t
 fill-column 80
 ;; max mini window height, 15%
 max-mini-window-height 0.15
 ;; Keep 10 lines when scrolling
 scroll-preserve-screen-position t
 scroll-margin 5
 scroll-conservatively 101
 ;; Moving as visual lines
 line-move-visual t
 ;; Keep the cursor at EOL when moving at EOL
 track-eol t
 ;; Show path if names are same
 uniquify-buffer-name-style 'post-forward-angle-brackets
 ;; Set sentence end
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil
 ;; Use y-or-n to replace yes-or-no
 use-short-answers t
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
  :init (setq recentf-max-saved-items 300
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

;;; [savehist] Save variables to file
(use-package savehist
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history
                                              kill-ring)
              savehist-autosave-interval 300))

;; Workaround for long one-line file
(use-package so-long
  :hook (after-init . global-so-long-mode))

;; [gcmh] Garbage Collector Magic Hack
(use-package gcmh
  :straight t
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 10
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x6400000)) ; 16MB

(provide 'init-basic)
