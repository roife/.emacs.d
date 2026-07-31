;;; -*- lexical-binding: t -*-

;; [isearch] Use builtin isearch to replace `anzu'
(use-package isearch
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char))
  :config
  (setq
   ;; Record isearch in minibuffer history, so C-x ESC ESC can repeat it.
   isearch-resume-in-command-history t
   ;; One space can represent a sequence of whitespaces
   isearch-lax-whitespace t
   ;; direction change
   isearch-repeat-on-direction-change t
   ;; M-< and M-> move to the first/last occurrence of the current search string.
   isearch-allow-motion t
   isearch-motion-changes-direction t
   ;; lazy-count
   isearch-lazy-count t
   lazy-highlight-cleanup nil
   lazy-highlight-buffer t
   ;; search-ring
   search-ring-max 200
   regexp-search-ring-max 200))


;; [speedbar]
(use-package speedbar
  :init
  (setq speedbar-prefer-window t
        speedbar-window-default-width 30))


;; [goto-addr] Click to open URL
(use-package goto-addr
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))


;; [arxiv.el] Search, browse, and save arXiv papers
(use-package arxiv
  :straight (:type git :host github :repo "roife/arxiv.el")
  :config
  (setq arxiv-browser-function #'arxiv-eww-browse-url)
  (arxiv-url-handler-mode 1))


;; [avy] Jump with several key strock
(use-package avy
  :straight t
  :bind (("C-, ." . avy-goto-char)
         ("C-, ," . avy-goto-char-2)
         ("C-, l" . avy-goto-line)
         :map isearch-mode-map
         ("C-, ," . avy-isearch))
  :config
  ;; overlay is used during isearch, `pre' style makes avy keys evident.
  (setq avy-styles-alist '((avy-isearch . pre)))
  )


;; [avy-pinyin] Avy support for pinyin
(use-package ace-pinyin
  :straight t
  :after avy
  :init (ace-pinyin-global-mode t))


;; [link-hint] Open URL in text with avy
(use-package link-hint
  :straight t
  :bind
  ("C-, j" . link-hint-open-link)
  ("C-, c" . link-hint-copy-link))


;; [ialign] Interactive align
(use-package ialign
  :straight t)


;; [hideshow] Code folding
(use-package hideshow
  :preface
  (defun +hideshow-setup ()
    "Set up hideshow block definitions for modes that need overrides."
    (pcase major-mode
      ('ruby-mode
       (setq-local hs-block-start-regexp "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
                   hs-block-end-regexp "end\\|[]}]"
                   hs-c-start-regexp "#\\|=begin"
                   hs-forward-sexp-function #'ruby-forward-sexp))
      ('nxml-mode
       (setq-local hs-block-start-regexp "<!--\\|<[^/>]*[^/]>"
                   hs-block-end-regexp "-->\\|</[^/>]*[^/]>"
                   hs-c-start-regexp "<!--"
                   hs-forward-sexp-function #'sgml-skip-tag-forward))
      ((or 'latex-mode 'LaTeX-mode)
       (setq-local hs-block-start-regexp "\\\\begin{[a-zA-Z*]+}\\(\\)"
                   hs-block-start-mdata-select 1
                   hs-block-end-regexp "\\\\end{[a-zA-Z*]+}"
                   hs-c-start-regexp "%"
                   hs-forward-sexp-function
                   (lambda (_arg)
                     ;; LaTeX-find-matching-end needs to be inside the environment.
                     (unless (save-excursion
                               (search-backward "\\begin{document}"
                                                (line-beginning-position) t))
                       (LaTeX-find-matching-end)))))))
  :hook (((prog-mode conf-mode yaml-mode) . hs-minor-mode)
         ((ruby-mode nxml-mode latex-mode LaTeX-mode) . +hideshow-setup)
         ((yaml-mode) . hs-indentation-mode))
  :bind (("C-c h TAB" . hs-cycle)
         ("C-c h `" . hs-toggle-all))
  :config
  (setq hs-indicator-type nil
        hs-display-lines-hidden t))


;; [project] Project manager
(use-package project
  :bind (:map project-prefix-map
              ("m" . magit-status))
  :config
  (setq project-switch-commands '((project-find-file "File")
                                  (project-find-regexp "Regexp")
                                  (project-switch-to-buffer "Buffer")
                                  (project-dired "Dired")
                                  (project-eshell "Eshell")
                                  (project-search "Search")
                                  (magit-status "Magit")))

  )


;; [vundo] Undo tree
(use-package vundo
  :straight t
  :config
  (setq vundo-compact-display t
        vundo-roll-back-on-quit t))


;; [undohist] Persist undo history
(use-package undo-fu-session
  :straight t
  :hook (after-init . undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))

  (when (executable-find "zstd")
    ;; There are other algorithms available, but zstd is the fastest
    (setq undo-fu-session-compression 'zst)))


;; [undo-hl] Highlight undo changes
(use-package undo-hl
  :straight (:host github :repo "casouri/undo-hl")
  :hook (after-init . undo-hl-mode)
  :config (setq undo-hl-flash-duration 0.1))


;; [imenu] Jump to function definitions
(use-package imenu
  :commands (imenu--make-index-alist)
  :hook ((prog-mode conf-mode yaml-mode markdown-ts-mode org-mode) . (lambda () (imenu--make-index-alist t))))


;; [re-builder]
(use-package re-builder
  :ensure nil
  :commands re-builder
  :bind (:map reb-mode-map
              ("C-c C-k" . reb-quit)
              ("C-c C-p" . reb-prev-match)
              ("C-c C-n" . reb-next-match))
  :config
  (setq reb-re-syntax 'string))


;; [separedit]
(use-package separedit
  :straight t
  :bind (:map prog-mode-map
              ("C-c '" . separedit))
  :config
  (setq separedit-default-mode 'markdown-ts-mode))


;; [emacs-reader] read docs in emacs
(use-package reader
  :straight '(reader :type git :host codeberg :repo "MonadicSheep/emacs-reader"
                     :files ("*.el" "render-core.dylib")
                     :pre-build ("make" "all")))
