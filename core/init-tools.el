;;; -*- lexical-binding: t -*-

;; [browse-url] Pass a URL to browser
(use-package browse-url
  :defines dired-mode-map
  :bind (("C-, o" . browse-url-at-point)
         ("C-, e" . browse-url-emacs))
  :config
  ;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)
  )


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


;; [goto-addr] Click to open URL
(use-package goto-addr
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))


;; [avy] Jump with several key strock
(use-package avy
  :straight t
  :bind (("C-, ." . avy-goto-char)
         ("C-, ," . avy-goto-char-2)
         ("C-, l" . avy-goto-line))
  :hook (after-init . avy-setup-default)
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
    (require 'hideshow)
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
  ;; Display line counts
  (defun +hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (let ((lines (number-to-string (count-lines (overlay-start ov) (overlay-end ov)))))
                     (concat " "
                             (propertize (concat " .. L" lines " ") 'face '(:inherit shadow :height 0.8 :box t))
                             " "))
                   )))
  (setq hs-set-up-overlay #'+hs-display-code-line-counts)
  )


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

  (defun +project-previous-buffer (arg)
    "Toggle to the previous buffer that belongs to current project."
    (interactive "P")
    (unless arg
      (if-let* ((pr (project-current)))
          (switch-to-buffer
           (->> (project--buffer-list pr)
                (--remove (or (minibufferp it)
                              (get-buffer-window-list it)))
                (car))))))

  ;; Use [fd] to find file in project
  (defun +search-project-files-with-fd (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -H -t f -0 . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))
  (cl-defmethod project-files ((project (head local)) &optional dirs)
    "Override `project-files' to use `fd' in local projects."
    (mapcan #'+search-project-files-with-fd
            (or dirs (list (project-root project)))))
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
  :hook ((prog-mode conf-mode yaml-mode markdown-mode org-mode) . (lambda () (when (fboundp 'imenu--make-index-alist) (imenu--make-index-alist t)))))


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
  (setq separedit-default-mode 'markdown-mode))
