;;; -*- lexical-binding: t -*-

;; [dired] File manager
(use-package dired
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  (setq
   ;; Always delete and copy recursively
   dired-recursive-deletes 'top
   dired-recursive-copies 'always
   ;; Move between two dired buffer quickly
   dired-dwim-target t
   ;; Ask whether destination dirs should get created when copying/removing files.
   dired-create-destination-dirs 'ask
   ;; don't prompt to revert, just do it
   dired-auto-revert-buffer #'dired-buffer-stale-p
   ;; symlink
   dired-hide-details-hide-symlink-targets nil
   )

  (when (eq system-type 'darwin)
    (if (executable-find "gls")
        (setq insert-directory-program "gls") ; Use GNU ls as `gls' from `coreutils' if available.
      ;; Suppress the warning: `ls does not support --dired'.
      (setq dired-use-ls-dired nil)))

  (when (or (not (eq system-type 'darwin)) (executable-find "gls"))
    (setq ls-lisp-use-insert-directory-program t ; Using `insert-directory-program'
          ;; Show directory first
          dired-listing-switches "-alh --group-directories-first"))
  )

;; [dired-git-info] Show git info in dired
(use-package dired-git-info
  :straight t
  :after dired
  :bind (:map dired-mode-map
              ("g" . dired-git-info-mode)
              ("r" . revert-buffer))
  :config
  (setq dgi-commit-message-format "%h %cs %s"
        dgi-auto-hide-details-p nil)
  )

;; [dired-aux] Extra Dired functionality
(use-package dired-aux
  :after dired
  :config
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))


;; [dired-x] Extra Dired functionality
(use-package dired-x
  :after dired
  :bind (:map dired-mode-map
              ("." . dired-omit-mode))
  :config
  (let ((cmd (cond ((and (eq system-type 'darwin) (display-graphic-p)) "open")
                   ((and (eq system-type 'gnu/linux) (display-graphic-p)) "xdg-open")
                   ((and (eq system-type 'windows-nt) (display-graphic-p)) "start")
                   (t ""))))
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd))))

  (setq dired-omit-verbose nil
        ;; hide dot files
        dired-omit-files "^\\..*\\'")

  ;; Disable the prompt about killing the Dired buffer for a deleted directory.
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  )


;; [fd-dired] Using `fd' with `find-dired'
(use-package fd-dired
  :straight t
  :bind ([remap find-dired] . fd-dired))


;; [dired-hacks] Several additional extensions for dired
(use-package dired-hacks
  :straight (:files (:defaults "*.el"))
  :after dired
  :init
  (use-package dired-subtree
    :bind (:map dired-mode-map
                ("TAB" . dired-subtree-toggle))
    :config
    (setq dired-subtree-line-prefix "  |  ")))


;; [diredfl] Make dired colorful
(use-package diredfl
  :straight t
  :hook (dired-mode . diredfl-mode))
