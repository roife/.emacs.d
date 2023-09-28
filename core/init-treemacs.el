;;; -*- lexical-binding: t -*-

;; [treemacs] File and project explorer
(use-package treemacs
  :straight t
  :functions (treemacs-filewatch-mode treemacs-git-mode treemacs-delete-other-windows)
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  :bind (("s-`"       . treemacs-select-window)
         ("C-x t t"   . +treemacs-toggle)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  ;; Bind after load treemacs
  (bind-keys :package treemacs
             ("C-x 1"     . treemacs-delete-other-windows))

  ;; treemacs-git-mode
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-sorting                 'alphabetic-case-insensitive-asc
        treemacs-follow-after-init       t
        treemacs-is-never-other-window   t
        treemacs-width                   30
        treemacs-no-png-images           t)

  (treemacs-filewatch-mode t)
  (treemacs-follow-mode -1)

  (defun +treemacs-toggle ()
    "Initialize or toggle treemacs.

Ensures that only the current project is present and all other projects have
been removed.

Use `treemacs' command for old functionality."
    (interactive)
    (require 'treemacs)
    (pcase (treemacs-current-visibility)
      (`visible (delete-window (treemacs-get-local-window)))
      (_ (if (project-current)
             (treemacs-add-and-display-current-project-exclusively)
           (treemacs))))))


(use-package treemacs-magit
  :straight t
  :demand t
  :after treemacs magit
  :commands treemacs-magit--schedule-update
  :hook ((magit-post-commit
          git-commit-post-finish
          magit-post-stage
          magit-post-unstage) . treemacs-magit--schedule-update))


(use-package treemacs-persp
  :straight t
  :demand t
  :after treemacs persp-mode
  :functions treemacs-set-scope-type
  :config (treemacs-set-scope-type 'Perspectives))
