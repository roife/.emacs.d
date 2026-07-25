;;; -*- lexical-binding: t -*-

;; [vc-mode] Version control interface
(use-package vc
  :config
  (setq vc-allow-async-revert t
        vc-auto-revert-mode t
        vc-allow-rewriting-published-history t
        vc-dir-auto-hide-up-to-date 'revert))


;; [git-link] Get remote repo URL for buffer location
(use-package git-link
  :straight t
  :bind (("C-, g l" . git-link)
         ("C-, g c" . git-link-commit)
         ("C-, g h" . git-link-homepage)))


;; [diff-hl] Highlight uncommitted changes using VC
(use-package diff-hl
  :straight t
  :hook ((find-file . global-diff-hl-mode)
         (vc-dir-mode  . diff-hl-dir-mode)
         (dired-mode   . diff-hl-dired-mode))
  :bind (:map diff-hl-mode-map
              ("C-c v v" . diff-hl-show-hunk)
              ("C-c v r" . diff-hl-revert-hunk)
              ("C-c v [" . diff-hl-previous-hunk)
              ("C-c v ]" . diff-hl-next-hunk)
              ("C-c v s" . diff-hl-stage-current-hunk)
              ("C-c v u" . diff-hl-undo-revert-hunk))
  :config
  (setq
   ;; Reduce load on remote
   diff-hl-disable-on-remote t
   ;; A slightly faster algorithm for diffing
   vc-git-diff-switches '("--histogram")
   ;; Use margins in terminal frames where fringes don't exist.
   diff-hl-fallback-to-margin t)

  (defun +diff-hl--vc-face (type)
    (pcase type
      ('insert 'diff-refine-added)
      ('delete 'diff-refine-removed)
      ('change 'diff-refine-changed)))

  (setq diff-hl-fringe-face-function #'(lambda (type _pos) (+diff-hl--vc-face type))
        diff-hl-fringe-reference-face-function #'(lambda (type _pos) (+diff-hl--vc-face type)))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  ;; WORKAROUND: Integration with ws-butler
  (with-eval-after-load 'ws-butler
    (advice-add #'ws-butler-after-save :after #'diff-hl-update))

  ;; HACK: Update after vc-state refreshed
  (advice-add #'vc-refresh-state :after #'diff-hl-update)

  ;; Update after focus change for different mode.
  (defun +diff-hl-update-after-focus-change ()
    (cond ((bound-and-true-p diff-hl-mode)
           (diff-hl-update))
          ((bound-and-true-p diff-hl-dir-mode)
           (diff-hl-dir-update))
          ((bound-and-true-p diff-hl-dired-mode)
           (diff-hl-dired-update))
          (t t)))
  (add-function :after after-focus-change-function
                #'+diff-hl-update-after-focus-change)
  )


;; [magit] Version control interface
(use-package magit
  :straight t
  :bind (("C-x g" . magit))
  :hook ((magit-process-mode . goto-address-mode))
  :config
  (setq
   ;; word-granularity diff
   ;; magit-diff-refine-hunk nil
   ;; Highlight the changed region in the hunk
   ;; magit-diff-fontify-hunk t
   ;; dont paint whitespace
   magit-diff-paint-whitespace nil
   ;; Don't autosave repo buffers. This is too magical
   magit-save-repository-buffers nil
   ;; Don't display parent/related refs in commit buffers; they are rarely helpful and only add to runtime costs.
   magit-revision-insert-related-refs nil
   magit-diff-use-indicator-faces t
   magit-diff-highlight-trailing nil)

  ;; Exterminate Magit buffers
  (defun +magit-kill-buffers (&rest _)
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (magit-restore-window-configuration)
    (let ((buffers (magit-mode-get-buffers)))
      (when (eq major-mode 'magit-status-mode)
        (mapc (lambda (buf)
                (with-current-buffer buf
                  (if (and magit-this-process
                           (eq (process-status magit-this-process) 'run))
                      (bury-buffer buf)
                    (kill-buffer buf))))
              buffers))))
  (setq magit-bury-buffer-function #'+magit-kill-buffers)

  (defun +toggle-magit-difftastic ()
    "Toggle `magit-difftastic-mode' in Magit buffers."
    (interactive)
    (magit-difftastic-mode
     (if magit-difftastic-mode -1 1)))
  (transient-append-suffix 'magit-diff '(-1 -1)
    [("D" "Difftastic Diff" +toggle-magit-difftastic)])
  )


(use-package magit-difftastic
  :straight (:host github :repo "rschmukler/magit-difftastic")
  :after magit
  :config
  (setq magit-difftastic-display "inline"
        magit-difftastic-line-numbers nil
        magit-difftastic-syntax-highlight nil)
  )


(use-package forge
  :straight t
  :after magit
  :custom-face
  (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline unspecified))))
  :config
  (setq forge-topic-list-columns
        '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
          ("Title" 60 t nil title  nil)
          ("State" 6 t nil state nil)
          ("Updated" 10 t nil updated nil)))
  )

;; [magh.el] Section-oriented GitHub frontend powered by the `gh' CLI
(use-package magh
  :straight (:host github :repo "roife/magh.el")
  :bind (("C-, g g" . magh)
         ("C-, g G" . magh-dispatch)
         ("C-, g d" . magh-repo-status)
         ("C-, g D" . magh-repo-status-other)
         ("C-, g H" . magh-user-status)
         ("C-, g i" . magh-issue-list)
         ("C-, g p" . magh-pr-list)
         ("C-, g v" . magh-review-requests)
         ("C-, g w" . magh-run-list)
         ("C-, g e" . magh-release-list)
         ("C-, g /" . magh-search-dispatch)
         ("C-, g t" . magh-browse-repository)
         ("C-, g n" . magh-notifications-dispatch)
         ("C-, g r" . magh-command)
         ("C-, g a" . magh-api-request))
  :config
  (setq magh-list-limit 50
        magh-client-cache-ttl 30
        magh-confirm-destructive-actions t
        magh-notifications-unread-only t
        magh-notifications-group-by 'repository
        magh-view-inline-images t)

  ;; Keep user-maintained GitHub shortcuts across Emacs sessions.  savehist is
  ;; enabled from init-basic.el's after-init hook in non-daemon sessions.
  (with-eval-after-load 'savehist
    (dolist (variable '(magh-known-repositories
                        magh-favorite-organizations
                        magh-workflow-template-repositories))
      (add-to-list 'savehist-additional-variables variable))))


;; [magh-magit] Lightweight asynchronous magh.el summaries in Magit status
(use-package magh-magit
  :straight (:host github :repo "roife/magh.el")
  :after magit
  :demand t
  :config
  (setq magh-magit-dispatch-key "@"
        magh-magit-status-sections '(pr issue run)
        magh-magit-summary-scope 'repository
        magh-magit-list-limit 10
        magh-magit-cache-ttl 30
        ;; Forge owns its PR and Issue sections; magh.el still shows Actions.
        magh-hide-forge-duplicates t)
  (magh-magit-mode 1))


;; Structured actions for magh.el candidates in Embark.
(use-package magh-embark
  :straight (:host github :repo "roife/magh.el")
  :after embark
  :demand t
  :config
  (magh-embark-mode 1))


;; Keep magh.el's native Issue/PR viewer, with an explicit Forge -> magh.el bridge.
(use-package magh-forge
  :straight (:host github :repo "roife/magh.el")
  :after forge
  :commands (magh-forge-open-current-topic-in-magh)
  :bind (:map forge-topic-mode-map
              ("C-c C-g" . magh-forge-open-current-topic-in-magh)))


;; Show TODOs in magit
(use-package magit-todos
  :straight t
  :after magit
  :hook (magit-mode . magit-todos-mode)
  :config
  (with-eval-after-load 'magit-status
    (transient-append-suffix 'magit-status-jump '(0 0 -1)
      '("t " "Todos" magit-todos-jump-to-todos)))
  )


;; [smerge] Highlight all the conflicted regions for git
(use-package smerge-mode
  :hook ((find-file . +smerge-try-smerge))
  :config
  (defun +smerge-try-smerge ()
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (require 'smerge-mode)
          (smerge-mode 1)))))
  )


;; [browse-at-remote] Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :straight t
  :bind (:map vc-prefix-map
              ("B" . browse-at-remote))
  )


;; [git-modes] Git configuration major modes
(use-package git-modes
  :straight t)


;; [abridge-diff]
(use-package abridge-diff
  :straight t
  :after magit ;; optional, if you'd like to use with magit
  :hook (magit-diff-visit-file . abridge-diff-mode))
