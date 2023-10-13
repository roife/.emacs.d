;;; -*- lexical-binding: t -*-


;; [vc-mode] Version control interface
(use-package vc
  :config
  (setq vc-allow-async-revert t))


;; [diff-hl] Highlight uncommitted changes using VC
(use-package diff-hl
  :straight t
  :defines desktop-minor-mode-table
  :hook ((find-file    . diff-hl-mode)
         (vc-dir-mode  . diff-hl-dir-mode)
         (dired-mode   . diff-hl-dired-mode)
         ((diff-hl-mode diff-hl-dir-mode diff-hl-dired-mode) . +diff-hl--fallback-margin)
         ((diff-hl-mode diff-hl-dir-mode diff-hl-dired-mode) . diff-hl-show-hunk-mouse-mode))
  :config
  (setq
   diff-hl-draw-borders nil
   ;; Reduce load on remote
   diff-hl-disable-on-remote t
   ;; A slightly faster algorithm for diffing
   vc-git-diff-switches '("--histogram"))

  ;; Fall back to the display margin since the fringe is unavailable in tty
  (defun +diff-hl--fallback-margin ()
    (if (display-graphic-p)
        (diff-hl-margin-local-mode -1)
      (diff-hl-margin-local-mode))
    (diff-hl-update-once))

  ;; Make fringes look better
  (define-fringe-bitmap '+diff-hl-bmp (vector #b11100000) 1 8 '(center t))
  (setq diff-hl-fringe-bmp-function #'(lambda (&rest _) '+diff-hl-bmp))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  ;; Integration with flymake
  (with-eval-after-load 'flymake
    (setq flymake-fringe-indicator-position 'right-fringe))

  ;; WORKAROUND: Integration with ws-butler
  (advice-add #'ws-butler-after-save :after #'diff-hl-update-once)

  ;; HACK: Update after vc-state refreshed
  (advice-add #'vc-refresh-state :after #'diff-hl-update)

  ;; Update after focus change
  (add-function :after after-focus-change-function #'diff-hl-update-once)
  )


;; [magit] Version control interface
(use-package magit
  :straight t
  :bind (("C-x g" . magit))
  :hook ((magit-process-mode . goto-address-mode))
  :config
  (setq
   ;; word-granularity diff
   magit-diff-refine-hunk t
   ;; dont paint whitespace
   magit-diff-paint-whitespace nil
   ;; Don't autosave repo buffers. This is too magical
   magit-save-repository-buffers nil
   ;; Don't display parent/related refs in commit buffers; they are rarely helpful and only add to runtime costs.
   magit-revision-insert-related-refs nil)

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
  )


(use-package forge
  :straight t
  :after magit
  :custom-face
  (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline nil))))
  :config
  (setq forge-topic-list-columns
        '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
          ("Title" 60 t nil title  nil)
          ("State" 6 t nil state nil)
          ("Updated" 10 t nil updated nil)))
  )


;; Show TODOs in magit
(use-package magit-todos
  :straight t
  :after magit
  :init
  (let ((inhibit-message t))
    (magit-todos-mode 1))
  :config
  (with-eval-after-load 'magit-status
    (transient-append-suffix 'magit-status-jump '(0 0 -1)
      '("t " "Todos" magit-todos-jump-to-todos)))
  )


;; [smerge] Highlight all the conflicted regions for git
(use-package smerge-mode
  :hook ((find-file . smerge-try-smerge))
  :config
  (defun smerge-try-smerge ()
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
