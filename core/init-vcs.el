;;; -*- lexical-binding: t -*-

;; See `magit-maybe-define-global-key-bindings'
(use-package magit
  :straight t
  :init (setq magit-diff-refine-hunk t)
  :bind (("C-x g" . magit))
  :config
  ;; Exterminate Magit buffers
  (defun my-magit-kill-buffers (&rest _)
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
  (setq magit-bury-buffer-function #'my-magit-kill-buffers)
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
  :defines magit-todos-nice
  :init
  (setq magit-todos-nice (if (executable-find "nice") t nil))
  (let ((inhibit-message t))
    (magit-todos-mode 1))
  :config
  (with-eval-after-load 'magit-status
    (transient-append-suffix 'magit-status-jump '(0 0 -1)
      '("t " "Todos" magit-todos-jump-to-todos)))
  )


;; [smerge] Resolve diff3 conflicts
(use-package smerge-mode
  :hook ((find-file . smerge-try-smerge))
  :config
  (defun smerge-try-smerge ()
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (require 'smerge-mode)
        (smerge-mode 1))))
  )


;; [browse-at-remote] Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :straight t
  :bind (:map vc-prefix-map
              ("B" . browse-at-remote))
  )


;; Git configuration major modes
(use-package git-modes
  :straight t)


(provide 'init-vcs)
