;;; -*- lexical-binding: t -*-

;; [vc-mode] Version control interface
(use-package vc
  :config
  (setq vc-handled-backends '(Git)
        vc-consult-headers nil
        vc-allow-async-revert t
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
    (add-hook! magit-post-refresh-hook #'diff-hl-magit-post-refresh))

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


;; [remoto] Browse GitHub repositories without cloning
(use-package remoto
  :straight (:host github :repo "agzam/remoto.el")
  :demand t)


;; [smerge] Highlight all the conflicted regions for git
(use-package smerge-mode
  :preface
  (defun +smerge-flymake-backend (report-fn &rest _args)
    "Report unresolved merge conflicts in the current buffer."
    (let (diagnostics)
      (save-excursion
        (goto-char (point-min))
        (while (smerge-find-conflict)
          (let ((beg (match-beginning 0)))
            (push (flymake-make-diagnostic
                   (current-buffer) beg
                   (save-excursion (goto-char beg) (line-end-position))
                   'flymake-error "Unresolved merge conflict")
                  diagnostics))))
      (funcall report-fn (nreverse diagnostics))))

  (defun +smerge-flymake-setup-h ()
    "Add the Smerge Flymake backend to a conflicted buffer."
    (when smerge-mode
      (add-hook 'flymake-diagnostic-functions
                #'+smerge-flymake-backend nil t)
      (if (bound-and-true-p flymake-mode)
          (flymake-start)
        (flymake-mode 1))))

  (defun +smerge-try-smerge ()
    (when (and buffer-file-name
               (save-excursion
                 (goto-char (point-min))
                 (re-search-forward "^<<<<<<< " nil t))
               (vc-backend buffer-file-name))
      (require 'smerge-mode)
      (smerge-mode 1)))
  :hook ((find-file . +smerge-try-smerge)
         (smerge-mode . +smerge-flymake-setup-h)))


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
