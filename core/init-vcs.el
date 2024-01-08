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

  ;; Make fringes look better (slow)
  ;; (define-fringe-bitmap '+diff-hl-bmp (vector #b11110000) 1 8 '(center t))
  ;; (setq diff-hl-fringe-bmp-function #'(lambda (&rest _) '+diff-hl-bmp))

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

  ;; Syntactic diff with [difftastic]
  (defun +magit--with-difftastic (buffer command)
    "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
    (let ((process-environment
           (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                         (number-to-string (frame-width)))
                 process-environment)))

      ;; Clear the result buffer (we might regenerate a diff, e.g., for
      ;; the current changes in our working directory).
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer))

      ;; Now spawn a process calling the git COMMAND.
      (make-process
       :name (buffer-name buffer)
       :buffer buffer
       :command command
       :noquery t ; Don't query for running processes when emacs is quit.
       :sentinel
       ;; Show the result buffer once the process has finished.
       (lambda (proc event)
         (when (eq (process-status proc) 'exit)
           (with-current-buffer (process-buffer proc)
             (goto-char (point-min))
             (ansi-color-apply-on-region (point-min) (point-max))
             (setq buffer-read-only t)
             (view-mode)
             (end-of-line)
             ;; difftastic diffs are usually 2-column side-by-side,
             ;; so ensure our window is wide enough.
             (let ((width (current-column)))
               (while (zerop (forward-line 1))
                 (end-of-line)
                 (setq width (max (current-column) width)))
               ;; Add column size of fringes
               (setq width (+ width
                              (fringe-columns 'left)
                              (fringe-columns 'right)))
               (goto-char (point-min))
               (pop-to-buffer
                (current-buffer)
                `(;; If the buffer is that wide that splitting the frame in
                  ;; two side-by-side windows would result in less than
                  ;; 80 columns left, ensure it's shown at the bottom.
                  ,(when (> 80 (- (frame-width) width))
                     #'display-buffer-at-bottom)
                  (window-width
                   . ,(min width (frame-width))))))))))))

  (defun +magit-show-with-difftastic (rev)
    "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If REV is given, just use it.
            (when (boundp 'rev) rev)
            ;; If not invoked with prefix arg, try to guess the REV from
            ;; point's position.
            (and (not current-prefix-arg)
                 (or (magit-thing-at-point 'git-revision t)
                     (magit-branch-or-commit-at-point)))
            ;; Otherwise, query the user.
            (magit-read-branch-or-commit "Revision"))))
    (if (not rev)
        (error "No revision specified")
      (+magit--with-difftastic
       (get-buffer-create (concat "*git show difftastic " rev "*"))
       (list "git" "--no-pager" "show" "--ext-diff" rev))))

  (defun +magit-diff-with-difftastic (arg)
    "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If RANGE is given, just use it.
            (when (boundp 'range) range)
            ;; If prefix arg is given, query the user.
            (and current-prefix-arg
                 (magit-diff-read-range-or-commit "Range"))
            ;; Otherwise, auto-guess based on position of point, e.g., based on
            ;; if we are in the Staged or Unstaged section.
            (pcase (magit-diff--dwim)
              ('unmerged (error "unmerged is not yet implemented"))
              ('unstaged nil)
              ('staged "--cached")
              (`(stash . ,value) (error "stash is not yet implemented"))
              (`(commit . ,value) (format "%s^..%s" value value))
              ((and range (pred stringp)) range)
              (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
    (let ((name (concat "*git diff difftastic"
                        (if arg (concat " " arg) "")
                        "*")))
      (+magit--with-difftastic
       (get-buffer-create name)
       `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

  (transient-append-suffix 'magit-diff '(-1 -1)
    [("D" "Difftastic Diff (dwim)" +magit-diff-with-difftastic)
     ("S" "Difftastic Show" +magit-show-with-difftastic)])
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
  :init (abridge-diff-mode 1))


;; [magit-delta] Use delta as git diff viewer
(use-package magit-delta
  :straight t
  :after magit
  :hook (magit-mode . magit-delta-mode)
  :config
  (setq magit-delta-delta-args '("--max-line-distance" "0.6"
                                 "--24-bit-color" "always"
                                 "--color-only"
                                 "--features" "magit-delta"))

  (defun +magit-delta-toggle ()
    (interactive)
    (magit-delta-mode (if magit-delta-mode -1 1))
    (magit-refresh))
  (transient-append-suffix 'magit-diff '(-1 -1 -1)
    '("l" "Toggle magit-delta" +magit-delta-toggle))
  )
