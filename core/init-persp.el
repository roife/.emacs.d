;;; -*- lexical-binding: t -*-

;; * add a *scratch* buffer when create a new persp
;; * save before quit a persp

;; [persp-mode] Windows/buffers sets shared among frames + save/load.
(use-package persp-mode
  :straight t
  :defines (recentf-exclude)
  :commands (get-current-persp persp-contain-buffer-p)
  :hook ((after-init . persp-mode))
  :config
  (setq
   persp-keymap-prefix (kbd "C-x p")
   persp-save-dir (concat +cache-dir "persp-confs/")
   persp-autokill-buffer-on-remove 'kill-weak
   persp-reset-windows-on-nil-window-conf nil
   persp-set-last-persp-for-new-frames t
   persp-remove-buffers-from-nil-persp-behaviour nil
   ;; Do not auto load
   persp-auto-resume-time 0)

  (defun +load-last-persp ()
    "Load last persp."
    (interactive)
    (persp-load-state-from-file))

  ;; Don't save dead or [temporary buffers]
  (add-hook 'persp-filter-save-buffers-functions
            (lambda (b)
              "Ignore dead and unneeded buffers."
              (or (not (buffer-live-p b))
                  (string-prefix-p " *" (buffer-name b)))))

  (add-hook 'persp-filter-save-buffers-functions
            (lambda (b)
              "Ignore temporary buffers."
              (let ((bname (file-name-nondirectory (buffer-name b))))
                (or (string-prefix-p ".newsrc" bname)
                    (string-prefix-p "magit" bname)
                    (string-prefix-p "COMMIT_EDITMSG" bname)
                    (string-prefix-p "Pfuture-Callback" bname)
                    (string-prefix-p "treemacs-persist" bname)
                    (string-match-p "\\.elc\\|\\.tar\\|\\.gz\\|\\.zip\\'" bname)
                    (string-match-p "\\.bin\\|\\.so\\|\\.dll\\|\\.exe\\'" bname)))))

  ;; Don't save persp configs in `recentf'
  (with-eval-after-load 'recentf
    (push persp-save-dir recentf-exclude))

  ;; Eshell integration
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))
  ;; Compile integration
  (persp-def-buffer-save/load
   :mode 'compilation-mode :tag-symbol 'def-compilation-buffer
   :save-vars '(major-mode default-directory compilation-directory
                           compilation-environment compilation-arguments))
  ;; Magit integration
  (persp-def-buffer-save/load
   :mode 'magit-status-mode :tag-symbol 'def-magit-status-buffer
   :save-vars '(default-directory)
   :load-function (lambda (savelist &rest _)
                    (cl-destructuring-bind (buffer-name vars &rest _rest) (cdr savelist)
                      (magit-status (alist-get 'default-directory vars)))))

  ;; Per-workspace [winner-mode] history
  (add-to-list 'window-persistent-parameters '(winner-ring . t))

  (add-hook 'persp-before-deactivate-functions
            (lambda (_)
              (when (and (bound-and-true-p winner-mode)
                         (get-current-persp))
                (set-persp-parameter
                 'winner-ring (list winner-currents
                                    winner-ring-alist
                                    winner-pending-undo-ring)))))

  (add-hook 'persp-activated-functions
            (lambda (_)
              (when (bound-and-true-p winner-mode)
                (cl-destructuring-bind
                    (currents alist pending-undo-ring)
                    (or (persp-parameter 'winner-ring) (list nil nil nil))
                  (setq winner-undo-frame nil
                        winner-currents currents
                        winner-ring-alist alist
                        winner-pending-undo-ring pending-undo-ring)))))

  ;; Don't try to persist dead/remote buffers. They cause errors.
  (add-hook 'persp-filter-save-buffers-functions
            (lambda (buf) (not (buffer-live-p buf))))
  (add-hook 'persp-filter-save-buffers-functions
            (lambda (buf) (let ((dir (buffer-local-value 'default-directory buf)))
                       (ignore-errors (file-remote-p dir)))))
  )
