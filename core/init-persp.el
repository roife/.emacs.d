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

  ;; Eww integration
  (persp-def-buffer-save/load
   :mode 'eww-mode :tag-symbol 'def-eww-status-buffer
   :save-vars '(major-mode eww-history eww-data eww-history-position)
   :after-load-function
   #'(lambda (b &rest _)
       (let ((cur-buf (current-buffer)))
         (with-current-buffer b
           (when-let ((url (plist-get eww-data :url)))
             (eww url nil)))
         ;; restore buffer
         (switch-to-buffer cur-buf))))

  ;; Tab bar integration
  (with-eval-after-load 'tab-bar
    ;; Save the current workspace's tab bar data.
    (add-hook 'persp-before-deactivate-functions
              (lambda (_)
                (set-persp-parameter 'tab-bar-tabs (tab-bar-tabs))
                (set-persp-parameter 'tab-bar-closed-tabs tab-bar-closed-tabs)))
    ;; Restores the tab bar data of the workspace we have just switched to.
    (add-hook 'persp-activated-functions
              (lambda (_)
                (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
                (setq tab-bar-closed-tabs (persp-parameter 'tab-bar-closed-tabs))
                (tab-bar--update-tab-bar-lines t)))

    ;; Filter frame parameters
    (setq +persp-filter-parameters-on-save
          '((tab-bar-tabs . (lambda (conf) (frameset-filter-tabs conf nil nil t)))
            (winner-ring . ignore)))

    (advice-add #'persp-save-state-to-file :around
                (lambda (fn &rest args)
                  (let ((all-persp-confs (make-hash-table))
                        (ret-val))
                    (dolist (persp (hash-table-values *persp-hash*))
                      (let ((cur-persp-confs (make-hash-table)))
                        (cl-loop for (tag . filter) in +persp-filter-parameters-on-save
                                 do (let ((old (persp-parameter tag persp)))
                                      (puthash persp old cur-persp-confs)
                                      (set-persp-parameter tag (funcall filter old) persp)))
                        (puthash persp cur-persp-confs all-persp-confs)))
                    (setq ret-val (apply fn args))
                    (dolist (persp (hash-table-values *persp-hash*))
                      (cl-loop for (tag . filter) in +persp-filter-parameters-on-save
                               do (let* ((cur-persp-confs (gethash persp all-persp-confs))
                                         (old (gethash tag cur-persp-confs)))
                                    (set-persp-parameter tag old persp))))
                    ret-val)))
    )

  ;; Per-workspace [winner-mode] history
  (with-eval-after-load 'winner
    (add-to-list 'window-persistent-parameters '(winner-ring . t))

  (add-hook 'persp-before-deactivate-functions
            (lambda (_)
              (when (get-current-persp)
                (set-persp-parameter
                 'winner-ring (list winner-currents
                                    winner-ring-alist
                                    winner-pending-undo-ring)))))

  (add-hook 'persp-activated-functions
            (lambda (_)
              (cl-destructuring-bind
                    (currents alist pending-undo-ring)
                    (or (persp-parameter 'winner-ring) (list nil nil nil))
                  (setq winner-undo-frame nil
                        winner-currents currents
                        winner-ring-alist alist
                        winner-pending-undo-ring pending-undo-ring))))

  (add-hook 'persp-before-save-state-to-file-functions
            (lambda (&rest _)
              (delete-persp-parameter 'winner-ring)))
    )

  ;; Don't try to persist dead/remote buffers. They cause errors.
  (add-hook 'persp-filter-save-buffers-functions
            (lambda (buf) (not (buffer-live-p buf))))
  (add-hook 'persp-filter-save-buffers-functions
            (lambda (buf) (let ((dir (buffer-local-value 'default-directory buf)))
                       (ignore-errors (file-remote-p dir)))))

  ;; Visual selection surviving workspace changes
  (add-hook 'persp-before-deactivate-functions #'deactivate-mark)
  )
