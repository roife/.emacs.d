;;; -*- lexical-binding: t -*-

;; * add a *scratch* buffer when create a new persp
;; * save before quit a persp

;; [persp-mode] Windows/buffers sets shared among frames + save/load.
(use-package persp-mode
  :straight t
  :defines (recentf-exclude)
  :commands (get-current-persp persp-contain-buffer-p persp-update-names-cache)
  :hook ((after-init . persp-mode))
  :bind (("C-`" . +eshell-toggle-by-persp)
         :map persp-key-map
              ("RET" . persp-switch))
  :init
  (setq persp-keymap-prefix (kbd "C-c p")
        persp-nil-name "⊥")
  :config
  (setq
   persp-autokill-buffer-on-remove 'kill-weak
   persp-reset-windows-on-nil-window-conf nil
   persp-set-last-persp-for-new-frames t
   persp-remove-buffers-from-nil-persp-behaviour nil
   ;; Do not auto load
   persp-auto-resume-time 0)

  (defun +eshell-toggle-by-persp (&optional arg)
    "Toggle a persistent eshell popup window.
If popup is visible but unselected, select it.
If popup is focused, kill it."
    (interactive "P")
    (if arg
        (chatgpt-shell)
      (require 'eshell)
      (if-let* ((name (concat "*Eshell-pop* [" persp-last-persp-name "]"))
                (win (get-buffer-window name)))
          (if (eq (selected-window) win)
              ;; If users attempt to delete the sole ordinary window. silence it.
              (ignore-errors (delete-window win))
            (select-window win))
        (let ((display-comint-buffer-action '(display-buffer-at-bottom
                                              (inhibit-same-window . nil)))
              (eshell-buffer-name name))
          (with-current-buffer (eshell)
            (persp-add-buffer name)
            (add-hook 'eshell-exit-hook #'(lambda () (ignore-errors (delete-window win))) nil t))))))

  (defun +load-last-persp ()
    "Load last persp."
    (interactive)
    (persp-load-state-from-file))

  ;; Don't save [dead] [temp] [remote]
  (add-hook! persp-filter-save-buffers-functions
    (defun +persp-ignore-dead-or-temp-buffers (b)
      "Ignore dead or temp buffers."
      (or (not (buffer-live-p b))
          (string-prefix-p " *" (buffer-name b)))))

  (add-hook! persp-filter-save-buffers-functions
    (defun +persp-ignore-more-temp-buffers (b)
      "Ignore more temporary buffers."
      (let ((bname (file-name-nondirectory (buffer-name b))))
        (or (string-prefix-p ".newsrc" bname)
            (string-prefix-p "magit" bname)
            (string-prefix-p "COMMIT_EDITMSG" bname)
            (string-prefix-p "Pfuture-Callback" bname)
            (string-prefix-p "treemacs-persist" bname)
            (string-match-p "\\.elc\\|\\.tar\\|\\.gz\\|\\.zip\\'" bname)
            (string-match-p "\\.bin\\|\\.so\\|\\.dll\\|\\.exe\\'" bname)))))

  ;; (add-hook! persp-filter-save-buffers-functions
  ;;   (defun +persp-ignore-remote-buffers (buf)
  ;;     "Ignore remote buffers, which cause errors"
  ;;     (let ((dir (buffer-local-value 'default-directory buf)))
  ;;       (ignore-errors (file-remote-p dir)))))

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
   :mode 'eww-mode :tag-symbol 'def-eww-buffer
   :save-vars '(major-mode eww-history eww-data eww-history-position)
   :after-load-function
   #'(lambda (b &rest _)
       (let ((cur-buf (current-buffer)))
         (with-current-buffer b
           (when-let ((url (plist-get eww-data :url)))
             (eww url nil)))
         ;; restore buffer
         (switch-to-buffer cur-buf))))

  ;; Telega integration
  (persp-def-buffer-save/load
   :mode 'telega-root-mode :tag-symbol 'def-telega-root-buffer
   :save-vars '()
   :load-function
   #'(lambda (&rest _) (telega '(4)))
   :after-load-function #'ignore)

  ;; Per-workspace [tab-bar]
  (with-eval-after-load 'tab-bar
    ;; Save the current workspace's tab bar data.
    (add-hook! persp-before-deactivate-functions
      (defun +persp-save-tab-bar-before-switching (&rest _)
        (set-persp-parameter 'tab-bar-tabs (tab-bar-tabs))
        (set-persp-parameter 'tab-bar-closed-tabs tab-bar-closed-tabs)))
    ;; Restores the tab bar data of the workspace we have just switched to.
    (add-hook! persp-activated-functions
      (defun +persp-restore-tab-bar-after-switching (&rest _)
        (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
        (setq tab-bar-closed-tabs (persp-parameter 'tab-bar-closed-tabs))))

    (add-hook! persp-after-load-state-functions
      (defun +persp-load-tab-bar-config-from-file (&rest _)
        (when (and (persp-parameter 'tab-bar-tabs)
                   (not tab-bar-mode))
          (tab-bar-mode 1))))
    )

  ;; Per-workspace [winner-mode] history
  (with-eval-after-load 'winner
    (add-to-list 'window-persistent-parameters '(winner-ring . t))

    (add-hook! persp-before-deactivate-functions
      (defun +persp-save-winner-before-switching (&rest _)
        (when (get-current-persp)
          (set-persp-parameter
           'winner-ring (list winner-currents
                              winner-ring-alist
                              winner-pending-undo-ring)))))

    (add-hook! persp-activated-functions
      (defun +persp-restore-winner-after-switching (&rest _)
        (cl-destructuring-bind
            (currents alist pending-undo-ring)
            (or (persp-parameter 'winner-ring) (list nil nil nil))
          (setq winner-undo-frame nil
                winner-currents currents
                winner-ring-alist alist
                winner-pending-undo-ring pending-undo-ring))))
    )

  ;; Filter frame parameters
  ;; HACK: `pp' is slow, replace it with prin1
  (+advice-pp-to-prin1! 'persp-save-state-to-file)

  (defvar +persp-filter-parameters-on-save
        '((tab-bar-tabs . (lambda (conf) (frameset-filter-tabs conf nil nil t)))
          (winner-ring . ignore)))

  (defvar +persp-set-parameter-on-save
    '(+persp-save-tab-bar-before-switching +persp-save-winner-before-switching))

  (defadvice! +persp--filter-frame-parameters-on-save-a (fn &rest args)
    :around #'persp-save-state-to-file
    (mapcar #'funcall +persp-set-parameter-on-save)
    (let ((all-persp-confs (make-hash-table))
          (ret-val))
      (dolist (persp (hash-table-values *persp-hash*))
        (let ((cur-persp-confs (make-hash-table)))
          (cl-loop for (tag . filter) in +persp-filter-parameters-on-save
                   do (let ((old (persp-parameter tag persp)))
                        (puthash tag old cur-persp-confs)
                        (set-persp-parameter tag (funcall filter old) persp)))
          (puthash persp cur-persp-confs all-persp-confs)))
      (setq ret-val (apply fn args))
      (dolist (persp (hash-table-values *persp-hash*))
        (cl-loop for (tag . _) in +persp-filter-parameters-on-save
                 do (let* ((cur-persp-confs (gethash persp all-persp-confs))
                           (old (gethash tag cur-persp-confs)))
                      (set-persp-parameter tag old persp))))
      ret-val))

  ;; Visual selection surviving workspace changes
  (add-hook 'persp-before-deactivate-functions #'deactivate-mark)

  ;; WORKAROUND: ace-window
  (add-hook! persp-activated-functions
    (defun +persp-update-ace-window-config (&rest _)
      (aw-update)))
  )
