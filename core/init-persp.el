;;; -*- lexical-binding: t -*-

;; [persp-mode] Windows/buffers sets shared among frames + save/load.
(use-package persp-mode
  :straight t
  :defines (recentf-exclude)
  :commands (get-current-persp persp-contain-buffer-p)
  :hook ((after-init . persp-mode))
  :init (setq persp-keymap-prefix (kbd "C-x p")
              persp-nil-name "default"
              persp-set-last-persp-for-new-frames nil
              persp-kill-foreign-buffer-behaviour 'kill
              persp-auto-resume-time 1.0)
  :config
  ;; Don't save if the state is not loaded
  (defvar persp-state-loaded nil
    "Whether the state is loaded.")

  (defun my-persp-after-load-state (&rest _)
    (setq persp-state-loaded t))
  (advice-add #'persp-load-state-from-file :after #'my-persp-after-load-state)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (add-hook 'find-file-hook #'my-persp-after-load-state)))

  (defun my-persp-asave-on-exit (fn &optional interactive-query opt)
    (or (not persp-state-loaded)
        (funcall fn interactive-query opt)))
  (advice-add #'persp-asave-on-exit :around #'my-persp-asave-on-exit)

  ;; Don't save dead or temporary buffers
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
  (push persp-save-dir recentf-exclude)

  ;; Eshell integration
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))

  ;; Shell integration
  (persp-def-buffer-save/load
   :mode 'shell-mode :tag-symbol 'def-shell-buffer
   :mode-restore-function (lambda (_) (shell))
   :save-vars '(major-mode default-directory))
  )

(provide 'init-persp)
