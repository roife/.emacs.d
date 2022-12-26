;;; -*- lexical-binding: t -*-

;; startup time
(defun efs/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Proxy
(setq url-proxy-services
      '(("http" . "127.0.0.1:7890")
        ("https" . "127.0.0.1:7890")))

(defvar +init-files (list
                     'init-straight
                     'init-ui
                     'init-basic
                     (when (eq system-type 'darwin) 'init-mac)
                     'init-highlight
                     'init-edit
                     'init-completion
                     'init-persp
                     'init-window
                     'init-dired
                     'init-eshell
                     'init-prog
                     'init-smartparens
                     'init-writing
                     'init-vcs
                     'init-treemacs
                     'init-dict
                     'init-ibuffer
                     'init-util
                     'init-modal
                     'init-modeline
                     ))

(let ((init-directory (expand-file-name "core/" user-emacs-directory)))
  (dolist (file +init-files)
    (when file
      (load-file (concat init-directory (symbol-name file) ".el")))))
