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
;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;         ("http" . "127.0.0.1:7890")
;;         ("https" . "127.0.0.1:7890")))

(defvar +init-files (list
                     'init-util
                     'init-straight
                     'init-ui
                     'init-tabbar
                     'init-basic
                     'init-completion
                     (when (eq system-type 'darwin) 'init-mac)
                     'init-tools
                     'init-keybinding
                     'init-highlight
                     'init-edit
                     'init-spell
                     'init-window
                     'init-dired
                     'init-eshell
                     'init-prog
                     'init-writing
                     'init-org
                     'init-vcs
                     'init-browser
                     'init-ibuffer
                     'init-dict
                     'init-ime
                     'init-modal
                     'init-modeline
                     'init-bib
                     'init-ai
                     'init-pdf
                     'init-test

                     ;; 'init-chat
                     ;; 'init-persp
                     ;; 'init-elfeed
                     ;; 'init-gnus
                     ;; 'init-webservice
                     ))

(let ((init-directory (expand-file-name "core/" user-emacs-directory)))
  (dolist (file +init-files)
    (when file
      (load-file (concat init-directory (symbol-name file) ".el")))))
