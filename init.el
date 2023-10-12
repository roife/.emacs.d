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

;; custom directory
(defvar +blog-dir (expand-file-name "~/source/roife.github.io/"))
(defvar +ebib-bib-dir (expand-file-name "~/Documents/papers/"))
(defvar +elfeed-enclosure-dir (expand-file-name "~/Downloads/"))

(defvar +init-files (list
                     'init-straight
                     'init-ui
                     'init-basic
                     'init-util
                     (when (eq system-type 'darwin) 'init-mac)
                     'init-highlight
                     'init-edit
                     'init-completion
                     'init-persp
                     'init-window
                     'init-dired
                     'init-eshell
                     'init-prog
                     'init-writing
                     'init-org
                     'init-vcs
                     'init-browser
                     'init-dict
                     'init-elfeed
                     'init-ibuffer
                     'init-test
                     'init-modal
                     'init-modeline
                     'init-tabbar
                     'init-bib
                     'init-ai
                     'init-chat
                     'init-pdf
                     'init-gnus
                     ;; 'init-webservice
                     ))

(let ((init-directory (expand-file-name "core/" user-emacs-directory)))
  (dolist (file +init-files)
    (when file
      (load-file (concat init-directory (symbol-name file) ".el")))))
(put 'dired-find-alternate-file 'disabled nil)
