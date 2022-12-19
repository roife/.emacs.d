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

;; Load path. Force "core" at the head to reduce the startup time.
(push (expand-file-name "core" user-emacs-directory) load-path)

;; Proxy
(setq url-proxy-services
      '(("http" . "127.0.0.1:7890")
        ("https" . "127.0.0.1:7890")))

;; Package Manager
(require 'init-straight)
(require 'init-ui)
(require 'init-basic)
(when (eq system-type 'darwin)
  (require 'init-mac))

(require 'init-highlight)
(require 'init-edit)
(require 'init-completion)
(require 'init-persp)
(require 'init-window)
(require 'init-dired)
(require 'init-eshell)
(require 'init-prog)
(require 'init-writing)
(require 'init-vcs)
(require 'init-treemacs)
(require 'init-dict)

(require 'init-util)
(require 'init-modal)
(require 'init-modeline)
