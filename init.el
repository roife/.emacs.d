;; Speed up startup
(setq auto-mode-case-fold nil)

;; startup time
;; (defun efs/display-startup-time ()
;;   (message
;;    "Emacs loaded in %s with %d garbage collections."
;;    (format
;;     "%.2f seconds"
;;     (float-time
;;      (time-subtract after-init-time before-init-time)))
;;    gcs-done))
;;
;; (add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Suppress flashing at startup
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;; Load path. Force "core" at the head to reduce the startup time.
(push (expand-file-name "core" user-emacs-directory) load-path)

;; Proxy
(setq url-proxy-services
      '(("http" . "127.0.0.1:7890")
        ("https" . "127.0.0.1:7890")))

;; Package Manager
(require 'init-straight)
(require 'init-ui)
;; ;; Basic
(require 'init-basic)

;; UI
(require 'init-mac)
(require 'init-highlight)
(require 'init-edit)
(require 'init-completion)
(require 'init-persp)
(require 'init-window)
(require 'init-dired)
(require 'init-test)
(require 'init-eshell)
(require 'init-prog)
(require 'init-writing)
(require 'init-vcs)
(require 'init-treemacs)
;; (require 'init-lsp)
(require 'init-dict)

(require 'init-util)
(require 'init-modal)
(require 'init-modeline)
