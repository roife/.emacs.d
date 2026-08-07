;;; -*- lexical-binding: t -*-

(add-hook 'window-setup-hook
          (lambda ()
            (message "window-setup: %.3fs, after-init: %.3fs"
                     (float-time (time-subtract nil before-init-time))
                     (float-time (time-subtract after-init-time before-init-time)))))

(defvar +init-files (list
                     'init-util
                     'init-straight
                     'init-basic
                     'init-ui
                     'init-xterm
                     (when (eq system-type 'darwin) 'init-mac)
                     'init-completion
                     'init-tools
                     'init-highlight
                     'init-edit
                     'init-window
                     'init-dired
                     'init-shell
                     'init-prog
                     'init-writing
                     'init-org
                     'init-vcs
                     'init-ibuffer
                     'init-ime
                     'init-modal
                     'init-modeline
                     'init-ai
                     'init-social
                     'init-gnus
                     'init-media
                     'init-test
                     ))

(let ((init-directory (expand-file-name "core/" user-emacs-directory)))
  (dolist (file +init-files)
    (when file
      (load-file (concat init-directory (symbol-name file) ".el")))))
