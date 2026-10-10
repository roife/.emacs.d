;;; -*- lexical-binding: t -*-

(add-hook 'window-setup-hook
          (lambda ()
            (message "window-setup: %.3fs, after-init: %.3fs"
                     (float-time (time-subtract nil before-init-time))
                     (float-time (time-subtract after-init-time before-init-time)))))

(dolist (file `(util straight basic ui xterm
                     ,(pcase system-type ('gnu/linux 'linux) ('darwin 'mac))
                     completion tools highlight edit window dired shell prog
                     writing org agenda vcs ibuffer ime modal modeline
                     ai social media test))
  (load (expand-file-name (format "core/init-%s.el" file) user-emacs-directory)
        nil 'nomessage 'nosuffix))
