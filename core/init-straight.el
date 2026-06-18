;;; -*- lexical-binding: t -*-

;; [straight] Package manager config (should put setq before installation for `straight`)
(setq straight-check-for-modifications nil                   ; skip modification
      straight-vc-git-default-clone-depth '(1 single-branch) ; shadow clone
      warning-suppress-log-types '((comp))                   ; Don't display comp warnings
      straight-disable-native-compile (not (and (fboundp 'native-comp-available-p)
                                                (native-comp-available-p))))

;; Installation
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; [use-package] config
(setq use-package-always-demand (daemonp)
      use-package-always-defer (not (daemonp))
      use-package-expand-minimally t
      use-package-enable-imenu-support t)
