;;; -*- lexical-binding: t -*-

;; [straight] Package manager config
(setq straight-check-for-modifications nil      ; skip modification
      straight-vc-git-default-clone-depth '(1 single-branch)     ; shadow clone
      comp-deferred-compilation-deny-list ()    ; config native comp
      straight-disable-native-compile (not (and (fboundp 'native-comp-available-p)
                                                (native-comp-available-p))))

;; [straight] Installation
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; [use-package] Installation
(straight-use-package 'use-package)

;; [use-package] Config for use-package
(setq use-package-always-demand (daemonp)
      use-package-always-defer (not (daemonp))
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

(provide 'init-straight)
