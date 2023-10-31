;;; -*- lexical-binding: t -*-
;;; Mainly for speeding up startup time

;; Defer gc
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

;; Prevent unwanted runtime compilation
(setq native-comp-deferred-compilation t)

;; In noninteractive sessions, prioritize .el file. It saves IO time
(setq load-prefer-newer noninteractive)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Inhibit startup screen & message
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t
      inhibit-x-resources t
      inhibit-default-init t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)
(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

;; Suppress flashing at startup
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redraw-frame)))

;; Inhibit package.el initialization
(setq package-enable-at-startup nil)

;; Faster to disable these here (before initialized)
(push '(tab-bar-lines . 1) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)
(push '(undecorated-round . t) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
; Set these to nil so users don't have to toggle the modes twice to reactivate.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      tab-bar-mode t)

;; Case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; `file-name-handler-alist' is consulted on each call to `require', `load', or various file/io functions
(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-value)))))))

;; TODO: optimize `load-suffixes'

;; Site files will use `load-file', which emit messages and triggers redisplay
;; Make it silent and undo advice later
(define-advice load-file (:override (file) silence)
  (load file nil 'nomessage))
(define-advice startup--load-user-init-file (:after (&rest _) undo-silence)
  (advice-remove #'load-file #'load-file@silence))
