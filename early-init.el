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

;; Inhibit startup screen
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-major-mode 'fundamental-mode
      inhibit-startup-message t
      initial-scratch-message nil)
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
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
; Set these to nil so users don't have to toggle the modes twice to reactivate.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

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


;; Font
(push '(font . "JetBrains Mono 14") default-frame-alist)
