;; Defer gc
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent unwanted runtime compilation
(setq native-comp-deferred-compilation t)

;; In noninteractive sessions, prioritize .el file. It saves IO time
(setq load-prefer-newer noninteractive)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Inhibit package.el initialization
(setq package-enable-at-startup nil)

;; Faster to disable these here (before initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; Font
(push '(font . "JetBrains Mono 14") default-frame-alist)

;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
