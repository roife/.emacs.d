;;; -*- lexical-binding: t -*-
;;; Mainly for speeding up startup time

;; Defer GC during startup, then restore sane runtime defaults later.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)
;; Restore when idle, so the first GC doesn't delay the first frame.
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer
             1 nil (lambda ()
                     (setq gc-cons-threshold (* 32 1024 1024)
                           gc-cons-percentage 0.2))))
          100)

;; Keep early startup quiet unless we're debugging init.
(setq ad-redefinition-action 'accept
      jka-compr-verbose init-file-debug
      native-comp-async-report-warnings-errors init-file-debug
      native-comp-warning-on-missing-source init-file-debug
      native-comp-async-on-battery-power nil
      warning-suppress-types '((defvaralias) (lexical-binding))
      warning-inhibit-types '((files missing-lexbind-cookie)))

;; Larger process output buffer (mainly for LSP).
(setq read-process-output-max (* 4 1024 1024))

;; Batch jobs should pick up source files newer than their byte-code.
(setq load-prefer-newer noninteractive)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Inhibit startup screen & message
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
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
            (unless (daemonp)
              (redraw-frame))))

;; Inhibit package.el initialization
(setq package-enable-at-startup nil)

;; Faster to disable these here (before initialized)
;; init-modeline enables the tab bar later to avoid clipping PGTK's echo area.
(push '(tab-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
;; Set these to nil so users don't have to toggle the modes twice to reactivate.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      tab-bar-mode nil)

;; Case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; Avoid processing command-line option tables irrelevant to this frame type.
(unless (eq system-type 'darwin)
  (setq command-line-ns-option-alist nil))
(unless (memq initial-window-system '(x pgtk))
  (setq command-line-x-option-alist nil))
