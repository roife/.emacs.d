;;; -*- lexical-binding: t -*-
;;; Mainly for speeding up startup time

;; Defer GC during startup, then restore sane runtime defaults later.
(defvar +gc-cons-threshold (* 32 1024 1024)
  "Default `gc-cons-threshold' after startup.")
(defvar +gc-cons-percentage 0.2
  "Default `gc-cons-percentage' after startup.")

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

(defun +restore-gc-threshold-h ()
  "Restore GC settings after startup."
  (setq gc-cons-threshold +gc-cons-threshold
        gc-cons-percentage +gc-cons-percentage))

(add-hook 'emacs-startup-hook #'+restore-gc-threshold-h 100)

;; Prevent unwanted runtime compilation
(setq native-comp-jit-compilation t)

;; Keep early startup quiet unless we're debugging init.
(setq ad-redefinition-action 'accept
      jka-compr-verbose init-file-debug
      native-comp-async-report-warnings-errors init-file-debug
      native-comp-warning-on-missing-source init-file-debug
      warning-suppress-types '((defvaralias) (lexical-binding))
      warning-inhibit-types '((files missing-lexbind-cookie)))

;; Increase process read size before any package can start subprocesses.
(setq read-process-output-max (* 64 1024))

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
            (unless (daemonp)
              (redraw-frame))))

;; Inhibit package.el initialization
(setq package-enable-at-startup nil)

;; Faster to disable these here (before initialized)
(push '(tab-bar-lines . 1) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)
;; (push '(undecorated-round . t) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
; Set these to nil so users don't have to toggle the modes twice to reactivate.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      tab-bar-mode t)

;; Avoid toolbar setup work during startup. It is unnecessary while the toolbar is
;; disabled, and can be reconstructed if `tool-bar-mode' is enabled later.
(when (fboundp 'tool-bar-setup)
  (advice-add #'tool-bar-setup :override #'ignore))

;; Case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; TTY terminal capability initialization can be expensive. Defer it until the
;; frame is up; terminal packages attached to `tty-setup-hook' will run then.
(unless (or (daemonp) noninteractive init-file-debug initial-window-system)
  (define-advice tty-run-terminal-initialization (:override (&rest _) defer)
    (advice-remove #'tty-run-terminal-initialization
                   #'tty-run-terminal-initialization@defer)
    (add-hook 'window-setup-hook
              (lambda ()
                (tty-run-terminal-initialization (selected-frame) nil t)))))

;; Avoid processing command-line option tables irrelevant to this frame type.
(unless (eq system-type 'darwin)
  (setq command-line-ns-option-alist nil))
(unless (memq initial-window-system '(x pgtk))
  (setq command-line-x-option-alist nil))

;; `setopt' can load custom dependencies early for type checks. Keep it from
;; doing so during init; type checks will still happen when variables are defined.
(when (fboundp 'setopt--set)
  (define-advice setopt--set (:around (fn &rest args) inhibit-load-symbol -90)
    (let ((custom-load-recursion t))
      (apply fn args))))

;; `file-name-handler-alist' is consulted on each call to `require', `load', or various file/io functions
(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (put 'file-name-handler-alist 'initial-value (copy-sequence old-value))
    (define-advice command-line-1 (:around (fn args-left) restore-file-name-handlers)
      (let ((file-name-handler-alist
             (if args-left (copy-sequence old-value) file-name-handler-alist)))
        (funcall fn args-left)))
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (set-default-toplevel-value
                 'file-name-handler-alist
                 (delete-dups
                  (append (default-toplevel-value 'file-name-handler-alist)
                          old-value))))
              101)))

;; TODO: optimize `load-suffixes'

;; Site files will use `load-file', which emit messages and triggers redisplay
;; Make it silent and undo advice later
(define-advice load-file (:override (file) silence)
  (load file nil 'nomessage))
(define-advice startup--load-user-init-file (:after (&rest _) undo-silence)
  (advice-remove #'load-file #'load-file@silence))
