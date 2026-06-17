;;; -*- lexical-binding: t -*-

;; Xterm-compatible terminal support.  Keep actual setup gated by TTY frames so
;; GUI frames are unaffected even when this file is loaded by a daemon.

(require 'term/xterm)

;; Xterm-compatible terminal support.  Keep actual setup gated by TTY frames so
;; GUI frames are unaffected even when this file is loaded by a daemon.
(setq xterm-extra-capabilities '(modifyOtherKeys reportBackground
                                                 getSelection setSelection)
      xterm-set-window-title t)

;; [Kitty Graphics Protocol] Implements support for Kitty's "graphics protocol",
;; which allows the terminal to display images and videos inline.
;;(use-package kitty-graphics
;;  :straight (:type git :host github :repo "cashmeredev/kitty-graphics.el")
;;  :hook (tty-setup . kitty-graphics-setup)
;;  :init
;;  (setq kitty-gfx-enable-video t))


;; [Kitty Keyboard Protocol] Implements support for Kitty's "keyboard protocol",
;; which allows the terminal to send key events to Emacs.
(use-package kkp
  :straight t
  :hook (tty-setup . global-kkp-mode))

;;(defun +xterm-read-csi-question-response ()
;;  "Read the rest of a CSI ? terminal response."
;;  (let (chars char)
;;    (while (and (setq char (xterm--read-event-for-query))
;;                (or (not (integerp char))
;;                    (< char #x40)
;;                    (> char #x7e)))
;;      (push char chars))
;;    (when char
;;      (push char chars))
;;    (apply #'string (nreverse chars))))
;;
;;(defun +xterm-apply-theme-mode (mode)
;;  "Apply terminal theme MODE from a CSI 997 notification."
;;  (set-terminal-parameter (frame-terminal (selected-frame)) 'background-mode mode)
;;  (frame-set-background-mode (selected-frame))
;;  (+load-theme)
;;  [])
;;
;;(defun +xterm-init-theme-update-notifications ()
;;  "Enable terminal theme update notifications."
;;  (define-key input-decode-map "\e[?997;1n" #'(lambda (_) (+xterm-apply-theme-mode 'dark)))
;;  (define-key input-decode-map "\e[?997;2n" #'(lambda (_) (+xterm-apply-theme-mode 'light)))
;;  (unless (member "\e[?2031l" (terminal-parameter nil 'tty-mode-reset-strings))
;;    (send-string-to-terminal "\e[?2031h")
;;    (send-string-to-terminal "\e[?996n")
;;    (push "\e[?2031l" (terminal-parameter nil 'tty-mode-reset-strings))
;;    (push "\e[?2031h" (terminal-parameter nil 'tty-mode-set-strings))))
;;
(add-hook! tty-setup-hook
 (defun +xterm-setup-frame ()
   "Enable xterm niceties for the current terminal frame."
   (xterm-mouse-mode 1)
   ;; (let ((frame (selected-frame)))
   ;;   (when (and (not (display-graphic-p frame))
   ;;              (eq (terminal-parameter (frame-terminal frame) 'terminal-initted)
   ;;                  'terminal-init-xterm))
   ;;     (with-selected-frame frame
   ;;       ;; (+xterm-init-theme-update-notifications)
   ;;       )))
   ))
