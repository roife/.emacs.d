;;; -*- lexical-binding: t -*-

;; Xterm-compatible terminal support.  Keep actual setup gated by TTY frames so
;; GUI frames are unaffected even when this file is loaded by a daemon.

(require 'term/xterm)

(setq xterm-extra-capabilities '(modifyOtherKeys reportBackground
                                                 getSelection setSelection)
      xterm-set-window-title t)

(defun +xterm-read-csi-question-response ()
  "Read the rest of a CSI ? terminal response."
  (let (chars char)
    (while (and (setq char (xterm--read-event-for-query))
                (or (not (integerp char))
                    (< char #x40)
                    (> char #x7e)))
      (push char chars))
    (when char
      (push char chars))
    (apply #'string (nreverse chars))))

(defun +xterm-apply-theme-mode (mode)
  "Apply terminal theme MODE from a CSI 997 notification."
  (set-terminal-parameter (frame-terminal (selected-frame)) 'background-mode mode)
  (frame-set-background-mode (selected-frame))
  (+load-theme))

(defun +xterm-csi-question-handler (&optional _prompt)
  "Handle CSI ? terminal theme update reports."
  (let ((response (+xterm-read-csi-question-response)))
    (pcase response
      ("997;1n" (+xterm-apply-theme-mode 'dark))
      ("997;2n" (+xterm-apply-theme-mode 'light)))
    []))

(defun +xterm-init-theme-update-notifications ()
  "Enable terminal theme update notifications."
  (define-key input-decode-map "\e[?" #'+xterm-csi-question-handler)
  (unless (member "\e[?2031l" (terminal-parameter nil 'tty-mode-reset-strings))
    (send-string-to-terminal "\e[?2031h")
    (send-string-to-terminal "\e[?996n")
    (push "\e[?2031l" (terminal-parameter nil 'tty-mode-reset-strings))
    (push "\e[?2031h" (terminal-parameter nil 'tty-mode-set-strings))))

(add-hook! tty-setup-hook :call-immediately
  (defun +xterm-setup-frame ()
    "Enable xterm niceties for the current terminal frame."
    (let ((frame (selected-frame)))
      (when (and (not (display-graphic-p frame))
                 (eq (terminal-parameter (frame-terminal frame) 'terminal-initted)
                     'terminal-init-xterm))
        (with-selected-frame frame
          (+xterm-init-theme-update-notifications)
          (xterm-mouse-mode 1))))))
