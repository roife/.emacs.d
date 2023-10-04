;;; -*- lexical-binding: t -*-

(use-package keyfreq
  :straight t
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-command
          forward-char
          backward-char
          left-char
          right-char
          mac-mwheel-scroll
          puni-kill-line
          undo
          save-buffer
          newline
          +copilot-complete
          execute-extended-command
          previous-line
          next-line
          er/expand-region
          meow-right
          meow-next
          find-file
          vertico-next
          +puni-hungry-delete
          minibuffer-complete
          meow-prev
          meow-left
          symbol-overlay-put
          meow-insert
          meow-insert-exit
          )))
