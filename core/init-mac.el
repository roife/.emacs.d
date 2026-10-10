;;; -*- lexical-binding: t -*-

(add-hook 'ns-system-appearance-change-functions #'+system-appearance-changed)

(unbind-key "s-,")
(bind-keys ("s-a" . mark-whole-buffer)
           ("s-x" . kill-region)
           ("s-s" . save-buffer)
           ("s-v" . yank)
           ("s-c" . copy-region-as-kill)
           ("s-z" . undo)
           ("s-Z" . undo-redo)
           ("s-f" . isearch-forward)
           ("s-w" . tab-close)
           ("s-t" . tab-new)
           ("s-o" . other-window))
