;;; -*- lexical-binding: t -*-

;; [Kitty Graphics Protocol] Implements support for Kitty's "graphics protocol",
;; which allows the terminal to display images and videos inline.
(use-package kitty-graphics
 :straight (:type git :host github :repo "cashmeredev/kitty-graphics.el")
 :hook (tty-setup . kitty-graphics-setup)
 :init
 (setq kitty-gfx-enable-video t))


;; [Kitty Keyboard Protocol] Implements support for Kitty's "keyboard protocol",
;; which allows the terminal to send key events to Emacs.
(use-package kkp
  :straight t
  :hook (tty-setup . global-kkp-mode))

(use-package term/xterm
  :straight nil
  :hook (tty-setup . xterm-mouse-mode)
  :init
  (setq xterm-extra-capabilities '(modifyOtherKeys reportBackground
                                                   getSelection setSelection)
        xterm-update-cursor t
        xterm-set-window-title t))
