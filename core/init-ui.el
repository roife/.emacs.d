;;; -*- lexical-binding: t -*-

;; Optimization
(setq idle-update-delay 1.0
      highlight-nonselected-windows nil
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t)

;; disable cursor blinking
(blink-cursor-mode -1)

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil)

(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Smooth Scroll (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((control) . nil))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))

;; Better fringe symbol
(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b11110000
   #b11110000
   #b00110000
   #b00110000])
(define-fringe-bitmap 'left-curly-arrow
  [#b00001100
   #b00001100
   #b00001111
   #b00001111
   #b00000000
   #b00000000
   #b00000000
   #b00000000])

;; Font
(push '(font . "JetBrains Mono 11") default-frame-alist)

;; Themes
;; (use-package one-themes
;;   :straight t
;;   :init (load-theme 'one-light t))


;; [window-divider] Display window divider
(use-package window-divider
  :hook (window-setup . window-divider-mode)
  :init
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1))

;; [iscroll] Smooth scrolling over images
(use-package iscroll
  :straight t
  :hook (image-mode . iscroll-mode))

;; [mixed-pitch] Use monospace in some occasions
(use-package mixed-pitch
  :straight t
  :hook (text-mode . mixed-pitch-mode))

(provide 'init-ui)
