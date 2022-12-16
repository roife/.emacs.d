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

;; Themes
(use-package spacemacs-theme
  :straight t
  :init (load-theme 'spacemacs-light t)
  )


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

;; [ligature] ligature support for Emacs
(use-package ligature
  :straight t
  :hook ((prog-mode markdown-mode) . ligature-mode)
  :config
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  ;; (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures '(prog-mode markdown-mode)
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                            "\\\\" "://"))
  )


;; [tab-bar] Tab bar
(use-package tab-bar
  :hook (after-init . tab-bar-mode)
  :custom-face
  (tab-bar-tab ((t (:inverse-video t))))
  :config
  (setq tab-bar-separator " "
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-tab-hints t
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-select-tab-modifiers '(super)
        tab-bar-tab-name-truncated-max 15)

  (defun +tab-bar-tab-name-current-with-count-truncated ()
    (let* ((tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
           (count (length (window-list-1 nil 'nomini)))
           (truncated-tab-name (if (< (length tab-name) tab-bar-tab-name-truncated-max)
                                   tab-name
                                 (truncate-string-to-width tab-name
                                                           tab-bar-tab-name-truncated-max
                                                           nil nil tab-bar-tab-name-ellipsis))))
      (if (> count 1)
          (format "%s (%d)" truncated-tab-name count)
        truncated-tab-name)))
  (setq tab-bar-tab-name-function #'+tab-bar-tab-name-current-with-count-truncated)

  (defun +tab-bar-tab-name-format (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat " "
               (when tab-bar-tab-hints (format "%d " i))
               (alist-get 'name tab)
               " ")
       'face (funcall tab-bar-tab-face-function tab))))
  (setq tab-bar-tab-name-format-function #'+tab-bar-tab-name-format)

  (defun +tab-bar-persp-name ()
    (when-let ((name (and (bound-and-true-p persp-mode)
                          (propertize persp-last-persp-name 'face 'font-lock-function-name-face))))
      (format "(%s)" name))
    )

  (setf tab-bar-format '(tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right +tab-bar-persp-name meow-indicator))
  )


(provide 'init-ui)
