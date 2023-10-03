;; -*- lexical-binding: t; -*-

;; Optimization
(setq
 ;; Update UI slowly
 idle-update-delay 1.0

 ;; Inhibits fontification while receiving input, which should help a little with scrolling performance.
 redisplay-skip-fontification-on-input t

 ;; [Selected-window]
 highlight-nonselected-windows nil
 cursor-in-non-selected-windows nil

 ;; Font compacting can be terribly expensive, but may increase memory use
 inhibit-compacting-font-caches t)


;; [Scrolling]
(setq
 ;; Performant and rapid scrolling
 fast-but-imprecise-scrolling t

 ;; Keep 5 lines when scrolling
 scroll-step 0
 scroll-margin 5
 scroll-conservatively 101
 ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll' for tall lines.
 auto-window-vscroll nil

 ;; [hscroll]
 ;; hscroll only for current line
 auto-hscroll-mode t
 hscroll-step 0
 hscroll-margin 2)


;; [Cursor] disable blinking
(blink-cursor-mode -1)


;; [Fringes] Reduce the clutter in the fringes
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)
;; Better fringe symbol
(define-fringe-bitmap 'right-curly-arrow
  [#b00110000
   #b00110000
   #b00000000
   #b00110000
   #b00110000
   #b00000000
   #b00110000
   #b00110000])
(define-fringe-bitmap 'left-curly-arrow
  [#b00110000
   #b00110000
   #b00000000
   #b00110000
   #b00110000
   #b00000000
   #b00110000
   #b00110000])

(define-fringe-bitmap 'right-arrow
  [#b00000000
   #b00000000
   #b00001110
   #b00001110
   #b00001110
   #b00000000
   #b00000000
   #b00000000])
(define-fringe-bitmap 'left-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b01110000
   #b01110000
   #b01110000
   #b00000000
   #b00000000])


;; Allow [resize] by pixels.
(setq frame-resize-pixelwise t)


;; Suppress GUI features for consistency
(setq use-file-dialog nil
      use-dialog-box nil)


;; [Minibuffer]
;; Allow minibuffer commands while in the minibuffer.
(setq enable-recursive-minibuffers t
      echo-keystrokes 0.02)
;; Keep the cursor out of the read-only portions of the minibuffer
(setq minibuffer-prompt-properties '(read-only t
                                               intangible t
                                               cursor-intangible t
                                               face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
;; Allow emacs to query passphrase through minibuffer
(setq epg-pinentry-mode 'loopback)


;; Load theme
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (select-frame-set-input-focus (selected-frame))
            (+load-theme)))


;; Font
(defvar +font-en-size (if (eq system-type 'darwin) 15 25))
(defvar +font-zh-size (if (eq system-type 'darwin) 16 26))
(defvar +font-emoji-size (if (eq system-type 'darwin) 11 16))

(defun +setup-fonts ()
  "Setup fonts."
  (set-face-attribute 'default nil :font (font-spec :family "Iosevka" :size +font-en-size))

  (set-fontset-font t 'han (font-spec :family "PingFang SC" :size +font-zh-size))
  (set-fontset-font t 'han (font-spec :script 'han) nil 'append)

  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji" :size +font-emoji-size))
  (set-fontset-font t 'emoji (font-spec :script 'emoji) nil 'append))

(+setup-fonts)
;; (add-hook 'window-setup-hook #'+setup-fonts)
;; (add-hook 'server-after-make-frame-hook #'+setup-fonts)

;; Smooth Scroll (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((control) . nil))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))

;; Load theme
(defvar +light-theme 'modus-operandi)
(defvar +dark-theme 'modus-vivendi)
(defun +load-theme (&optional theme)
  (unless theme
    (setq theme (if (and (display-graphic-p)
                         (cond ((eq system-type 'darwin)
                                (string= (plist-get (mac-application-state) ':appearance)
                                         "NSAppearanceNameAqua"))
                               (t t)))
                    +light-theme
                  +dark-theme)))
  (unless (member theme custom-enabled-themes)
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(+load-theme)

;; [window-divider] Display window divider
(setq window-divider-default-places t
      window-divider-default-bottom-width 0
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)


;; [ligature] ligature support for Emacs
(use-package ligature
  :straight t
  :hook ((prog-mode markdown-mode) . ligature-mode)
  :config
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures '(prog-mode markdown-mode org-mode)
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
  ;; Turn on tab-bar-mode in early-init to speed-up
  ;; :hook (window-setup . tab-bar-mode)
  :config
  (setq tab-bar-separator " "
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width nil
        tab-bar-close-button-show nil
        tab-bar-tab-hints t)

  (customize-set-variable 'tab-bar-select-tab-modifiers '(super))

  ;; truncate for [tab name] and add count
  (setq tab-bar-tab-name-function
        (lambda () (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
                     (count (length (window-list-1 nil 'nomini)))
                     (truncated-tab-name (if (< (length raw-tab-name)
                                                tab-bar-tab-name-truncated-max)
                                             raw-tab-name
                                           (truncate-string-to-width raw-tab-name
                                                                     tab-bar-tab-name-truncated-max
                                                                     nil nil tab-bar-tab-name-ellipsis))))
                (if (> count 1)
                    (format "%s (%d)" truncated-tab-name count)
                  truncated-tab-name))))

  ;; Add spaces for tab-name
  (setq tab-bar-tab-name-format-function
        (lambda (tab i) (propertize
                    (format " %d %s " i (alist-get 'name tab))
                    'face (funcall tab-bar-tab-face-function tab))))

  ;; cache for persp indicator
  ;; add [persp-name] and [meow-indicator] on tab-bar
  (defvar +tab-bar-persp-indicator-cache nil)
  (defun +tab-bar-update-persp-indicator (&rest _)
    (setq +tab-bar-persp-indicator-cache
          (when-let* ((persp-list (and (bound-and-true-p persp-mode)
                                       (persp-names-current-frame-fast-ordered)))
                      (cur-persp (safe-persp-name (get-current-persp)))
                      (subst-persp-list (cl-substitute (concat "@" cur-persp) cur-persp persp-list :count 1))
                      (persp-list-text (concat " " (string-join subst-persp-list " "))))
            (propertize persp-list-text 'face '(:inherit font-lock-variable-name-face))
            ;; `((persp-indicator
            ;;    menu-item
            ;;    ,propertized-text
            ;;    +tab-bar-persp-menu
            ;;    :help "Perp-mode indicator\nmouse-1: popup menu"))
            )))
  (defun +tab-bar-persp-indicator ()
    (or +tab-bar-persp-indicator-cache (+tab-bar-update-persp-indicator)))

  ;; TODO: KILL
  (dolist (hook '(persp-created-functions
                  persp-renamed-functions
                  persp-activated-functions
                  persp-after-load-state-functions))
    (add-hook hook #'(lambda (&rest _)
                       (+tab-bar-update-persp-indicator)
                       (force-mode-line-update t))))

;;   (defun +tab-bar-persp-menu (event)
;;     "Pop up the same menu as displayed by the menu bar.
;; Used by `tab-bar-format-menu-bar'."
;;     (interactive "e")
;;     (let ((menu (make-sparse-keymap (propertize "Menu Bar" 'hide t))))
;;       (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
;;       (map-keymap (lambda (key binding)
;;                     (when (consp binding)
;;                       (define-key-after menu (vector key)
;;                         (copy-sequence binding))))
;;                   persp-minor-mode-menu)
  ;;       (popup-menu menu event)))


  (setq tab-bar-format '(meow-indicator +tab-bar-persp-indicator tab-bar-format-tabs tab-bar-separator))

  ;; WORKAROUND: fresh tab-bar for daemon
  (when (daemonp)
    (add-hook 'after-make-frame-functions
              #'(lambda (&rest _) (force-mode-line-update))))
  )

(setq frame-title-format
      '((:eval (or buffer-file-truename "%b"))
        (" · Emacs")))
