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


;; [Cursor] disable blinking
(blink-cursor-mode -1)

;; [Fringes] Reduce the clutter in the fringes
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)
;; make left-fringe half
(fringe-mode '(5 . 8))
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
(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

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


;; Font: Same width and height for emoji, chinese and english characters
(defvar +font-en-size (if (eq system-type 'darwin) 15 26))
(defvar +font-emoji-size (if (eq system-type 'darwin) 11 22))

(add-hook! 'server-after-make-frame-hook :call-immediately
  (defun +setup-fonts ()
    "Setup fonts."
    (set-face-attribute 'default nil :font (font-spec :family "Sarasa Term SC" :size +font-en-size))
    (set-face-font 'fixed-pitch "Sarasa Term SC")
    (set-face-font 'fixed-pitch-serif "Sarasa Term Slab SC")
    (set-face-font 'variable-pitch "Sarasa UI SC")

    (set-fontset-font t 'han (font-spec :family "Sarasa Term SC"))
    (set-fontset-font t 'han (font-spec :script 'han) nil 'append)

    (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji" :size +font-emoji-size))
    (set-fontset-font t 'emoji (font-spec :script 'emoji) nil 'append)))

;; Smooth Scroll (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((control) . nil))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))

;; Load theme
(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(defvar +light-theme 'doom-nord-light)
(defvar +dark-theme 'doom-spacegrey)
(defun-call! +load-theme (&optional theme)
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

(setq frame-title-format
      '((:eval (or buffer-file-truename "%b"))
        (" · Emacs")))
