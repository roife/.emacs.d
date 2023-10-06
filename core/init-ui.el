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

;; Font: Same width and height for emoji, chinese and english characters
(defvar +font-en-size (if (eq system-type 'darwin) 15 26))
(defvar +font-emoji-size (if (eq system-type 'darwin) 12 22))

(defun +setup-fonts ()
  "Setup fonts."
  (set-face-attribute 'default nil :font (font-spec :family "Sarasa Term SC" :size +font-en-size))

  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji" :size +font-emoji-size))
  (set-fontset-font t 'emoji (font-spec :script 'emoji) nil 'append))

(+setup-fonts)
;; (add-hook 'window-setup-hook #'+setup-fonts)
(add-hook 'server-after-make-frame-hook #'+setup-fonts)

;; Smooth Scroll (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((control) . nil))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))

;; Load theme
(use-package doom-themes
  :straight t)
(defvar +light-theme 'doom-nord-light)
(defvar +dark-theme 'doom-spacegrey)
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

(setq frame-title-format
      '((:eval (or buffer-file-truename "%b"))
        (" · Emacs")))
