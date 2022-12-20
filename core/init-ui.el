;; -*- lexical-binding: t; -*-

;; Optimization
(setq idle-update-delay 1.0
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      highlight-nonselected-windows nil
      cursor-in-non-selected-windows nil)

;; disable cursor blinking
(blink-cursor-mode -1)

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      ; Font compacting can be terribly expensive
      inhibit-compacting-font-caches t)

;; Load theme
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (select-frame-set-input-focus (selected-frame))
            (+load-theme)))

;; Smooth Scroll (less "jumpy" than defaults)
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((control) . nil))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))


;; [window-divider] Display window divider
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

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
  :hook (window-setup . tab-bar-mode)
  :custom
  (tab-bar-separator " ")
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-tab-name-truncated-max 20)
  ;; :custom-face
  ;; (tab-bar-tab ((t (:inverse-video t))))
  :config
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
    (propertize
     (concat " "
             (number-to-string i)
             " "
             (alist-get 'name tab)
             " ")
     'face (funcall tab-bar-tab-face-function tab))
    )
  (setq tab-bar-tab-name-format-function #'+tab-bar-tab-name-format)

  (defun +tab-bar-persp-name ()
    (when-let ((name (and (bound-and-true-p persp-mode)
                          (propertize persp-last-persp-name 'face 'font-lock-function-name-face)))
               (count (length persp-names-cache)))
      (if (> count 1)
          (format "[%s/%d] " name count)
        (concat "[" name "] "))))

  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right +tab-bar-persp-name meow-indicator))

  (when (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame) (tab-bar--update-tab-bar-lines (list frame)))))
  )


(provide 'init-ui)
