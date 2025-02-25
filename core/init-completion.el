;;; -*- lexical-binding: t -*-

(use-package vertico
  :straight (:files (:defaults "extensions/*.el"))
  :bind (:map vertico-map
              ("TAB" . minibuffer-complete)
              ("<tab>" . minibuffer-complete)
              ("C-<return>" . vertico-exit-input)
              ("C-, ." . vertico-quick-jump))
  :hook ((after-init . vertico-mode))
  :defines (crm-separator)
  :config
  (setq vertico-cycle t
        vertico-resize nil
        vertico-count 15)

  (defadvice! +vertico--set-crm-separator-a (args)
    :filter-args #'completing-read-multiple
    (cons (concat "[CRM"
                  (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                                            crm-separator)
                  "] "
                  (car args))
          (cdr args)))

  ;; WORKAROUND: https://github.com/minad/vertico#problematic-completion-commands
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  )


(use-package vertico-directory
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (;; Cleans up path when moving directories with shadowed paths syntax
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  )


(use-package vertico-repeat
  :straight nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (:map vertico-map
              ("C-r" . vertico-repeat-select)))


(use-package orderless
  :straight t
  :init (require 'orderless)
  :config
  ;; Dispatchers
  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern) `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "^" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "^" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))
     ;; Annotations
     ((string-prefix-p "@" pattern) `(orderless-annotation . ,(substring pattern 1)))
     ((string-suffix-p "@" pattern) `(orderless-annotation . ,(substring pattern 0 -1)))
     ))

  ;; Remote file completion
  (defun +vertico-basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))

  (defun +vertico-basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))

  (add-to-list
   'completion-styles-alist
   '(+vertico-basic-remote
     +vertico-basic-remote-try-completion
     +vertico-basic-remote-all-completions
     "Use basic completion on remote files only"))

  (defun orderless+basic-all (str table pred point)
    (or (orderless-all-completions str table pred point)
        (completion-basic-all-completions str table pred point)))

  (defun orderless+basic-try (str table pred point)
    (or (completion-basic-try-completion str table pred point)
        (orderless-try-completion str table pred point)))

  (add-to-list 'completion-styles-alist
               '(orderless+basic
                 orderless+basic-try
                 orderless+basic-all
                 "Unholy mix of Orderless and Basic."))

  ;; configuration
  (setq completion-styles '(orderless+basic)
        completion-category-defaults nil
        completion-ignore-case t
        ;; despite override in the name, orderless can still be used in find-file etc.
        completion-category-overrides '((file (styles +vertico-basic-remote orderless+basic))
                                        (eglot (styles orderless)))
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator "[ &]")
  )


(use-package marginalia
  :straight t
  :hook (vertico-mode . marginalia-mode)
  :config
  (defun marginalia-annotate-buffer (cand)
    "Annotate buffer CAND with modification status, file name and major mode."
    (when-let ((buffer (get-buffer cand)))
      (if (buffer-live-p buffer)
          (marginalia--fields
           ((marginalia--buffer-status buffer))
           ((marginalia--buffer-file buffer) :face 'marginalia-file-name))
        (marginalia--fields ("(dead buffer)" :face 'error))))))


(use-package embark-consult
  :straight t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))


(use-package embark
  :straight t
  :bind (("C-;" . embark-act)
         ("C-c ; e" . embark-export)
         ("C-c ; c" . embark-collect)
         :map minibuffer-local-map
         ("C-c C-e" . +embark-export-write)
         :map embark-file-map
         ("s" . +reopen-file-with-sudo)
         ("g" . +embark-magit-status))
  :defines (wgrep-change-to-wgrep-mode)
  :init
  (setq prefix-help-command 'embark-prefix-help-command)
  :config
  (defun +embark-magit-status (file)
    "Run `magit-status` on repo containing the embark target."
    (interactive "GFile: ")
    (magit-status (locate-dominating-file file ".git")))

  (defun +embark-export-write ()
    "Export the current vertico results to a writable buffer if possible.
     Supports exporting consult-grep/consult-ripgrep to wgrep, file to wdeired,
     and consult-location to occur-edit"
    (interactive)
    (require 'embark)
    (require 'wgrep)
    (let* ((edit-command
            (pcase-let ((`(,type . ,candidates)
                         (run-hook-with-args-until-success 'embark-candidate-collectors)))
              (pcase type
                ('consult-grep #'wgrep-change-to-wgrep-mode)
                ('consult-ripgrep #'wgrep-change-to-wgrep-mode)
                ('file #'wdired-change-to-wdired-mode)
                ('consult-location #'occur-edit-mode)
                (x (user-error "embark category %S doesn't support writable export" x)))))
           (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
      (embark-export)))
  )


(use-package consult
  :straight t
  :bind (([remap bookmark-jump]                 . consult-bookmark)
         ([remap list-registers]                . consult-register)
         ([remap goto-line]                     . consult-goto-line)
         ([remap imenu]                         . consult-imenu)
         ("C-c i"                               . consult-imenu)
         ("C-c I"                               . consult-imenu-multi)
         ([remap locate]                        . consult-locate)
         ([remap load-theme]                    . consult-theme)
         ([remap man]                           . consult-man)
         ([remap recentf-open-files]            . consult-recent-file)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop)
         ("C-c d r"                             . consult-ripgrep)
         ("C-c d f"                             . consult-fd)
         :map minibuffer-mode-map
         ("C-r" . consult-history))
  ;; :hook ((completion-list-mode . consult-preview-at-point-mode))
  :config
  (setq consult-narrow-key "<"
        consult-async-min-input 2)

  ;; replace multi-occur with consult-multi-occur
  (advice-add #'multi-occur :override #'consult-multi-occur)

  ;; [consult-register] Configure the register formatting.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; [consult-xref] Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; better preview
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file
   consult--source-project-recent-file consult--source-bookmark
   consult-buffer
   :preview-key "s-p")
  (consult-customize
   consult-theme
   :preview-key (list "s-p" :debounce 0.6 'any))
  )


;; [consult-dir] Insert path quickly in minibuffer
(use-package consult-dir
  :straight t
  :bind (([remap list-directory] . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t)
  )

(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
              ("TAB" . company-complete-selection)
              ("<tab>" . company-complete-selection))
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0.1
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 1
        company-icon-margin 3
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode)
        company-backends '((company-capf :with company-yasnippet)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev)
        company-format-margin-function nil
        company-transformers '(company-sort-prefer-same-case-prefix
                               company-sort-by-occurrence
                               company-sort-by-backend-importance))
  )


(use-package yasnippet
  :straight t
  :hook (after-init . yas-global-mode))


(use-package dabbrev
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))


(use-package consult-dash
  :straight t
  :bind (("M-s d" . consult-dash))
  :config
  ;; Use the symbol at point as initial search term
  (consult-customize consult-dash :initial (thing-at-point 'symbol)))
