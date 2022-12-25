;;; -*- lexical-binding: t -*-

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :bind (:map vertico-map
              ("TAB" . minibuffer-complete)
              ("<tab>" . minibuffer-complete))
  :hook ((after-init . vertico-mode))
  :defines (crm-separator)
  :config
  (setq vertico-cycle t
        vertico-resize nil
        vertico-count 17)

  (advice-add #'completing-read-multiple :filter-args
              (lambda (args)
                (cons (format "[CRM%s] %s"
                              (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""  crm-separator)
                              (car args))
                      (cdr args))))

  ;; WORKAROUND: https://github.com/minad/vertico#problematic-completion-commands
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps t)
  (advice-add #'org-olpath-completing-read :around
              (lambda (&rest args)
                (minibuffer-with-setup-hook
                    (lambda () (setq-local completion-styles '(basic)))
                  (apply args))))
  )


(use-package vertico-directory
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy) ; Cleans up path when moving directories with shadowed paths syntax
  )


(use-package vertico-repeat
  :straight nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save))


(use-package orderless
  :straight t
  :after vertico
  :init (require 'orderless)
  :config

  ;; Dispatchers
  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))

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
        completion-category-overrides '((file (styles +vertico-basic-remote orderless+basic)))
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator "[ &]")
  )


(use-package marginalia
  :straight t
  :hook (vertico-mode . marginalia-mode))


(use-package embark-consult
  :straight t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  )


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
         ([remap locate]                        . consult-locate)
         ([remap load-theme]                    . consult-theme)
         ([remap man]                           . consult-man)
         ([remap recentf-open-files]            . consult-recent-file)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop)
         ("C-c d r" . consult-ripgrep))
  :config
  (setq consult-narrow-key "<")

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
   :preview-key (kbd "s-p"))
  (consult-customize
   consult-theme
   :preview-key (list (kbd "s-p") :debounce 0.6 'any))

  ;; [consult-fd]
  (defvar consult-fd-args "fd --color=never -i -H -E .git --regex ")
  (defun +consult--fd-builder (input)
    (pcase-let* ((cmd (split-string-and-unquote consult-fd-args))
                 (`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler arg 'extended t)))
      (when re
        (list :command (append cmd
                               (list (consult--join-regexps re 'extended))
                               opts)
              :highlight hl))))

  (defun +consult-fd (&optional dir initial)
    "Search for regexp with fd in DIR with INITIAL input.
The find process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search."
    (interactive "P")
    (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
           (default-directory (cdr prompt-dir)))
      (find-file (consult--find (car prompt-dir) #'+consult--fd-builder initial))))
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


(use-package tempel
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :hook (((prog-mode text-mode) . +tempel-setup-capf)
         ((prog-mode text-mode) . tempel-abbrev-mode))
  :config
  (defun +tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (setq tempel-trigger-prefix "<")
)


(use-package tempel-collection
  :straight t)


;; [corfu] compleletion frontend
(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :hook (((prog-mode conf-mode yaml-mode shell-mode eshell-mode) . corfu-mode)
         ((eshell-mode shell-mode) . (lambda () (setq-local corfu-auto nil)))
         (minibuffer-setup . corfu-enable-in-minibuffer))
  :bind (:map corfu-map
              ("H-m" . corfu-move-to-minibuffer))
  :config
  (setq corfu-cycle t                ;; Enable cycling for `corfu-next/previous'
        corfu-auto t                 ;; Enable auto completion
        corfu-separator "&"          ;; Orderless field separator
        corfu-quit-at-boundary nil   ;; Never quit at completion boundary
        corfu-echo-documentation nil ;; Disable documentation in the echo area
        corfu-auto-prefix 2          ;; minimun prefix to enable completion
        corfu-preview-current nil
        corfu-count 15)

  ;; Transfer completion to the minibuffer
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  ;; Completing in the minibuffer
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  )


(use-package corfu-history
  :straight nil
  :after corfu
  :init
  (corfu-history-mode 1)
  :config
  (with-eval-after-load 'savehist
    (cl-pushnew 'corfu-history savehist-additional-variables))
  )


(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :custom-face
  (corfu-popupinfo ((t (:height 1))))
  :init
  (corfu-popupinfo-mode 1)
  :config
  (setq corfu-popupinfo-delay '(0.8 . 0.8)
        corfu-popupinfo-max-height 18
        corfu-popupinfo-max-width 100)
  )


(use-package cape
  :straight t
  :hook ((corfu-mode . +corfu-add-cape-backends)
         ((TeX-mode LaTeX-mode org-mode markdown-mode) . +corfu-add-cape-tex-backends))
  :config
  (defun +corfu-add-cape-backends ()
    (add-to-list 'completion-at-point-functions #'cape-file :append)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev :append))

  (defun +corfu-add-cape-tex-backends ()
    (add-to-list 'completion-at-point-functions #'cape-tex :append))
  )


(use-package dabbrev
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))


(use-package corfu-terminal
  :straight t
  :when (not (display-graphic-p))
  :after corfu
  :init (corfu-terminal-mode 1))


;; TODO: Consult-dash
