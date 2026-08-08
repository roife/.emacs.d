;;; -*- lexical-binding: t -*-

;;; Minibuffer

(use-package vertico
  :straight (:files (:defaults "extensions/*.el"))
  :bind (:map vertico-map
              ("TAB" . minibuffer-complete)
              ("<tab>" . minibuffer-complete)
              ("C-<return>" . vertico-exit-input)
              ("C-, ." . vertico-quick-jump))
  :hook ((after-init . vertico-mode))
  :config
  (setq vertico-cycle t
        vertico-count 15)

  (vertico-mouse-mode)

  ;; WORKAROUND: https://github.com/minad/vertico#problematic-completion-commands
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil))


(use-package vertico-directory
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Cleans up path when moving directories with shadowed paths syntax.
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


(use-package vertico-quick
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("C-," . vertico-quick-jump)))


(use-package vertico-buffer
  :straight nil
  :after vertico
  :hook (vertico-mode . vertico-buffer-mode)
  :config
  (setq vertico-buffer-display-action '(display-buffer-in-direction
                                        (direction . below)
                                        (window-height . 0.5)))
  (defadvice! +vertico-buffer-disbale-mode-line ()
    :before #'vertico-buffer--setup
    (setq-local mode-line-format nil)))


;;; Matching styles

(use-package orderless
  :straight t
  :init
  ;; Component modifiers:
  ;;   !foo excludes, =foo matches literally, ~foo uses flex,
  ;;   ^foo matches a literal prefix, ,foo uses initialism,
  ;;   %foo enables char-folding, @foo matches annotations.
  (defun +orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern) `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ((string= "!" pattern) `(orderless-literal . ""))
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ((string-prefix-p "^" pattern) `(orderless-literal-prefix . ,(substring pattern 1)))
     ((string-suffix-p "^" pattern) `(orderless-literal-prefix . ,(substring pattern 0 -1)))
     ((string-prefix-p "," pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "," pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))
     ((string-prefix-p "@" pattern) `(orderless-annotation . ,(substring pattern 1)))
     ((string-suffix-p "@" pattern) `(orderless-annotation . ,(substring pattern 0 -1)))))

  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-ignore-case t
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-category-overrides '((file (styles basic partial-completion))
                                        (eglot (styles orderless))
                                        (eglot-capf (styles orderless)))
        orderless-style-dispatchers '(+orderless-dispatch)
        orderless-component-separator #'orderless-escapable-split
        completions-sort 'historical
        completion-pcm-leading-wildcard t))


(use-package marginalia
  :straight t
  :hook (vertico-mode . marginalia-mode))


;;; Actions and search commands

(use-package embark
  :straight t
  :bind (("C-;" . embark-act)
         ("M-;" . embark-dwim)
         ("C-h E" . embark-bindings)
         :map embark-file-map
         ("s" . sudo-edit)
         ("g" . +embark-magit-status))
  :init
  (setq prefix-help-command 'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (defun +embark-magit-status (file)
    "Run `magit-status` on repo containing the embark target."
    (interactive "GFile: ")
    (magit-status (locate-dominating-file file ".git"))))


(use-package embark-consult
  :straight t
  :after (embark consult))


(use-package consult
  :straight t
  :bind (([remap bookmark-jump]                 . consult-bookmark)
         ([remap list-registers]                . consult-register)
         ([remap goto-line]                     . consult-goto-line)
         ([remap imenu]                         . consult-imenu)
         ("C-c d i"                             . consult-imenu)
         ("C-c d I"                             . consult-imenu-multi)
         ([remap locate]                        . consult-locate)
         ([remap load-theme]                    . consult-theme)
         ([remap man]                           . consult-man)
         ([remap recentf-open-files]            . consult-recent-file)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop)
         ("C-c d l"                             . consult-line)
         ("C-c d r"                             . consult-ripgrep)
         ("C-c d f"                             . consult-fd)
         :map minibuffer-mode-map
         ("C-r"                                 . consult-history))
  :config
  (setq consult-narrow-key "<"
        consult-async-min-input 1
        consult-async-refresh-delay 0.05)

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
   consult-buffer
   :preview-key "s-p")
  (consult-customize
   consult-theme
   :preview-key (list "s-p" :debounce 0.6 'any))

  (defvar +consult-fd-dwim (consult--fd-make-builder
                            (list (or (consult--project-root)
                                      (expand-file-name default-directory)))))

  (defvar +consult-source-fd
    (list :name     "Find"
          :narrow   ?f
          :category 'file
          :face     'consult-file
          :history  'file-name-history
          :action   #'consult--file-action
          :state    #'consult--file-preview
          :async (lambda (sink)
                   (let ((consult-fd-args '("fd" "--full-path" "--color=never" "--hidden" "--follow" "--exclude .git")))
                     (funcall
                      (consult--process-collection +consult-fd-dwim
                        :transform (consult--async-map #'abbreviate-file-name)
                        :highlight t
                        :file-handler t)
                      sink)))))
  (add-to-list 'consult-buffer-sources '+consult-source-fd 'append))


(use-package avy-embark-collect
  :straight (:host github :repo "oantolin/embark"
                   :files ("avy-embark-collect.el"))
  :after (embark avy)
  :bind (:map embark-collect-mode-map
              ("j" . avy-embark-collect-choose)
              ("J" . avy-embark-collect-act)))


;; [consult-dir] Insert path quickly in minibuffer
(use-package consult-dir
  :straight t
  :bind (([remap list-directory] . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t))


;;; In-buffer completion

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :hook (((prog-mode conf-mode yaml-mode shell-mode eshell-mode text-mode codex-ide-session-mode) . corfu-mode)
         ((eshell-mode shell-mode) . (lambda () (setq-local corfu-auto nil)))
         (minibuffer-setup . +corfu-enable-in-minibuffer))
  :bind (:map corfu-map
              ("TAB" . corfu-complete)
              ("<tab>" . corfu-complete)
              ("S-TAB" . +corfu-move-to-minibuffer)
              ("S-<tab>" . +corfu-move-to-minibuffer)
              ("RET" . nil))
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-prefix 2
        corfu-preselect 'first
        corfu-preview-current nil
        corfu-auto-delay 0.1)

  (defun +corfu-move-to-minibuffer ()
    "Use Consult's minibuffer UI for the current completion-in-region table."
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (add-to-list 'corfu-continue-commands #'+corfu-move-to-minibuffer)

  (defun +corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1))))

(use-package corfu-history
  :straight nil
  :after corfu
  :config
  (corfu-history-mode 1)
  (with-eval-after-load 'savehist
    (cl-pushnew 'corfu-history savehist-additional-variables)))

(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :config
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay '(1.0 . 1.0)))

(use-package corfu-quick
  :straight nil
  :after corfu
  :bind (:map corfu-map
              ("C-, ," . corfu-quick-complete)))


(use-package cape
  :straight t
  :hook (((TeX-mode LaTeX-mode org-mode markdown-ts-mode) . +completion-add-tex-capfs))
  :init
  (setq cape-dabbrev-buffer-function #'buffer-list)

  (defun +completion-add-capfs (&rest capfs)
    "Append CAPFS to the buffer-local `completion-at-point-functions'."
    (dolist (capf capfs)
      (unless (memq capf completion-at-point-functions)
        (setq-local completion-at-point-functions
                    (append completion-at-point-functions (list capf))))))

  (setq-default completion-at-point-functions
                (append completion-at-point-functions (list #'cape-file #'cape-dabbrev)))

  (defun +completion-add-tex-capfs ()
    (+completion-add-capfs #'cape-tex)))


;;; Snippets

(use-package tempel
  :straight t
  :bind (:map tempel-map
              ("TAB" . tempel-next)
              ("<tab>" . tempel-next)
              ("S-<tab>" . tempel-previous)
              ("<backtab>" . tempel-previous))
  :hook (((prog-mode text-mode conf-mode) . +tempel-setup-capf)
         ((prog-mode text-mode) . tempel-abbrev-mode))
  :init
  (defvar +tempel-trigger-capf nil)

  (defun +tempel-setup-capf ()
    (unless +tempel-trigger-capf
      (setq +tempel-trigger-capf (cape-capf-trigger #'tempel-complete ?/)))
    (unless (memq +tempel-trigger-capf completion-at-point-functions)
      (setq-local completion-at-point-functions
                  (cons +tempel-trigger-capf completion-at-point-functions))))
  :config
  (setq tempel-path (expand-file-name "tempel-templates" user-emacs-directory)))


(use-package tempel-collection
  :straight t
  :after tempel)


(use-package dabbrev
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))
