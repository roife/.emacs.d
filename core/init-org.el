;;; -*- lexical-binding: t -*-

;; [org-fragtog] Preview and edit latex in md/org elegantly
;; (use-package org-fragtog
;;   :straight t
;;   :hook ((org-mode . org-fragtog-mode)))

;; [org]
(use-package org
  ;; :straight (:type built-in)
  :custom-face (org-quote ((t (:inherit org-block-begin-line))))
  :hook ((org-mode . (lambda () (setq-local dabbrev-abbrev-skip-leading-regexp "[=*]")))  ;; Skipping leading char, so corfu can complete with dabbrev for formatted text
         (org-mode . (lambda ()
                       (push '("\\operatorname{\\mathrm{" . (?  (Bc . Bl) ?{ (Bc . Br) ?{)) prettify-symbols-alist)
                       (push '("\\mathcal{" . (?  (Bc . Bl) ?{ (Bc . Br) ?𝒞)) prettify-symbols-alist)
                       (push '("\\mathbb{" . (?  (Bc . Bl) ?{ (Bc . Br) ?𝔹)) prettify-symbols-alist)
                       (push '("\\\\{" . ?{) prettify-symbols-alist)
                       (push '("\\\\}" . ?}) prettify-symbols-alist)
                       (push '("\\vec{" . (?  (Bc . Bl) ?{ (Bc . Br) ?⃗)) prettify-symbols-alist)
                       (push '("\\ " . ?‿) prettify-symbols-alist)
                   ;;     (push '("\\(" . ?‹) prettify-symbols-alist)
                   ;;           (push '("\\)" . ?›) prettify-symbols-alist)
                   ;;     (push '("\\)，" . (?  (Bc . Bl) ?， (Bc . Br) ?›)) prettify-symbols-alist)
                   ;; (push '("\\)。" . (?  (Bc . Bl) ?。 (Bc . Br) ?›)) prettify-symbols-alist)
                   ;; (push '("\\)；" . (?  (Bc . Bl) ?； (Bc . Br) ?›)) prettify-symbols-alist)
                   ;; (push '("\\[" . ?«) prettify-symbols-alist)
                   ;; (push '("\\]" . ?») prettify-symbols-alist)
                       (prettify-symbols-mode))))
  :config
  (setq
   ;; subscription: Use {} for sub- or super- scripts
   org-use-sub-superscripts '{}
   org-export-with-sub-superscripts '{}

   ;; prettify
   org-startup-indented t
   org-pretty-entities t
   org-ellipsis "…"
   ;; Highlight quote and verse blocks
   org-fontify-quote-and-verse-blocks t
   ;; Highlight the whole line for headings
   org-fontify-whole-heading-line t

   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-insert-heading-respect-content t

   ;; better keybindings
   org-special-ctrl-a/e t
   org-special-ctrl-k t
   org-special-ctrl-o t
   org-support-shift-select t
   org-ctrl-k-protect-subtree 'error
   org-fold-catch-invisible-edits 'show-and-error

   org-imenu-depth 4)

  ;; Better Org Latex Preview
  (setq org-latex-create-formula-image-program 'dvisvgm
        org-startup-with-latex-preview nil
        org-highlight-latex-and-related '(latex))
  (plist-put org-format-latex-options :scale 1.7)

  ;; HACK: inline highlight for CJK
  (setq org-emphasis-regexp-components '("-[:space:]('\"{[:nonascii:][:alpha:]"
                                         "-[:space:].,:!?;'\")}\\[[:nonascii:][:alpha:]"
                                         "[:space:]"
                                         "."
                                         1))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)
  (org-element--set-regexps)
  )


;; [org-entities]
(use-package org-entities
  :config
  (setq org-entities-user
        '(("vdash" "\\vdash" t "⊢" "⊢" "⊢" "⊢")
          ("vDash" "\\vDash" t "⊨" "⊨" "⊨" "⊨")
          ("Vdash" "\\Vdash" t "⊩" "⊩" "⊩" "⊩")
          ("Vvdash" "\\Vvdash" t "⊪" "⊪" "⊪" "⊪")
          ("nvdash" "\\nvdash" t "⊬" "⊬" "⊬" "⊬")
          ("nvDash" "\\nvDash" t "⊭" "⊭" "⊭" "⊭")
          ("nVdash" "\\nVdash" t "⊮" "⊮" "⊮" "⊮")
          ("nVDash" "\\nVDash" t "⊯" "⊯" "⊯" "⊯")
          ("subseteq" "\\subseteq" t "⊆" "⊆" "⊆" "⊆")
          ("supseteq" "\\supseteq" t "⊇" "⊇" "⊇" "⊇")
          ("subsetneq" "\\subsetneq" t "⊊" "⊊" "⊊" "⊊")
          ("supsetneq" "\\supsetneq" t "⊋" "⊋" "⊋" "⊋")
          ("nsubseteq" "\\nsubseteq" t "⊈" "⊈" "⊈" "⊈")
          ("nsupseteq" "\\nsupseteq" t "⊉" "⊉" "⊉" "⊉")
          ("nsubseteqq" "\\nsubseteqq" t "⊈" "⊈" "⊈" "⊈")
          ("nsupseteqq" "\\nsupseteqq" t "⊉" "⊉" "⊉" "⊉")
          ("subsetneqq" "\\subsetneqq" t "⊊" "⊊" "⊊" "⊊")
          ("supsetneqq" "\\supsetneqq" t "⊋" "⊋" "⊋" "⊋")
          ("nsubset" "\\nsubset" t "⊄" "⊄" "⊄" "⊄")
          ("nsupset" "\\nsupset" t "⊅" "⊅" "⊅" "⊅")
          ("nsubseteq" "\\nsubseteq" t "⊈" "⊈" "⊈" "⊈")
          ("nsupseteq" "\\nsupseteq" t "⊉" "⊉" "⊉" "⊉"))))


;; [org-appear] Make invisible parts of Org elements appear visible.
(use-package org-appear
  :straight t
  :hook ((org-mode . org-appear-mode))
  :config
  (setq
   org-hide-emphasis-markers t

   org-appear-autosubmarkers t
   org-appear-autoentities t
   org-appear-autokeywords t
   org-appear-inside-latex t

   org-appear-delay 0.1

   org-appear-trigger 'manual)

  (add-hook! org-mode-hook :call-immediately
    (defun +org-add-appear-hook ()
      (add-hook 'meow-insert-enter-hook #'org-appear-manual-start nil t)
      (add-hook 'meow-insert-exit-hook #'org-appear-manual-stop nil t))))


;; [org-pomodoro] Pomodoro timer for org-mode
(use-package org-pomodoro
  :straight t
  :after org
  :config
  (setq org-pomodoro-play-sounds nil))


;; [org-modern] A modern org-mode
(use-package org-modern
  :straight t
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda-mode)))

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))


;; [ox]
(use-package ox
  :config
  (setq org-export-with-smart-quotes t
        org-html-validation-link nil
        org-latex-prefer-user-labels t
        org-export-with-latex t))
