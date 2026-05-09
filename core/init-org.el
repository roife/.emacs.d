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
;; (use-package org-appear
;;   :straight t
;;   :hook ((org-mode . org-appear-mode))
;;   :config
;;   (setq
;;    org-hide-emphasis-markers t
;;
;;    org-appear-autosubmarkers t
;;    org-appear-autoentities t
;;    org-appear-autokeywords t
;;    org-appear-inside-latex t
;;
;;    org-appear-delay 0.1
;;
;;    org-appear-trigger 'manual)
;;
;;   (add-hook! org-mode-hook :call-immediately
;;     (defun +org-add-appear-hook ()
;;       (add-hook 'meow-insert-enter-hook #'org-appear-manual-start nil t)
;;       (add-hook 'meow-insert-exit-hook #'org-appear-manual-stop nil t))))


;; [org-visual-outline] Add guide lines for org outline
(use-package org-visual-outline
  :after org
  :no-require t
  :straight (:host github :repo "legalnonsense/org-visual-outline" :files ("*.el"))
  :init
  (use-package org-visual-indent
    :after org
    :hook ((org-mode . org-visual-indent-mode))
    :config
    ;; HACK: `org-visual-indent' calculates its faces from the current theme,
    ;; but it could be wrong after switching theme.
    (add-hook! enable-theme-functions :call-immediately
      (defun +org-visual-outline-indent-color-update (&rest _)
        (let (bufs)
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (when org-visual-indent-mode
                (push buf bufs)
                (org-visual-indent-mode -1))))
          (setq org-visual-indent-color-indent
                (cl-loop for x from 1 to 8
                         with color = nil
                         do (setq color (or (face-foreground (intern (concat "org-level-" (number-to-string x))) nil t)
                                            (face-foreground 'org-level-1)))
                         collect `(,x ,(list :background color :foreground color :height .1))))
          (set-face-attribute 'org-visual-indent-pipe-face nil
                              :foreground (face-attribute 'default :foreground)
                              :background (face-attribute 'default :foreground))
          (set-face-attribute 'org-visual-indent-blank-pipe-face nil
                              :foreground (face-attribute 'default :background)
                              :background (face-attribute 'default :background))
          (dolist (buf bufs)
            (with-current-buffer buf
              (org-visual-indent-mode t))))
        ))
    ))


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


;; [ox]
(use-package ox
  :config
  (setq org-export-with-smart-quotes t
        org-html-validation-link nil
        org-latex-prefer-user-labels t
        org-export-with-latex t))


;; [ox-hugo]
(use-package ox-hugo
  :straight t
  :after ox
  :init (require 'ox-hugo)
  :config
  (setq org-hugo-section "posts"
        org-hugo-base-dir +blog-dir
        org-hugo-external-file-extensions-allowed-for-copying nil)

  (defun +ox-hugo/export-all (&optional org-files-root-dir dont-recurse)
    "Export all Org files (including nested) under ORG-FILES-ROOT-DIR.

All valid post subtrees in all Org files are exported using
`org-hugo-export-wim-to-md'.

If optional arg ORG-FILES-ROOT-DIR is nil, all Org files in
current buffer's directory are exported.

If optional arg DONT-RECURSE is nil, all Org files in
ORG-FILES-ROOT-DIR in all subdirectories are exported. Else, only
the Org files directly present in the current directory are
exported.  If this function is called interactively with
\\[universal-argument] prefix, DONT-RECURSE is set to non-nil.

Example usage in Emacs Lisp: (ox-hugo/export-all \"~/org\")."
    (interactive)
    (let* ((org-files-root-dir (or org-files-root-dir default-directory))
           (dont-recurse (or dont-recurse (and current-prefix-arg t)))
           (search-path (file-name-as-directory (expand-file-name org-files-root-dir)))
           (org-files (if dont-recurse
                          (directory-files search-path :full "\.org$")
                        (directory-files-recursively search-path "\.org$"))))
      (if (null org-files)
          (message "No Org files found in %s" search-path)
        (let ((cnt 1)
              (num-files (length org-files)))
          (dolist (org-file org-files)
            (with-current-buffer (find-file-noselect org-file)
              (message "[ox-hugo/export-all file %d/%d] Exporting %s" cnt num-files org-file)
              (org-hugo-export-wim-to-md :all-subtrees)
              (cl-incf cnt)))))))
  )
