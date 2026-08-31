;;; -*- lexical-binding: t -*-

;; [org-fragtog] Preview and edit latex in md/org elegantly
;; (use-package org-fragtog
;;   :straight t
;;   :hook ((org-mode . org-fragtog-mode)))

;; [org-persist]
(use-package org-persist
  :straight nil
  :init
  (setq org-persist-directory (no-littering-expand-var-file-name "org/persist/")))


;; [org]
(use-package org
  :straight (:type built-in)
  :defer 2
  :init
  (setq org-modules '(org-habit)
        org-directory (file-truename "~/org/"))
  :custom-face
  (org-quote ((t (:inherit org-block-begin-line))))
  :hook ((org-babel-after-execute . org-link-preview-refresh)
         (org-mode . (lambda () (setq-local dabbrev-abbrev-skip-leading-regexp "[=*]"))))
  :config
  (setq
   ;; Keep generated LaTeX previews out of note directories.
   org-preview-latex-image-directory (no-littering-expand-var-file-name "org/latex/")
   ;; Recognize a), A), a., and A. as list markers.
   org-list-allow-alphabetical t
   ;; Always use shift to select
   org-support-shift-select 'always

   ;; subscription: Use {} for sub- or super- scripts
   org-use-sub-superscripts '{}
   org-export-with-sub-superscripts '{}

   ;; prettify
   org-startup-indented t
   ;; Respect #+startup visibility, archived trees, drawers, and hidden blocks.
   org-startup-folded nil
   org-pretty-entities t
   org-ellipsis "…"
   org-hide-emphasis-markers t
   ;; Highlight quote and verse blocks
   org-fontify-quote-and-verse-blocks t
   ;; Highlight the whole line for headings
   org-fontify-whole-heading-line t
   org-image-actual-width nil
   org-priority-faces '((?A . error)
                        (?B . warning)
                        (?C . shadow))

   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-M-RET-may-split-line nil
   org-insert-heading-respect-content t

   ;; Source blocks
   org-src-preserve-indentation t
   org-src-tab-acts-natively t
   org-src-window-setup 'other-window

   ;; Store ID-based attachments centrally and inherit them in subtrees.
   org-attach-id-dir (expand-file-name ".attach/" org-directory)
   org-attach-store-link-p 'attached
   org-attach-use-inheritance t

   ;; Keep numbering opt-out explicit.
   org-num-face '(:inherit org-special-keyword :underline nil :weight bold)
   org-num-skip-tags '("noexport" "nonum")

   ;; better keybindings
   org-special-ctrl-a/e t
   org-special-ctrl-k t
   org-special-ctrl-o t
   org-ctrl-k-protect-subtree 'error
   org-fold-catch-invisible-edits 'show-and-error

   org-imenu-depth 4)

  ;; Common web and local-note link abbreviations.
  (dolist (abbrev '(("github" . "https://github.com/%s")
                    ("youtube" . "https://youtube.com/watch?v=%s")
                    ("google" . "https://google.com/search?q=%s")
                    ("gmap" . "https://maps.google.com/maps?q=%s")
                    ("wiki" . "https://en.wikipedia.org/wiki/%s")
                    ("wolfram" . "https://wolframalpha.com/input/?i=%s")
                    ("org" . (lambda (path) (abbreviate-file-name (expand-file-name path org-directory))))))
    (add-to-list 'org-link-abbrev-alist abbrev))

  ;; Open file links in place, directories in Dired, and flag broken links.
  (setf (alist-get 'file org-link-frame-setup) #'find-file)
  (add-to-list 'org-file-apps '(directory . emacs))
  (add-to-list 'org-file-apps '(remote . emacs))
  (defun +org-file-link-face (path)
    "Use a warning face for a missing local Org file link at PATH."
    (if (or (file-remote-p path)
            (file-exists-p (expand-file-name (org-link-unescape path))))
        'org-link
      '(warning org-link)))
  (org-link-set-parameters "file" :face #'+org-file-link-face)

  (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

  (add-hook! meow-insert-exit-hook
    (defun +org-realign-table-maybe-h ()
      "Realign the Org table at point when its contents changed."
      (when (and (derived-mode-p 'org-mode)
                 org-table-automatic-realign
                 (org-at-table-p)
                 org-table-may-need-update)
        (let ((point (point))
              (inhibit-message t))
          (org-table-align)
          (goto-char point)))))

  ;; Block delimiter faces inherit from `org-meta-line'.
  (dolist (face '(org-meta-line org-block-begin-line org-block-end-line))
    (set-face-attribute face nil :height 0.85))

  ;; Cycle the visible parent heading when point is in or just past folded text.
  (add-hook! org-cycle-tab-first-hook
    (defun +org-cycle-visible-heading ()
      "Cycle the visible parent heading at either edge of a folded subtree."
      (when-let* ((folded-region (or (org-fold-get-region-at-point 'headline)
                                     (and (> (point) (point-min))
                                          (org-fold-get-region-at-point
                                           'headline (1- (point)))))))
        (goto-char (car folded-region))
        (org-back-to-heading t)
        (org-cycle)
        t)))

  ;; Better Org Latex Preview
  (setq org-preview-latex-default-process 'dvisvgm
        org-startup-with-latex-preview nil
        org-highlight-latex-and-related '(latex))
  (plist-put org-format-latex-options :scale 1.7)

  ;; Allow CJK characters and full-width punctuation next to
  ;; emphasis markers.  Keep ASCII letters excluded to avoid
  ;; treating paths and identifiers as emphasis.
  (setq org-emphasis-regexp-components '("-[:space:]('\"{[:nonascii:]"
                                         "-[:space:].,:!?;'\")}\\[[:nonascii:]"
                                         "[:space:]"
                                         "."
                                         1))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))


;; [ob-mermaid] Generate Mermaid diagrams through Org Babel
(use-package ob-mermaid
  :straight t
  :after org
  :init
  (setf (alist-get 'mermaid org-babel-load-languages) t)
  :config
  (setq ob-mermaid-default-config-file
        (no-littering-expand-etc-file-name "mermaid/config.json")))


;; [org-appear] Make invisible parts of Org elements appear visible.
(use-package org-appear
  :straight t
  :hook ((org-mode . org-appear-mode))
  :config
  (setq org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t
        org-appear-inside-latex t
        org-appear-delay 0.1
        org-appear-trigger 'manual)

  (add-hook! org-mode-hook
    (defun +org-appear-meow-integration ()
      (add-hook! meow-insert-enter-hook :local #'org-appear-manual-start)
      (add-hook! meow-insert-exit-hook :local #'org-appear-manual-stop)))
  (when (derived-mode-p 'org-mode) (+org-appear-meow-integration))
  )


(use-package org-modern
  :straight t
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

;; [ox]
(use-package ox
  :config
  (setq org-export-with-smart-quotes t
        org-html-validation-link nil
        org-latex-prefer-user-labels t
        org-export-with-latex t))


;; [org-typst-preview] Render native Typst formulae asynchronously.
(use-package org-typst-preview
  :straight (:host github :repo "roife/org-typst-preview")
  :after org
  :hook (org-mode . org-typst-preview-mode)
  :config
  (setq org-typst-preview-scale 1.5
        org-typst-preview-image-directory
        (no-littering-expand-var-file-name "org/typst/")))
