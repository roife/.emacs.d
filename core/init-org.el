;;; -*- lexical-binding: t -*-

;; [org-fragtog] Preview and edit latex in md/org elegantly
;; (use-package org-fragtog
;;   :straight t
;;   :hook ((org-mode . org-fragtog-mode)))

(defvar +org-agenda-directory nil
  "Directory containing files used by Org Agenda.")

(defvar +org-refile-files nil
  "Org files that contain GTD refile destinations.")

;; [org]
(use-package org
  :straight (:type built-in)
  :init
  ;; Load optional Org modules only when explicitly enabled.
  (setq org-modules nil)
  :bind (("C-c a" . org-agenda)
         ("C-c n c" . org-capture))
  :custom-face
  (org-quote ((t (:inherit org-block-begin-line))))
  :hook ((org-mode . (lambda () (setq-local dabbrev-abbrev-skip-leading-regexp "[=*]")))  ;; Skipping leading char, so corfu can complete with dabbrev for formatted text
         (org-mode . (lambda ()
                       (push '("\\operatorname{\\mathrm{" . (?  (Bc . Bl) ?{ (Bc . Br) ?{)) prettify-symbols-alist)
                       (push '("\\mathcal{" . (?  (Bc . Bl) ?{ (Bc . Br) ?𝒞)) prettify-symbols-alist)
                       (push '("\\mathbb{" . (?  (Bc . Bl) ?{ (Bc . Br) ?𝔹)) prettify-symbols-alist)
                       (push '("\\\\{" . ?{) prettify-symbols-alist)
                       (push '("\\\\}" . ?}) prettify-symbols-alist)
                       (push '("\\vec{" . (?  (Bc . Bl) ?{ (Bc . Br) ?⃗)) prettify-symbols-alist)
                       (push '("\\ " . ?‿) prettify-symbols-alist)
                       (prettify-symbols-mode))))
  :config
  (setq
   org-directory (file-truename "~/org/")
   +org-agenda-directory (expand-file-name "agenda/" org-directory)
   +org-refile-files (mapcar (lambda (file) (expand-file-name file +org-agenda-directory))
                             '("actions.org" "projects.org" "someday.org"))
   org-default-notes-file (expand-file-name "inbox.org" +org-agenda-directory)

   ;; Task workflow
   org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "SOMEDAY(s)" "|" "DONE(d!)" "CANCELED(c@)"))
   org-log-done 'time
   org-log-into-drawer t
   org-log-reschedule 'time
   org-log-redeadline 'time
   org-tag-alist '((:startgroup)
                   ("@home" . ?h)
                   ("@work" . ?w)
                   (:endgroup)
                   ("project" . ?p)
                   ("note" . ?n))

   ;; Refile clarified inbox items into an action list or a project.
   org-refile-targets '((+org-refile-files :maxlevel . 3))
   org-refile-use-outline-path 'file
   org-outline-path-complete-in-steps nil
   org-refile-allow-creating-parent-nodes 'confirm
   org-archive-location "%s_archive::* Archived"

   ;; Agenda: all Org files directly under ~/org/agenda/ are included.
   org-agenda-files (list +org-agenda-directory)
   org-agenda-span 7
   org-agenda-start-on-weekday 1
   org-agenda-window-setup 'current-window
   org-agenda-restore-windows-after-quit t
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-tags-column 0
   org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                              (todo . " %i %-12:c")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c"))
   org-agenda-custom-commands '(("d" "Dashboard"
                                 ((agenda ""
                                          ((org-agenda-overriding-header "This week")
                                           (org-agenda-span 7)))
                                  (todo "NEXT"
                                        ((org-agenda-overriding-header "Next actions")))
                                  (todo "WAIT"
                                        ((org-agenda-overriding-header "Waiting")))
                                  (todo "TODO"
                                        ((org-agenda-overriding-header "Unscheduled tasks")
                                         (org-agenda-skip-function
                                          '(org-agenda-skip-entry-if 'scheduled 'deadline))))))
                                ("n" "Next actions" todo "NEXT")
                                ("w" "Waiting" todo "WAIT")
                                ("s" "Someday / maybe" todo "SOMEDAY"))

   ;; Capture quickly; clarify and organize during inbox processing.
   org-capture-templates '(("t" "Inbox task" entry
                            (file org-default-notes-file)
                            "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"
                            :empty-lines 1)
                           ("n" "Inbox note" entry
                            (file org-default-notes-file)
                            "* %? :note:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"
                            :empty-lines 1)
                           ("a" "Next action" entry
                            (file+headline (lambda ()
                                             (expand-file-name "actions.org" +org-agenda-directory))
                                           "Actions")
                            "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"
                            :empty-lines 1)
                           ("p" "Project" entry
                            (file (lambda ()
                                    (expand-file-name "projects.org" +org-agenda-directory)))
                            "* %^{Project name} :project:\n:PROPERTIES:\n:CREATED: %U\n:END:\n** NEXT %?\n"
                            :empty-lines 1)
                           ("s" "Someday / maybe" entry
                            (file+headline (lambda ()
                                             (expand-file-name "someday.org" +org-agenda-directory))
                                           "Someday / Maybe")
                            "* SOMEDAY %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"
                            :empty-lines 1)
                           ("e" "Calendar event" entry
                            (file (lambda ()
                                    (expand-file-name "calendar.org" +org-agenda-directory)))
                            "* %?\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
                            :empty-lines 1))

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
   org-insert-heading-respect-content t

   ;; better keybindings
   org-special-ctrl-a/e t
   org-special-ctrl-k t
   org-special-ctrl-o t
   org-support-shift-select t
   org-ctrl-k-protect-subtree 'error
   org-fold-catch-invisible-edits 'show-and-error

   org-imenu-depth 4)

  ;; Block delimiter faces inherit from `org-meta-line'.
  (dolist (face '(org-meta-line org-block-begin-line org-block-end-line))
    (set-face-attribute face nil :height 0.9))

  ;; Cycle the visible parent heading when point is in or just past folded text.
  (add-hook! org-cycle-tab-first-hook
    (defun +org-cycle-visible-heading ()
      "Cycle the visible parent heading at either edge of a folded subtree."
      (when-let ((folded-region
                  (or (org-fold-get-region-at-point 'headline)
                      (and (> (point) (point-min))
                           (org-fold-get-region-at-point
                            'headline (1- (point)))))))
        (goto-char (car folded-region))
        (org-back-to-heading t)
        (org-cycle)
        t)))

  ;; Keep first-run capture and agenda commands from failing when directories are absent.
  (make-directory (car org-agenda-files) t)

  ;; Better Org Latex Preview
  (setq org-preview-latex-default-process 'dvisvgm
        org-startup-with-latex-preview nil
        org-highlight-latex-and-related '(latex))
  (plist-put org-format-latex-options :scale 1.7)

  ;; HACK: inline highlight for CJK
  ;; (setq org-emphasis-regexp-components '("-[:space:]('\"{[:nonascii:][:alpha:]"
  ;;                                        "-[:space:].,:!?;'\")}\\[[:nonascii:][:alpha:]"
  ;;                                        "[:space:]"
  ;;                                        "."
  ;;                                        1))
  ;; (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  ;; (org-element-update-syntax)
  ;; (org-element--set-regexps)
  )


;; [ob-mermaid] Generate Mermaid diagrams through Org Babel
(use-package ob-mermaid
  :straight t
  :after org
  :config
  (setf (alist-get 'mermaid org-babel-load-languages) t))


;; [org-entities]
(use-package org-entities
  :config
  (setq org-entities-user '(("vdash" "\\vdash" t "⊢" "⊢" "⊢" "⊢")
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
      (add-hook! meow-insert-enter-hook :local #'org-appear-manual-start)
      (add-hook! meow-insert-exit-hook :local #'org-appear-manual-stop))))


(use-package org-modern
  :straight t
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

;; (use-package org-modern-indent
;;   :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
;;   :config
;;   (add-hook! org-mode-hook :depth 90 #'org-modern-indent-mode))


;; [ox]
(use-package ox
  :config
  (setq org-export-with-smart-quotes t
        org-html-validation-link nil
        org-latex-prefer-user-labels t
        org-export-with-latex t))
