;;; -*- lexical-binding: t -*-

;; [org-fragtog] Preview and edit latex in md/org elegantly
;; (use-package org-fragtog
;;   :straight t
;;   :hook ((org-mode . org-fragtog-mode)))

(defvar +org-agenda-directory nil
  "Directory containing files used by Org Agenda.")

(defvar +org-refile-files nil
  "Org files that contain GTD refile destinations.")

;; Low-coupling utilities adapted from Doom's :lang org module.
;; Dynamically rebound by `+org-save-buffer-after-capture-refile-a'.
(defvar org-after-refile-insert-hook)

;; [org-persist]
(use-package org-persist
  :straight nil
  :init
  (setq org-persist-directory (no-littering-expand-var-file-name "org/persist/")))


;; [org]
(use-package org
  :straight (:type built-in)
  :init
  ;; Load optional Org modules only when explicitly enabled.
  (setq org-modules nil
        org-hide-emphasis-markers t)
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
   ;; Keep generated LaTeX previews out of note directories.
   org-preview-latex-image-directory (no-littering-expand-var-file-name "org/latex/")
   ;; Recognize a), A), a., and A. as list markers.
   org-list-allow-alphabetical t

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
   org-agenda-skip-unavailable-files t
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-deadline-faces '((1.001 . error)
                               (1.0 . org-warning)
                               (0.5 . org-upcoming-deadline)
                               (0.0 . org-upcoming-distant-deadline))
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
   ;; Respect #+startup visibility, archived trees, drawers, and hidden blocks.
   org-startup-folded nil
   org-pretty-entities t
   org-ellipsis "…"
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

   ;; Save archive targets immediately, and keep numbering opt-out explicit.
   org-archive-subtree-save-file-p t
   org-num-face '(:inherit org-special-keyword :underline nil :weight bold)
   org-num-skip-tags '("noexport" "nonum")

   ;; better keybindings
   org-special-ctrl-a/e t
   org-special-ctrl-k t
   org-special-ctrl-o t
   org-support-shift-select t
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

  (add-to-list 'org-src-lang-modes '("md" . markdown))
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

  (add-hook! org-babel-after-execute-hook
    (defun +org-redisplay-inline-images-in-babel-result-h ()
      "Refresh inline images produced by the Babel block at point.
After Babel inserts its result, find that result's bounds and refresh link
previews only within that region.  Skip exports and temporary buffers to avoid
unnecessary display work during non-interactive operations."
      (unless (or (bound-and-true-p org-export-current-backend)
                  (string-prefix-p " *temp" (buffer-name)))
        (save-excursion
          (when-let* ((beg (org-babel-where-is-src-block-result))
                      (end (progn
                             (goto-char beg)
                             (forward-line)
                             (org-babel-result-end))))
            (org-link-preview-region nil t (min beg end) (max beg end)))))))

  (defadvice! +org-save-buffer-after-capture-refile-a (fn &rest args)
    :around #'org-refile
    "Save the refile target after moving an entry from `org-capture'.
Temporarily prepend `save-buffer' to `org-after-refile-insert-hook' only while
`org-capture-is-refiling' is non-nil, leaving ordinary refiles unchanged."
    (let ((org-after-refile-insert-hook
           (if (bound-and-true-p org-capture-is-refiling)
               (cons #'save-buffer org-after-refile-insert-hook)
             org-after-refile-insert-hook)))
      (apply fn args)))

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

  ;; Keep first-run capture and agenda commands from failing when directories are absent.
  (make-directory (car org-agenda-files) t)

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
  (org-set-emph-re 'org-emphasis-regexp-components
                   org-emphasis-regexp-components)

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
                            ("nvdash" "\\nvdash" t "⊬" "⊬" "⊬" "⊬")
                            ("nvDash" "\\nvDash" t "⊭" "⊭" "⊭" "⊭")
                            ("subseteq" "\\subseteq" t "⊆" "⊆" "⊆" "⊆")
                            ("supseteq" "\\supseteq" t "⊇" "⊇" "⊇" "⊇")
                            ("subsetneq" "\\subsetneq" t "⊊" "⊊" "⊊" "⊊")
                            ("supsetneq" "\\supsetneq" t "⊋" "⊋" "⊋" "⊋")
                            ("nsubseteq" "\\nsubseteq" t "⊈" "⊈" "⊈" "⊈")
                            ("nsupseteq" "\\nsupseteq" t "⊉" "⊉" "⊉" "⊉")
                            ("nsubset" "\\nsubset" t "⊄" "⊄" "⊄" "⊄")
                            ("nsupset" "\\nsupset" t "⊅" "⊅" "⊅" "⊅"))))


;; [org-appear] Make invisible parts of Org elements appear visible.
(use-package org-appear
  :straight t
  :hook ((org-mode . org-appear-mode))
  :config
  (setq
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

;; [ox]
(use-package ox
  :config
  (setq org-export-with-smart-quotes t
        org-html-validation-link nil
        org-latex-prefer-user-labels t
        org-export-with-latex t))
