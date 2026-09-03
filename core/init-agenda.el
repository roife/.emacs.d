;;; -*- lexical-binding: t -*-

;; [org] Task, project, and refile workflow shared by Agenda and Capture.
(use-package org
  :straight (:type built-in)
  :config
  (setq
   ;; Tasks describe executable work; project states describe the lifecycle
   ;; of a multi-step outcome.
   org-todo-keywords '((sequence
                        "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "SOMEDAY(s)" "IMMEDIATE(i!)"
                       "|" "DONE(d!)" "CANCELED(c@)")
                       (sequence
                        "PROPOSED(o)" "PLANNED(p)" "ACTIVE(a)" "BLOCKED(b@)" "URGENT(u!)"
                        "|" "COMPLETED(f!)" "ABANDONED(x@)"))
   org-log-done 'time
   org-log-into-drawer t
   org-log-reschedule 'time
   org-log-redeadline 'time
   org-tag-alist '((:startgroup)
                   ("@home" . ?h)
                   ("@work" . ?w)
                   (:endgroup)
                   ("note" . ?n))
   org-archive-location "%s_archive::* Archived"
   org-archive-subtree-save-file-p t)

  (add-hook! org-mode-hook
    (defun +org-set-project-archive-location ()
      "Archive active projects by domain and completion year.
Work and personal project files use separate yearly archives.  Other Org
files keep the default value of `org-archive-location'."
      (when-let* ((file buffer-file-name)
                  (domain (pcase (file-name-base file)
                            ("projects-work" "work")
                            ("projects-personal" "personal"))))
        (setq-local org-archive-location
                    (format "archives/projects-%s-%s.org::* Archived"
                            domain
                            (format-time-string "%Y")))))))


;; [org-refile]
(use-package org-refile
  :straight nil
  :hook (org-after-refile-insert . save-buffer)
  :init
  ;; Refile clarified inbox items into an action list or a project.
  (setq org-refile-targets
        `((,(mapcar (lambda (file)
                      (expand-file-name (concat "agenda/" file) org-directory))
                    '("actions.org" "projects-work.org" "projects-personal.org"
                      "routines.org" "someday.org"))
           :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm))


;; [org-capture]
(use-package org-capture
  :straight nil
  :bind ("C-c o c" . org-capture)
  :config
  (setq
   org-default-notes-file (expand-file-name "agenda/inbox.org" org-directory)
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
                            (file+headline "agenda/actions.org" "Actions")
                            "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"
                            :empty-lines 1)
                           ("p" "Project")
                           ("pw" "Work project" entry
                            (file "agenda/projects-work.org")
                            "* PLANNED %^{Project name} :project:\n:PROPERTIES:\n:CREATED: %U\n:END:\n** NEXT %?\n"
                            :empty-lines 1)
                           ("pp" "Personal project" entry
                            (file "agenda/projects-personal.org")
                            "* PLANNED %^{Project name} :project:\n:PROPERTIES:\n:CREATED: %U\n:END:\n** NEXT %?\n"
                            :empty-lines 1)
                           ("r" "Reminder" entry
                            (file org-default-notes-file)
                            "* TODO %?\nSCHEDULED: %^{When}T\n:PROPERTIES:\n:CREATED: %U\n:APPT_WARNTIME: %^{Warn before (minutes)|15}\n:END:\n"
                            :empty-lines 1)
                           ("R" "Repeating reminder" entry
                            (file+headline "agenda/routines.org" "Recurring")
                            "* TODO %?
SCHEDULED: %(let ((time (org-read-date t t nil \"First occurrence: \")))
              (format \"<%s %s>\"
                      (format-time-string \"%Y-%m-%d %a %H:%M\" time)
                      (completing-read
                       \"Repeat interval: \"
                       '(\".+1d\" \".+1w\" \".+1m\" \"++1w\" \"++1m\" \"++1y\")
                       nil t)))
:PROPERTIES:
:CREATED: %U
:APPT_WARNTIME: %^{Warn before (minutes)|15}
:END:
"
                            :empty-lines 1)
                           ("s" "Someday / maybe" entry
                            (file+headline "agenda/someday.org" "Someday / Maybe")
                            "* SOMEDAY %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n"
                            :empty-lines 1)
                           ("e" "Calendar event" entry
                            (file "agenda/calendar.org")
                            "* %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:APPT_WARNTIME: %^{Warn before (minutes)|15}\n:END:\n%^{When}T\n%?\n"
                            :empty-lines 1))))


;; [org-agenda]
(use-package org-agenda
  :straight nil
  :preface
  (defun +org-agenda-show-eisenhower-quadrants (&rest _)
    (interactive)
    (org-agenda
     nil
     (if (equal (cadr org-agenda-redo-command) "Eisenhower quadrants")
         "d"
       "e")))
  :bind (("C-c o a" . org-agenda)
         :map org-agenda-mode-map
         ("V" . +org-agenda-show-eisenhower-quadrants))
  :config
  (setq
   ;; All Org files directly under ~/org/agenda/ are included.
   org-agenda-files (list (expand-file-name "agenda/" org-directory))
   org-agenda-window-setup 'current-window
   org-agenda-restore-windows-after-quit t
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-deadline-faces '((1.001 . error)
                               (1.0 . org-warning)
                               (0.5 . org-upcoming-deadline)
                               (0.0 . org-upcoming-distant-deadline))
   org-agenda-custom-commands '(("d" "Dashboard"
                                 ((agenda ""
                                          ((org-agenda-overriding-header "This week")
                                           (org-agenda-span 7)
                                           (org-agenda-start-on-weekday nil)
                                           (org-agenda-start-day "+0d")))
                                  (todo "NEXT"
                                        ((org-agenda-overriding-header "Next actions")))
                                  (todo "WAIT"
                                        ((org-agenda-overriding-header "Waiting")))
                                  (todo "ACTIVE"
                                        ((org-agenda-overriding-header "Active projects")))
                                  (todo "BLOCKED"
                                        ((org-agenda-overriding-header "Blocked projects")))
                                  (todo "PLANNED"
                                        ((org-agenda-overriding-header "Planned projects")))
                                  (todo "PROPOSED"
                                        ((org-agenda-overriding-header "Proposed projects")))))
                                ("e" "Eisenhower quadrants"
                                 ((tags-todo "PRIORITY={A\\|B}/!IMMEDIATE|URGENT"
                                             ((org-agenda-overriding-header
                                               "Q1 · Important and urgent (A/B)")
                                              (org-agenda-skip-function
                                               '(org-agenda-skip-entry-if
                                                 'notregexp org-priority-regexp))))
                                  (tags-todo "PRIORITY={A\\|B}/!-IMMEDIATE-URGENT"
                                             ((org-agenda-overriding-header
                                               "Q2 · Important, not urgent (A/B)")
                                              (org-agenda-skip-function
                                               '(org-agenda-skip-entry-if
                                                 'notregexp org-priority-regexp))))
                                  (tags-todo "PRIORITY=\"C\"/!IMMEDIATE|URGENT"
                                             ((org-agenda-overriding-header
                                               "Q3 · Not important and urgent (C)")
                                              (org-agenda-skip-function
                                               '(org-agenda-skip-entry-if
                                                 'notregexp org-priority-regexp))))
                                  (tags-todo "PRIORITY=\"C\"/!-IMMEDIATE-URGENT"
                                             ((org-agenda-overriding-header
                                               "Q4 · Not important, not urgent (C)")
                                              (org-agenda-skip-function
                                               '(org-agenda-skip-entry-if
                                                 'notregexp org-priority-regexp))))
                                  (alltodo ""
                                           ((org-agenda-overriding-header
                                             "Unclassified · choose A, B, or C")
                                            (org-agenda-skip-function
                                             '(org-agenda-skip-entry-if
                                               'regexp org-priority-regexp)))))
                                 ((org-agenda-sorting-strategy
                                   '(priority-down category-keep))))
                                ("i" "Immediate actions" todo "IMMEDIATE")
                                ("u" "Urgent projects" todo "URGENT")
                                ("n" "Next actions" todo "NEXT")
                                ("w" "Waiting" todo "WAIT")
                                ("o" "Proposed projects" todo "PROPOSED")
                                ("p" "Projects"
                                 ((todo "URGENT"
                                        ((org-agenda-overriding-header "Urgent projects")))
                                  (todo "ACTIVE"
                                        ((org-agenda-overriding-header "Active projects")))
                                  (todo "BLOCKED"
                                        ((org-agenda-overriding-header "Blocked projects")))
                                  (todo "PLANNED"
                                        ((org-agenda-overriding-header "Planned projects")))
                                  (todo "PROPOSED"
                                        ((org-agenda-overriding-header "Proposed projects")))))
                                ("s" "Someday / maybe" todo "SOMEDAY"))))


;; [calendar]
(use-package calendar
  :straight nil
  :bind (("C-c o C" . calendar)
         :map calendar-mode-map
         ("H" . calendar-cursor-holidays))
  :hook (calendar-today-visible . calendar-mark-today)
  :config
  ;; `diary-file' contains "%%(org-diary)", which exposes Agenda entries
  ;; to Calendar while preserving Org's scheduling and repeater semantics.
  (setq calendar-chinese-all-holidays-flag t
        calendar-mark-holidays-flag t
        calendar-holidays holiday-oriental-holidays
        calendar-mark-diary-entries-flag t))


;; [org-clock] Portable desktop notification backend, loaded on first use.
(use-package org-clock
  :straight nil
  :commands (org-show-notification))


;; [appt] Convert today's timed Org entries into desktop notifications.
(use-package appt
  :straight nil
  :after org
  :defer 3
  :preface
  (defun +appt-refresh ()
    (org-agenda-to-appt t))
  :hook ((org-mode . +org-appt-refresh-after-save)
         ((org-capture-after-finalize
           org-after-todo-state-change) . +appt-refresh))
  :config
  (setq appt-message-warning-time 15
        appt-display-interval 5
        appt-display-diary nil
        appt-audible nil
        appt-delete-window-function #'ignore
        appt-disp-window-function
        (lambda (minutes _current-time message)
          (org-show-notification
           (format "%s min · %s"
                   (string-join (ensure-list minutes) "\n")
                   (string-join (ensure-list message) "\n")))))

  (add-hook! org-mode-hook
    (defun +org-appt-refresh-after-save (&rest _)
      (add-hook 'after-save-hook #'+appt-refresh nil t)))

  (appt-activate 1)
  (+appt-refresh)
  (run-at-time "00:01" 86400 #'+appt-refresh))


;; [org-pomodoro] Clock focused work sessions on the Org task at point.
(use-package org-pomodoro
  :straight t
  :after org
  :bind ("C-c o p" . org-pomodoro)
  :config
  (setq org-pomodoro-length 30
        org-pomodoro-long-break-length 15))
