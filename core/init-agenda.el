;;; -*- lexical-binding: t -*-

;; [org] Task, project, and refile workflow shared by Agenda and Capture.
(use-package org
  :straight (:type built-in)
  :config
  (setq
   ;; A single workflow keeps tasks and project headings consistent.
   org-todo-keywords '((sequence
                        "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "SOMEDAY(s)" "URGENT(i!)"
                        "|" "DONE(d!)" "CANCELED(c@)"))
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
   org-archive-subtree-save-file-p t
   org-deadline-warning-days 3)

  (add-hook! org-mode-hook
    (defun +org-set-project-archive-location ()
      "Archive entries by source filename and year."
      (when buffer-file-name
        (setq-local org-archive-location
                    (format "archives/%s-%s.org::* Archived"
                            (file-name-base buffer-file-name)
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
                    '("actions.org" "work.org" "personal.org"
                      "routines.org" "someday.org"))
           :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm))


;; [org-capture]
(use-package org-capture
  :straight nil
  :require-incrementally (org-agenda t)
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
                            (file "agenda/work.org")
                            "* TODO %^{Project name} :project:\n:PROPERTIES:\n:CREATED: %U\n:END:\n** NEXT %?\n"
                            :empty-lines 1)
                           ("pp" "Personal project" entry
                            (file "agenda/personal.org")
                            "* TODO %^{Project name} :project:\n:PROPERTIES:\n:CREATED: %U\n:END:\n** NEXT %?\n"
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
  :require-incrementally t
  :bind (("C-c o a" . org-agenda))
  :config
  (cl-flet ((files (&rest names)
              (mapcar (lambda (name)
                        (expand-file-name (concat "agenda/" name ".org") org-directory))
                      names)))
    (let* ((agenda-files (files "actions" "calendar" "inbox" "personal"
                                "routines" "someday" "work"))
           (dated-files (remove (car (files "someday")) agenda-files))
           (action-files (remove (car (files "calendar")) dated-files))
           (project-files (files "personal" "work"))
           (someday-files (files "personal" "someday" "work")))
      (setq
       org-agenda-files agenda-files
       org-agenda-sticky t
       org-agenda-window-setup 'current-window
       org-agenda-restore-windows-after-quit t
       org-agenda-inhibit-startup t
       org-agenda-dim-blocked-tasks nil
       org-agenda-use-tag-inheritance nil
       org-agenda-ignore-properties '(stats)
       org-agenda-skip-scheduled-if-done t
       org-agenda-skip-deadline-if-done t
       org-agenda-deadline-faces '((1.001 . error)
                                   (1.0 . org-warning)
                                   (0.5 . org-upcoming-deadline)
                                   (0.0 . org-upcoming-distant-deadline))
       org-agenda-custom-commands
       `(("d" "Dashboard"
          ((agenda "" ((org-agenda-files ',dated-files)))
           (todo "URGENT" ((org-agenda-files ',action-files)
                           (org-agenda-overriding-header "Urgent actions")))
           (todo "NEXT" ((org-agenda-files ',action-files)
                         (org-agenda-overriding-header "Next actions")))
           (tags-todo "+project/TODO"
                      ((org-agenda-files ',project-files)
                       (org-agenda-overriding-header "Projects")))
           (todo "WAIT" ((org-agenda-files ',action-files)
                         (org-agenda-overriding-header "Waiting")))
           (todo "SOMEDAY" ((org-agenda-files ',someday-files)
                            (org-agenda-overriding-header "Someday / maybe")))))
         ("i" "Urgent actions" todo "URGENT"
          ((org-agenda-files ',action-files)))
         ("n" "Next actions" todo "NEXT"
          ((org-agenda-files ',action-files)))
         ("w" "Waiting" todo "WAIT"
          ((org-agenda-files ',action-files)))
         ("p" "Projects"
          ((tags-todo "+project/TODO"
                      ((org-agenda-files ',project-files)
                       (org-agenda-overriding-header "Projects")))))
         ("s" "Someday / maybe" todo "SOMEDAY"
          ((org-agenda-files ',someday-files))))))))


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


;; [org-clock] Portable desktop notification backend.
(use-package org-clock
  :straight nil
  :commands (org-show-notification))


;; [appt] Convert today's timed Org entries into desktop notifications.
(use-package appt
  :straight nil
  :require-incrementally (org-agenda t)
  :preface
  (defvar +appt-refresh-timer nil
    "Timer for deferred appointment refreshes.")
  (defun +appt-refresh ()
    "Schedule one appointment refresh after Emacs becomes idle."
    (when +appt-refresh-timer
      (cancel-timer +appt-refresh-timer))
    (setq +appt-refresh-timer
          (run-with-idle-timer 0.75 nil #'org-agenda-to-appt t)))
  :hook (((org-capture-after-finalize
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

  (add-hook 'after-save-hook
            (lambda ()
              (when (and (derived-mode-p 'org-mode)
                         (member buffer-file-name org-agenda-files))
                (+appt-refresh))))

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
