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
  :bind ("C-t c" . org-capture)
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
  :bind (("C-t a" . org-agenda)
         :map org-agenda-mode-map
         ([remap org-agenda-goto-calendar] . +agenda-calendar-blocks))
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


;; [calfw] One calendar view for Org entries and subscribed days off/workdays.
(use-package calfw
  :straight (:host github :repo "haji-ali/emacs-calfw")
  :preface
  (defun +agenda-calendar (&optional view)
    "Open Org and China holidays, keeping the date selected in Agenda."
    (interactive)
    (require 'calfw-org)
    (require 'calfw-ical)
    (let ((day (when (derived-mode-p 'org-agenda-mode)
                 (get-text-property (point) 'day))))
      (calfw-open-calendar-buffer
       :date (if day (calendar-gregorian-from-absolute day)
               (calendar-current-date))
       :view view
       :custom-map calfw-org-schedule-map
       :contents-sources
       (list (calfw-org-create-source nil "Org" "SeaGreen")
             (calfw-ical-create-source "https://cdn.jsdelivr.net/npm/chinese-days/dist/holidays.ics" "中国放假" "IndianRed")))))
  :config
  (setq calfw-calendar-buffer-name "*Org Calendar*"
        calfw-display-calendar-holidays t)
  (add-hook! calfw-calendar-mode-hook #'+enable-conservative-scrolling)
  (advice-add 'calfw--render-footer :override (lambda (&rest _) (string))
              '((name . hide-calendar-sources))))

(use-package calfw-org
  :straight (:host github :repo "haji-ali/emacs-calfw"
                   :files ("calfw-org.el"))
  :after calfw)

(use-package calfw-ical
  :straight (:host github :repo "haji-ali/emacs-calfw"
                   :files ("calfw-ical.el"))
  :after calfw)

(use-package calfw-blocks
  :straight (:host github :repo "haji-ali/calfw-blocks")
  :after calfw-org
  :demand t
  :preface
  (defun +calfw-event (item start &optional end)
    "Return ITEM as an event, preserving Agenda properties and timing."
    (if (not (stringp item)) item
      (let* ((time (get-text-property 0 'time-of-day item))
             (minutes (when time (+ (* (/ time 100) 60) (% time 100))))
             (finish (when minutes
                       (+ minutes (round (or (get-text-property 0 'duration item)
                                             (* 60 calfw-blocks-default-event-length)))))))
        (make-calfw-event
         :title item :source (get-text-property 0 'cfw:source item)
         :start-date start
         :end-date (if finish
                       (calendar-gregorian-from-absolute
                        (+ (calendar-absolute-from-gregorian (or end start))
                           (/ finish 1440)))
                     end)
         :start-time (when minutes (list (/ minutes 60) (% minutes 60)))
         :end-time (when finish (list (% (/ finish 60) 24) (% finish 60)))))))
  (defun +calfw-block-contents (contents)
    "Convert text entries and place untimed events in the all-day area."
    (let (periods days)
      (cl-loop for (date . items) in contents
               do (if (eq date 'periods)
                      (pcase-dolist (`(,start ,end ,item) items)
                        (push (list start end (+calfw-event item start end)) periods))
                    (let ((timed
                           (cl-loop for item in items
                                    for event = (+calfw-event item date)
                                    if (calfw-event-start-time event) collect event
                                    else do (push (list date (or (calfw-event-end-date event) date)
                                                        event) periods))))
                      (when timed (push (cons date timed) days)))))
      (cons (cons 'periods (nreverse periods)) (nreverse days))))
  (defun +agenda-calendar-blocks ()
    "Open Org and subscribed holidays in a weekly time-block view."
    (interactive)
    (+agenda-calendar 'block-week))
  :config
  ;; Normal calfw views pass two arguments; the block toolbar needs four.
  ;; Block views call their own toolbar directly, so keep the normal one.
  (advice-remove 'calfw--render-toolbar #'calfw-blocks-render-toolbar)
  (advice-add 'calfw--contents-put-source :filter-return #'+calfw-block-contents))


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
  :bind ("C-t p" . org-pomodoro)
  :config
  (setq org-pomodoro-length 30
        org-pomodoro-long-break-length 15))
