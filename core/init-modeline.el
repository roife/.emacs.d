;;; -*- lexical-binding: t -*-

(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'cl-lib))

;;; face
(defgroup +mode-line nil
  "Mode-Line faces."
  :group 'faces)

(defface +mode-line-meta-inactive-unchanged-face
  '((t (:inherit (font-lock-function-name-face bold))))
  "The face for meta panel on the mode-line of an inactive window."
  :group '+mode-line)

(defface +mode-line-meta-inactive-modified-face
  '((t (:inherit (font-lock-keyword-face bold))))
  "Face used for meta panel on the mode-line of an active window."
  :group '+mode-line)

(defface +mode-line-meta-inactive-autosaved-face
  '((t (:inherit (font-lock-doc-face bold))))
  "Face used for meta panel on the mode-line of an active window."
  :group '+mode-line)

(defface +mode-line-host-name-active-face
  '((t (:inherit (font-lock-function-name-face bold))))
  "The face for host name on the mode-line of an active window."
  :group '+mode-line)

;;; Indicators
(defsubst +mode-line-get-window-name-face ()
  "Get face of window name for current window."
  (let ((modified (buffer-modified-p)))
    (cond ((eq modified t)
           '+mode-line-meta-inactive-modified-face)
          ((eq modified nil)
           '+mode-line-meta-inactive-unchanged-face)
          ((eq modified 'autosaved)
           '+mode-line-meta-inactive-autosaved-face))))

(defsubst +mode-line-get-window-name ()
  "Get window number for current window, as set by `ace-window'."
  (let ((path (window-parameter (selected-window) 'ace-window-path)))
    (when (and path (not (string-empty-p path)))
      (concat " " path))))

(defsubst +mode-line-macro-indicator ()
  "Display current Emacs macro being recorded."
  (cond (defining-kbd-macro " MacroDef")
        (executing-kbd-macro " MacroExc")))

(defsubst +mode-line-overwrite-readonly-indicator ()
  "Display whether it is in overwrite mode or read-only buffer."
  (let ((ro (when buffer-read-only " %%"))
        (ov (when overwrite-mode " #")))
    (concat ro ov)))

(defsubst +mode-line-symbol-overlay-indicator ()
  "Display the number of matches for symbol overlay."
  (when (and (bound-and-true-p symbol-overlay-keywords-alist)
             (not symbol-overlay-temp-symbol))
    (let* ((keyword (symbol-overlay-assoc (symbol-overlay-get-symbol t)))
           (symbol (car keyword))
           (before (symbol-overlay-get-list -1 symbol))
           (after (symbol-overlay-get-list 1 symbol))
           (count (length before)))
      (if (symbol-overlay-assoc symbol)
          (concat  " " (number-to-string (1+ count))
                   "/" (number-to-string (+ count (length after)))
                   " sym"
                   (and (cadr keyword) " in scope"))))))


;;; Cache remote host name
(defvar-local +mode-line-remote-host-name nil)
(add-hook! find-file-hook
  (defun +mode-line-update-remote-host-name ()
    "Hostname for remote buffers."
    (setq +mode-line-remote-host-name
          (when-let* ((hostname (and default-directory
                                     (file-remote-p default-directory 'host))))
            (when (not (string-equal hostname "localhost"))
              (concat "@" hostname))))))


;;; Cache encoding info
(setq eol-mnemonic-unix "LF"
      eol-mnemonic-dos "CRLF"
      eol-mnemonic-mac "CR"
      eol-mnemonic-undecided "?")

(defvar-local +mode-line-encoding nil)
(defadvice! +mode-line-update-encoding (&rest _)
  :after '(after-insert-file-set-coding set-buffer-file-coding-system)
  "Get encoding and EOL type of current buffer."
  (setq +mode-line-encoding
        (unless (and (memq (coding-system-category buffer-file-coding-system)
                           '(coding-category-undecided coding-category-utf-8))
                     (eq (coding-system-eol-type buffer-file-coding-system) 0))
          "%Z")))
(add-hook! find-file-hook #'+mode-line-update-encoding)

;; [project-crumb]
(defvar-local +mode-line-project-crumb nil)
(defadvice! +mode-line-update-project-crumb (&rest _)
  :after '(rename-buffer set-visited-file-name pop-to-buffer popup-create popup-delete)
  (setq +mode-line-project-crumb
        (breadcrumb-project-crumbs)))
(add-hook! (find-file-hook after-save-hook clone-indirect-buffer-hook Info-selection-hook
                           window-configuration-change-hook)
  #'+mode-line-update-project-crumb)

;;; Cache envrc status
(defvar-local +mode-line-envrc nil)
(defun +mode-line-update-envrc (&optional buffer)
  "Cache envrc status for BUFFER."
  (setq buffer (or buffer (current-buffer)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq +mode-line-envrc
            (pcase (and (bound-and-true-p envrc-mode)
                        envrc--status)
              ('on t)
              ('error 'error)
              (_ nil)))))
  (force-mode-line-update t))
(defadvice! +mode-line-envrc-after-apply (buffer _result)
  :after #'envrc--apply
  "Update the cached envrc status after applying BUFFER's environment."
  (+mode-line-update-envrc buffer))
(add-hook! envrc-mode-hook #'+mode-line-update-envrc)


(defsubst +mode-line-normal ()
  "Formatting active-long mode-line."
  (let* ((meta-face (+mode-line-get-window-name-face))
         (active-p (mode-line-window-selected-p))
         (panel-face `(:inherit ,meta-face :inverse-video ,active-p)))
    `((:propertize ,(+mode-line-get-window-name) face ,panel-face)
      (:propertize ,(+mode-line-overwrite-readonly-indicator) face ,panel-face)
      (:propertize ,(pcase +mode-line-envrc
                      ('error " ⎇[error]")
                      (_ (when +mode-line-envrc " ⎇")))
                   face ,panel-face)
      (:propertize mode-line-process face ,panel-face)
      (,active-p (:propertize
                  ,(concat (+mode-line-macro-indicator)
                           (+mode-line-symbol-overlay-indicator))
                  face ,panel-face))
      (:propertize " " face ,panel-face)
      " "
      ,(or +mode-line-project-crumb
           `(:propertize "%b" face ,meta-face))
      (:propertize ":%l " face font-lock-comment-face)
      (:eval (breadcrumb-imenu-crumbs))
      (:propertize +mode-line-remote-host-name
                   face +mode-line-host-name-active-face)
      " "
      (:eval +mode-line-encoding)
      " "
      eglot-mode-line-progress)))

(setq-default mode-line-format '((:eval (+mode-line-normal))))
(setq-default header-line-format nil)

;;; Header Line
;; [breadcrumb] Add breadcrumb navigation in header-line
(use-package breadcrumb
  :straight (:host github :repo "joaotavora/breadcrumb" :files ("*.el"))
  :custom-face
  (breadcrumb-project-base-face ((t (:inherit breadcrumb-project-crumbs-face :bold t))))
  (breadcrumb-project-leaf-face ((t (:inherit font-lock-function-name-face :bold t))))
  (breadcrumb-imenu-leaf-face ((t (:inherit font-lock-function-name-face :foreground unspecified))))
  :config
  (setq breadcrumb-imenu-crumb-separator " ⋅ "
        breadcrumb-project-max-length 0.55
        breadcrumb-idle-time 5)
  )

;; [tab-bar] Tab bar
(use-package tab-bar
  :bind (("M-t" . tab-new)
         ("M-q" . tab-close)
         ("M-<tab>" . tab-next)
         ("M-S-<tab>" . tab-previous)
         ("M-SPC" . +tab-bar-echo))
  :config
  (setq tab-bar-separator ""
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width nil
        tab-bar-close-button-show nil
        tab-bar-tab-hints t)

  (customize-set-variable 'tab-bar-select-tab-modifiers '(meta))
  (customize-set-variable 'tab-bar-show nil)

  ;; truncate for [tab name] and add count
  (setq tab-bar-tab-name-format-functions
        '(tab-bar-tab-name-format-hints
          tab-bar-tab-name-format-truncated
          (lambda (name &rest _) (concat " " name " "))
          tab-bar-tab-name-format-face))

  (defadvice! +tab-bar-echo (&rest _)
    :after '(tab-bar-select-tab tab-bar-new-tab tab-bar-close-tab)
    "Display the current tab bar in the echo area."
    (interactive)
    (message
     "%s%s%s%s"
     (or +tab-bar-telega-indicator-cache "")
     (or +tab-bar-emms-indicator-cache "")
     (or +tab-bar-gnus-indicator-cache "")
     (cl-loop for tab in (funcall tab-bar-tabs-function)
              for i from 1
              for current = (eq (car tab) 'current-tab)
              for face = (if current
                             '(:inherit tab-bar-tab :inverse-video t)
                           'tab-bar-tab)
              concat (propertize
                      (format " %d %s " i
                              (tab-bar-tab-name-format-truncated
                               (alist-get 'name tab) tab i))
                      'face face))))

  (defvar +tab-bar-gnus-indicator-cache nil)
  (defvar +tab-bar-telega-indicator-cache nil)
  (defvar +tab-bar-emms-indicator-cache nil)

  (with-eval-after-load 'gnus
    (add-hook! (gnus-started-hook gnus-after-getting-new-news-hook
                                  gnus-group-catchup-group-hook gnus-summary-exit-hook)
      (defun +tab-bar-gnus-indicator-update (&rest _)
        "Update the cached Gnus unread count in the tab bar."
        (setq +tab-bar-gnus-indicator-cache
              (when-let* ((count (cl-loop for entry being the hash-values
                                          of gnus-newsrc-hashtb
                                          for unread = (car entry)
                                          when (numberp unread)
                                          sum unread))
                          ((> count 0)))
                (propertize (format " M %d " count) 'face 'font-lock-keyword-face))))))

  (with-eval-after-load 'telega
    (defadvice! +tab-bar-telega-indicator-update (&rest _)
      :after '(telega--on-updateUnreadChatCount
               telega--on-updateChatUnreadMentionCount
               telega--on-updateChatUnreadReactionCount)
      "Update the cached Telega status in the tab bar."
      (when (and (featurep 'telega)
                 (telega-server-live-p))
        (setq +tab-bar-telega-indicator-cache
              (let* ((chats (telega-chats-list))
                     (online-p (funcall telega-online-status-function))
                     (unread-count (or (plist-get telega--unread-chat-count :unread_unmuted_count) 0))
                     (mention-count (apply #'+ (mapcar (telega--tl-prop :unread_mention_count)
                                                       (telega-filter-chats chats '(and is-known mention)))))
                     (reaction-count (apply #'+ (mapcar (telega--tl-prop :unread_reaction_count)
                                                        (telega-filter-chats chats '(and is-known unread-reactions)))))
                     (count (+ unread-count mention-count reaction-count)))
                (propertize
                 (concat " T" (unless (zerop count) (number-to-string count)) " ")
                 'face `(:inherit font-lock-keyword-face :inverse-video ,online-p))))))

    (add-hook! (telega-ready-hook
                telega-chats-fetched-hook
                telega-kill-hook
                telega-online-status-hook)
      #'+tab-bar-telega-indicator-update))

  (with-eval-after-load 'emms
    (defun +tab-bar-emms-indicator-update (&rest _)
      "Update the cached EMMS track indicator in the tab bar."
      (setq +tab-bar-emms-indicator-cache
            (when (and (bound-and-true-p emms-player-playing-p))
              (propertize (concat " " (if emms-player-paused-p "Ⅱ" "♫") " ")
                          'face 'font-lock-keyword-face))))

    (add-hook! (emms-player-started-hook
                emms-player-paused-hook
                emms-player-stopped-hook
                emms-player-finished-hook)
      :append #'+tab-bar-emms-indicator-update)
    (+tab-bar-emms-indicator-update))

  ;; WORKAROUND: fresh tab-bar for daemon
  (add-hook! (server-after-make-frame-hook window-setup-hook) :call-immediately
    (defun +refresh-tab-bar (&rest _)
      (tab-bar--update-tab-bar-lines)
      (force-mode-line-update)))
  )
