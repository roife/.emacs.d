;;; -*- lexical-binding: t -*-

(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'cl-lib))

;;; Get current window
(defvar +modeline-current-window nil)
(defun +modeline-set-selected-window (&rest _)
  "Set `+modeline-current-window' appropriately."
  (let ((win (frame-selected-window)))
    (setq +modeline-current-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))

(defsubst +modeline-window-active-p ()
  "Whether is an active window."
  (eq (frame-selected-window) +modeline-current-window))
(add-hook 'pre-redisplay-functions #'+modeline-set-selected-window)

;;; Check whether `window-total-width' is larger than the limit
(defconst +modeline-window-width-limit 80)
(defvar-local +modeline-large-width-p nil)
(defun +modeline-window-size-change-function (&rest _)
  "Function for `window-size-change-functions'."
  (setq +modeline-large-width-p
        (> (window-total-width) +modeline-window-width-limit)))
(add-hook 'after-revert-hook #'+modeline-window-size-change-function)
(add-hook 'buffer-list-update-hook #'+modeline-window-size-change-function)
(add-hook 'window-size-change-functions #'+modeline-window-size-change-function)

;;;; face
(defgroup +modeline nil
  "Modeline faces."
  :group 'faces)

(defface +modeline-line-number-active-face
  `((t (:background ,(face-background 'mode-line-inactive))))
  "The face for line number on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-vc-mode-active-face
  '((t (:inherit (font-lock-constant-face))))
  "The face for vc-mode on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-meta-active-face
  `((t (:background ,(face-foreground 'font-lock-function-name-face)
                    :foreground ,(face-background 'mode-line))))
  "Face used for meta panel on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-meta-inactive-face
  '((t (:inherit (bold font-lock-function-name-face))))
  "The face for meta panel on the mode-line of an inactive window."
  :group '+modeline)

(defface +modeline-modification-active-face
  '((t (:inherit (font-lock-function-name-face))))
  "The face for modification indicator on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-project-name-active-face
  '((t (:inherit (bold font-lock-function-type-face))))
  "The face for project name on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-project-name-inactive-face
  `((t (:foreground ,(face-foreground 'mode-line))))
  "The face for project name on the mode-line of an inactive window."
  :group '+modeline)

(defface +modeline-buffer-name-active-face
  '((t (:inherit (bold font-lock-function-name-face))))
  "The face for buffer name on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-buffer-name-inactive-face
  `((t (:foreground ,(face-foreground 'mode-line)
                    :bold t)))
  "The face for buffer name on the mode-line of an inactive window."
  :group '+modeline)

(defface +modeline-persp-name-active-face
  '((t (:inherit (bold font-lock-doc-face))))
  "The face for persp name on the mode-line of an active window."
  :group '+modeline)

;;; Indicators
(with-eval-after-load 'popper
  ;; modeline indicator
  (setq popper-mode-line
        '(:propertize " POP |"
                      face +modeline-meta-active-face)))

(defsubst +modeline-macro-indicator ()
  "Display current Emacs macro being recorded."
  (cond (defining-kbd-macro "| Macro   ")
        (executing-kbd-macro "| Macro   ")))

(defsubst +modeline-anzu-indicator ()
  "Display the number for anzu."
  (when (bound-and-true-p anzu--state)
    (propertize
     (let ((here anzu--current-position)
           (total anzu--total-matched))
       (cond ((eq anzu--state 'replace-query)
              (format "| %d replace " anzu--cached-count))
             ((eq anzu--state 'replace)
              (format "| %d/%d " here total))
             (anzu--overflow-p
              (format "| %s+ " total))
             (t
              (format "| %s/%d search " here total)))))))

(defsubst +modeline-multiple-cursors-indicator ()
  "Display the number of multiple cursors."
  (when (bound-and-true-p multiple-cursors-mode)
    (format "| %d cursors " (mc/num-cursors))))

(defsubst +modeline-use-region-indicator ()
  "Display selected region in current buffer."
  (when (use-region-p)
    (format "| L%d W%d C%d "
            (count-lines (region-beginning) (region-end))
            (count-words (region-beginning) (region-end))
            (abs (- (mark t) (point))))))

(defsubst +modeline-overwrite-indicator ()
  "Display whether it is in overwrite mode."
  (when overwrite-mode "| Ovr "))

(defsubst +modeline-modal-indicator ()
  "Display the modal indicator for modal editing."
  "")

;;; Cache persp name
(defvar-local +modeline-persp-name nil)
(defun +modeline-update-persp-name (&rest _)
  "Update perspective name in mode-line."
  (setq +modeline-persp-name
        (when-let* ((persp (and (bound-and-true-p persp-mode)
                                (get-current-persp)))
                    (name (safe-persp-name persp)))
          (concat "#"  name " "))))
(add-hook 'find-file-hook #'+modeline-update-persp-name)
(add-hook 'buffer-list-update-hook #'+modeline-update-persp-name)
(add-hook 'find-file-hook #'+modeline-update-persp-name)
(add-hook 'persp-activated-functions #'+modeline-update-persp-name)
(add-hook 'persp-renamed-functions #'+modeline-update-persp-name)

;;; Cache project name
(defvar-local +modeline-project-name nil)
(defsubst +modeline-update-project-name ()
  "Get project name for current buffer."
  (setq +modeline-project-name
        (when-let ((project (project-current)))
          (concat
           " "
           (file-name-nondirectory
            (directory-file-name (project-root project)))
           ))))
(add-hook 'find-file-hook #'+modeline-update-project-name)

;;; Cache remote host name
(defvar-local +modeline-remote-host-name nil)
(defsubst +modeline-update-remote-host-name ()
  "Hostname for remote buffers."
  (setq +modeline-remote-host-name
        (when (and default-directory
                   (file-remote-p default-directory 'host))
          (concat "@" (file-remote-p default-directory 'host)))))
(add-hook 'find-file-hook #'+modeline-update-remote-host-name)

;;; Cache flymake report
(defvar-local +modeline-flymake-indicator nil)
(defun +modeline-flymake-update (&rest _)
  "Display flymake info for current buffer."
  (setq +modeline-flymake-indicator
        (when (and flymake-mode (flymake-running-backends))
          (let* ((err-count (cadadr (flymake--mode-line-counter :error t)))
                 (warning-count (cadadr (flymake--mode-line-counter :warning t)))
                 (note-count (cadadr (flymake--mode-line-counter :note t)))
                 (err (when err-count (propertize err-count 'face '(:inherit compilation-error))))
                 (warning (when warning-count (propertize (concat " " warning-count) 'face '(:inherit compilation-warning))))
                 (note (when note-count (propertize (concat " " note-count) 'face '(:inherit compilation-info)))))
            (concat " [" err warning note "]"))))
  )
(advice-add #'flymake--handle-report :after #'+modeline-flymake-update)
(add-hook 'flymake-mode-hook #'+modeline-flymake-update)

;;; Cache VCS status
(defvar-local +modeline-vcs-status nil)
(defun +modeline-update-vcs-status (&rest _)
  "Update icon of vcs state in mode-line."
  (setq +modeline-vcs-status
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state (file-local-name buffer-file-name) backend))
                 (icon (cond ((memq state '(edited added)) "*")
                             ((eq state 'needs-merge) "&")
                             ((eq state 'needs-update) "??")
                             ((eq state 'ignored) "#")
                             ((eq state 'unregistered) "?")
                             ((memq state '(removed conflict missing)) "!")
                             (t "-")))
                 (str (if vc-display-status
                          (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                        "")))
            (concat "/" str icon)))))
(add-hook 'find-file-hook #'+modeline-update-vcs-status)
(add-hook 'after-save-hook #'+modeline-update-vcs-status)
(advice-add #'vc-refresh-state :after #'+modeline-update-vcs-status)

;;; Cache encoding info
(defvar-local +modeline-encoding nil)
(defsubst +modeline-update-encoding (&rest _)
  "Get encoding and EOL type of current buffer."
  (setq +modeline-encoding
        `(,(if (memq (coding-system-category buffer-file-coding-system)
                     '(coding-category-undecided coding-category-utf-8))
               "UTF-8"
             (upcase (symbol-name (coding-system-get buffer-file-coding-system :name))))
          ,(pcase (coding-system-eol-type buffer-file-coding-system)
             (0 ":LF ")
             (1 ":CRLF ")
             (2 ":CR ")
             (_ " ")))))
(add-hook 'find-file-hook #'+modeline-update-encoding)
(advice-add #'after-insert-file-set-coding :after #'+modeline-update-encoding)
(advice-add #'set-buffer-file-coding-system :after #'+modeline-update-encoding)

(defsubst +mode-line-active-long ()
  "Formatting active-long modeline."
  (let* ((lhs `((:propertize (" " ,(winum-get-number-string)  " ")
                             face +modeline-meta-active-face)
                (:propertize ,(when (+modeline-window-active-p)
                                (concat (+modeline-macro-indicator)
                                        (+modeline-anzu-indicator)
                                        (+modeline-multiple-cursors-indicator)
                                        (+modeline-use-region-indicator)
                                        (+modeline-overwrite-indicator)))
                             face +modeline-meta-active-face)
                (:propertize " %*" face +modeline-modification-active-face)
                " %I "
                (:propertize ("%b" ,+modeline-remote-host-name)
                             face +modeline-buffer-name-active-face)
                (:propertize +modeline-project-name
                             face +modeline-project-name-active-face)
                (:propertize +modeline-vcs-status
                             face +modeline-vc-mode-active-face)
                ))
         (rhs '((:propertize +modeline-persp-name
                             face +modeline-persp-name)
                (:propertize mode-name
                             face +modeline-buffer-name-active-face)
                (:eval +modeline-flymake-indicator)
                " "
                (:eval +modeline-encoding)
                (:propertize " %l,%C "
                             face +modeline-line-number-active-face)
                " "
                (-3 "%p")
                "%%"))
         (lhs-str (format-mode-line lhs))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs-str
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,rhs-w))))
      ,rhs-str)))


(defsubst +mode-line-inactive-long ()
  "Formatting active-long modeline."
  (let* ((lhs `((:propertize (" " ,(winum-get-number-string)  " ")
                             face +modeline-meta-inactive-face)
                "%* %I "
                (:propertize ("%b" ,+modeline-remote-host-name)
                             face +modeline-buffer-name-inactive-face)
                (:propertize +modeline-project-name
                             face +modeline-project-name-inactive-face)
                (:eval +modeline-vcs-status)
                ))
         (rhs '((:eval mode-name)
                " "
                (:eval +modeline-encoding)
                "%l,%C "
                (-3 "%p")
                "%%"))
         (lhs-str (format-mode-line lhs))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs-str
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,rhs-w)))))
      ,rhs-str)))


(defsubst +mode-line-active-short ()
  "Formatting active-long modeline."
  (let* ((lhs `((:propertize (" " ,(winum-get-number-string)  " ")
                             face +modeline-meta-active-face)
                (:propertize ,(when (+modeline-window-active-p)
                                (concat (+modeline-macro-indicator)
                                        (+modeline-anzu-indicator)
                                        (+modeline-multiple-cursors-indicator)
                                        (+modeline-use-region-indicator)
                                        (+modeline-overwrite-indicator)))
                             face +modeline-meta-active-face)
                (:propertize " %*" face +modeline-modification-active-face)
                " "
                (:propertize ("%b" ,(when +modeline-remote-host-name "@"))
                             face +modeline-buffer-name-active-face)
                ))
         (rhs '((:propertize +modeline-persp-name
                             face +modeline-persp-name)
                (:propertize mode-name
                             face +modeline-buffer-name-active-face)
                " "
                (:eval +modeline-encoding)
                (:propertize " %l "
                             face +modeline-line-number-active-face)
                " "
                (-3 "%p")
                "%%"))
         (lhs-str (format-mode-line lhs))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs-str
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,rhs-w)))))
      ,rhs-str)))

(defsubst +mode-line-inactive-short ()
  "Formatting active-long modeline."
  (let* ((lhs `((:propertize (" " ,(winum-get-number-string)  " ")
                             face +modeline-meta-inactive-face)
                "%* "
                (:propertize ("%b" ,(when +modeline-remote-host-name "@"))
                             face +modeline-buffer-name-inactive-face)
                ))
         (rhs '(" %l"))
         (lhs-str (format-mode-line lhs))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs-str
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,rhs-w)))))
      ,rhs-str)))

(setq-default mode-line-format
              '((:eval (if (+modeline-window-active-p)
                           (if +modeline-large-width-p (+mode-line-active-long) (+mode-line-active-short))
                         (if +modeline-large-width-p (+mode-line-inactive-long) (+mode-line-inactive-short))))))

(setq-default header-line-format nil)

;; Automatically update face after load-theme
(defun +update-modeline-faces (&rest _)
  "Update faces for modeline."
  (set-face-attribute '+modeline-line-number-active-face nil
                      :background (face-background 'mode-line-inactive))

  (set-face-attribute '+modeline-meta-active-face nil
                      :background (face-foreground 'font-lock-function-name-face)
                      :foreground (face-background 'mode-line))

  (set-face-attribute '+modeline-project-name-inactive-face nil
                      :foreground (face-foreground 'mode-line))

  (set-face-attribute '+modeline-buffer-name-inactive-face nil
                      :foreground (face-foreground 'mode-line))
  )
(advice-add #'enable-theme :after #'+update-modeline-faces)

(provide 'init-modeline)
