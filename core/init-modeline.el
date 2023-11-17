;;; -*- lexical-binding: t -*-

(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'cl-lib))

;; show encodings for UTF-8:LF
(defvar +mode-line-show-common-encodings nil)
;; show VC tools name for Git
(defvar +mode-line-show-common-vc-tools-name nil)

;;; Check whether `window-total-width' is larger than the limit
(defconst +mode-line-window-width-limit 85)
(defvar-local +mode-line-enough-width-p nil)
(add-hook! (after-revert-hook buffer-list-update-hook window-size-change-functions)
           (defun +mode-line-window-size-change-function (&rest _)
             (setq +mode-line-enough-width-p
                   (> (window-total-width) +mode-line-window-width-limit))))

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

(defface +mode-line-mode-name-active-face
  '((t (:inherit (font-lock-function-name-face))))
  "The face for buffer name on the mode-line of an active window."
  :group '+mode-line)

(defface +mode-line-host-name-active-face
  '((t (:inherit (font-lock-function-name-face bold italic))))
  "The face for host name on the mode-line of an active window."
  :group '+mode-line)

;;; Indicators
(defsubst +mode-line-get-window-name ()
  "Get window name for current window."
  (concat " " (window-parameter (selected-window) 'ace-window-path)))

(defsubst +mode-line-get-window-name-face ()
  "Get face of window name for current window."
  (let ((modified (buffer-modified-p)))
    (cond ((eq modified t)
           '+mode-line-meta-inactive-modified-face)
          ((eq modified nil)
           '+mode-line-meta-inactive-unchanged-face)
          ((eq modified 'autosaved)
           '+mode-line-meta-inactive-autosaved-face))))

(defsubst +mode-line-macro-indicator ()
  "Display current Emacs macro being recorded."
  (cond (defining-kbd-macro "🞄 MacroDef ")
        (executing-kbd-macro "🞄 MacroExc ")))

(defsubst +mode-line-overwrite-readonly-indicator ()
  "Display whether it is in overwrite mode or read-only buffer."
  (let ((ro (when buffer-read-only " %%"))
        (ov (when overwrite-mode " #")))
    (concat ro ov " ")))

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
          (concat  "🞄 " (number-to-string (1+ count))
                   "/" (number-to-string (+ count (length after)))
                   " sym "
                   (and (cadr keyword) "in scope "))))))


;;; Cache remote host name
(defvar-local +mode-line-remote-host-name nil)
(add-hook! find-file-hook
  (defun +mode-line-update-remote-host-name ()
    "Hostname for remote buffers."
    (setq +mode-line-remote-host-name
          (when-let ((hostname (and default-directory
                                    (file-remote-p default-directory 'host))))
            (when (not (string-equal hostname "localhost"))
              (concat "@" hostname)))
          )))

;;; Cache flymake report
(defvar-local +mode-line-flymake-indicator nil)
(add-hook! flymake-mode-hook
  (defun +mode-line-update-flymake (&rest _)
    "Display flymake info for current buffer."
    (setq +mode-line-flymake-indicator
          (when (and flymake-mode (flymake-running-backends))
            (let* ((err-count (cadadr (flymake--mode-line-counter :error)))
                   (warning-count (cadadr (flymake--mode-line-counter :warning)))
                   (note-count (cadadr (flymake--mode-line-counter :note)))
                   (err (when (and err-count (not (string= err-count "0")))
                          (propertize err-count 'face '(:inherit compilation-error))))
                   (warning (when (and warning-count (not (string= warning-count "0")))
                              (propertize warning-count 'face '(:inherit compilation-warning))))
                   (note (when (and note-count (not (string= note-count "0")))
                           (propertize note-count 'face '(:inherit compilation-info))))
                   (info (string-join (remove nil (list err warning note)) "/")))
              (when (not (string-empty-p info)) (concat " " info)))))))
(advice-add #'flymake--handle-report :after #'+mode-line-update-flymake)

;;; Cache encoding info
(defvar-local +mode-line-encoding nil)
(add-hook! find-file-hook
  (defun +mode-line-update-encoding (&rest _)
    "Get encoding and EOL type of current buffer."
    (setq +mode-line-encoding
          `(,(if (memq (coding-system-category buffer-file-coding-system)
                       '(coding-category-undecided coding-category-utf-8))
                 (when +mode-line-show-common-encodings "U8")
               (let ((name (coding-system-get buffer-file-coding-system :name)))
                 (concat (if (eq name 'no-conversion) "NO-CONV" (upcase (symbol-name name)))
                         "⋅")))
            ,(pcase (coding-system-eol-type buffer-file-coding-system)
               (0 (when +mode-line-show-common-encodings "LF "))
               (1 "CRLF ")
               (2 "CR ")
               (_ "UNK "))))))
(advice-add #'after-insert-file-set-coding :after #'+mode-line-update-encoding)
(advice-add #'set-buffer-file-coding-system :after #'+mode-line-update-encoding)

;;; Cache pdf-tools info
(defvar-local +mode-line-pdf-pages nil)
(add-hook! pdf-view-change-page-hook
  (defun +mode-line-update-pdf-pages ()
    "Update PDF pages."
    (when (eq major-mode 'pdf-view-mode)
      (setq +mode-line-pdf-pages
            (format "p%d/%d "
                    (or (eval `(pdf-view-current-page)) 0)
                    (pdf-cache-number-of-pages))))))


;;; [vcs-info] cache for vcs
(defvar-local +mode-line-smerge-count nil) ; [smerge] cache for smerge conflict indicator
(defadvice! +mode-line-update-smerge-count (&rest _)
  :after '(smerge-auto-leave smerge-mode)
  (let ((all-matches-count (count-matches smerge-begin-re (point-min) (point-max))))
    (setq-local +mode-line-smerge-count
                (if (zerop all-matches-count)
                    nil
                  (propertize (concat "[" (number-to-string all-matches-count) "]")
                              'face 'vc-dir-status-warning)))
    ))

(defvar-local +mode-line-vcs-info nil)
(add-hook! (find-file-hook after-save-hook)
  (defun +mode-line-update-vcs-info ()
    (when (and vc-mode buffer-file-name)
      (setq +mode-line-vcs-info
            (let* ((backend (vc-backend buffer-file-name))
                   (state   (vc-state buffer-file-name backend))
                   (rev     (if +mode-line-show-common-vc-tools-name
                                (substring-no-properties vc-mode 1)
                              (substring-no-properties vc-mode (+ (if (eq backend 'Hg) 2 3) 2))))
                   (face (cond ((eq state 'up-to-date) 'vc-dir-status-up-to-date)
                               ((eq state 'ignored) 'vc-dir-status-ignored)
                               ((memq state '(needs-update needs-merge conflict missing)) 'vc-dir-status-warning)
                               (t 'vc-dir-status-edited)))
                   (state-symbol (cond ((eq state 'up-to-date) "√")
                                       ((eq state 'edited) "*")
                                       ((eq state 'added) "@")
                                       ((eq state 'needs-update) "￬")
                                       ((eq state 'needs-merge) "&")
                                       ((eq state 'unlocked-changes) "")
                                       ((eq state 'removed) "×")
                                       ((eq state 'conflict) "!")
                                       ((eq state 'missing) "?")
                                       ((eq state 'ignored) "-")
                                       ((eq state 'unregistered) "+")
                                       ((stringp state) (concat "#" state ":"))
                                       (t " "))))
              (concat " "
                      (propertize (concat rev state-symbol)
                                  'face face
                                  'help-echo (get-text-property 1 'help-echo vc-mode))))))))
(advice-add #'vc-refresh-state :after #'+mode-line-update-vcs-info)


;; [buffer position]
(defsubst +mode-line-buffer-position ()
  (let ((pos (format-mode-line '(-3 "%p"))))
    (pcase pos
      ("Top" "⊤")
      ("Bot" "⊥")
      ("All" "∀")
      (_ (let ((first-char (substring pos 0 1)))
           (if (string= first-char " ") "0" first-char))))))


;; [project-crumb]
(defvar-local +mode-line-project-crumb nil)
(defsubst +mode-line-update-project-crumb (&rest _)
  (or +mode-line-project-crumb
      (setq +mode-line-project-crumb
            (breadcrumb-project-crumbs))))
(add-hook 'find-file-hook #'+mode-line-update-project-crumb)
(add-hook 'after-save-hook #'+mode-line-update-project-crumb)
(add-hook 'clone-indirect-buffer-hook #'+mode-line-update-project-crumb)
(add-hook 'Info-selection-hook #'+mode-line-update-project-crumb)
(advice-add #'rename-buffer :after #'+mode-line-update-project-crumb)
(advice-add #'set-visited-file-name :after #'+mode-line-update-project-crumb)
(advice-add #'pop-to-buffer :after #'+mode-line-update-project-crumb)
(advice-add #'popup-create :after #'+mode-line-update-project-crumb)
(advice-add #'popup-delete :after #'+mode-line-update-project-crumb)


(defsubst +mode-line-normal ()
  "Formatting active-long mode-line."
  (let* ((meta-face (+mode-line-get-window-name-face))
         (active-p (mode-line-window-selected-p))
         (panel-face `(:inherit ,meta-face :inverse-video ,active-p))
         (imenu (and +mode-line-enough-width-p (breadcrumb-imenu-crumbs)))
         (imenu-text (when (and imenu (not (string-empty-p imenu)))
                       (concat "⋅" imenu)))
         (lhs `((:propertize ,(+mode-line-get-window-name)
                             face ,panel-face)
                (:propertize ,(+mode-line-overwrite-readonly-indicator)
                             face ,panel-face)
                (,active-p (:propertize
                            ,(concat (+mode-line-macro-indicator)
                                     (+mode-line-symbol-overlay-indicator))
                            face ,panel-face))
                " "
                ;; (:propertize "%b" face ,meta-face)
                (,(not +mode-line-project-crumb)
                 (:propertize "%b" face ,meta-face)
                 ,+mode-line-project-crumb)
                (:propertize +mode-line-remote-host-name
                             face +mode-line-host-name-active-face)
                (,active-p ,imenu-text (:propertize ,imenu-text face nil))
                ))
         (vcs-info (concat +mode-line-vcs-info +mode-line-smerge-count))
         (rhs `((:propertize mode-name face ,(when active-p '+mode-line-mode-name-active-face))
                (,active-p ,vcs-info
                           (:propertize ,vcs-info face nil))
                (,active-p ,+mode-line-flymake-indicator)
                " "
                (:eval +mode-line-encoding)
                ,(or +mode-line-pdf-pages
                     (list "%l⋅" '(:eval (+mode-line-buffer-position))))
                " "
                ))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,rhs-w))))
      ,rhs-str)))

(setq-default mode-line-format
              '((:eval (+mode-line-normal))))


;;; Header Line
;; TODO: The performance of bc is a little bad, so I disable it for now.
;;      Maybe I will solve the problem in the future.
;; [breadcrumb] Add breadcrumb navigation in header-line
(use-package breadcrumb
  :custom-face
  (breadcrumb-project-base-face ((t (:inherit breadcrumb-project-crumbs-face :bold t))))
  (breadcrumb-project-leaf-face ((t (:inherit font-lock-function-name-face :bold t))))
  (breadcrumb-imenu-leaf-face ((t (:inherit font-lock-function-name-face :foreground unspecified))))
  :straight (:host github :repo "joaotavora/breadcrumb" :files ("*.el"))
  :commands breadcrumb--header-line
  :config
  (setq breadcrumb-imenu-crumb-separator "⋅"
        breadcrumb-project-max-length 0.4
        breadcrumb-imenu-max-length 0.3
        breadcrumb-idle-time 10))

(setq-default header-line-format nil)
