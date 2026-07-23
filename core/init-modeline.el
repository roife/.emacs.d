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
      (concat " " path " "))))

(defsubst +mode-line-macro-indicator ()
  "Display current Emacs macro being recorded."
  (cond (defining-kbd-macro " MacroDef ")
        (executing-kbd-macro " MacroExc ")))

(defsubst +mode-line-overwrite-readonly-indicator ()
  "Display whether it is in overwrite mode or read-only buffer."
  (let ((ro (when buffer-read-only "%% "))
        (ov (when overwrite-mode "# ")))
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
                   " sym "
                   (and (cadr keyword) "in scope "))))))


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
(add-hook! find-file-hook
  (defun +mode-line-update-encoding (&rest _)
    "Get encoding and EOL type of current buffer."
    (setq +mode-line-encoding
          (unless (and (memq (coding-system-category buffer-file-coding-system)
                             '(coding-category-undecided coding-category-utf-8))
                       (eq (coding-system-eol-type buffer-file-coding-system) 0))
            "%Z"))))
(advice-add #'after-insert-file-set-coding :after #'+mode-line-update-encoding)
(advice-add #'set-buffer-file-coding-system :after #'+mode-line-update-encoding)

;; [project-crumb]
(defvar-local +mode-line-project-crumb nil)
(add-hook! (find-file-hook after-save-hook clone-indirect-buffer-hook Info-selection-hook
                           window-configuration-change-hook)
  (defun +mode-line-update-project-crumb (&rest _)
    (setq +mode-line-project-crumb
          (breadcrumb-project-crumbs))))
(advice-add #'rename-buffer :after #'+mode-line-update-project-crumb)
(advice-add #'set-visited-file-name :after #'+mode-line-update-project-crumb)
(advice-add #'pop-to-buffer :after #'+mode-line-update-project-crumb)
(advice-add #'popup-create :after #'+mode-line-update-project-crumb)
(advice-add #'popup-delete :after #'+mode-line-update-project-crumb)

(defsubst +mode-line-normal ()
  "Formatting active-long mode-line."
  (let* ((meta-face (+mode-line-get-window-name-face))
         (active-p (mode-line-window-selected-p))
         (panel-face `(:inherit ,meta-face :inverse-video ,active-p)))
    `((:propertize ,(+mode-line-get-window-name)
                   face ,panel-face)
      (:propertize ,(+mode-line-overwrite-readonly-indicator)
                   face ,panel-face)
      (,active-p (:propertize
                  ,(concat (+mode-line-macro-indicator)
                           (+mode-line-symbol-overlay-indicator))
                  face ,panel-face))
      " "
      ,(or +mode-line-project-crumb
           `(:propertize "%b" face ,meta-face))
      " "
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
        breadcrumb-idle-time 10)
  )

;; [tab-bar] Tab bar
(use-package tab-bar
  ;; Turn on tab-bar-mode in early-init to speed-up
  :config
  (setq tab-bar-separator ""
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width nil
        tab-bar-close-button-show nil
        tab-bar-tab-hints t
        tab-bar-show nil)

  ;; WORKAROUND: fresh tab-bar for daemon
  (add-hook! (server-after-make-frame-hook window-setup-hook) :call-immediately
    (defun +refresh-tab-bar (&rest _)
      (tab-bar--update-tab-bar-lines)
      (force-mode-line-update)))
  )
