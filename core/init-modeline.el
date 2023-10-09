;;; -*- lexical-binding: t -*-

(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'cl-lib))

;; show encodings for UTF-8:LF
(defvar +mode-line-show-common-encodings nil)
;; show VC tools name for Git
(defvar +mode-line-show-common-vc-tools-name nil)

;;; get current window
(defvar +mode-line-current-window nil)
(defun +mode-line-set-selected-window (&rest _)
  "Set `+mode-line-current-window' appropriately."
  (let ((win (frame-selected-window)))
    (setq +mode-line-current-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))
(add-hook 'pre-redisplay-functions #'+mode-line-set-selected-window)

(defsubst +mode-line-window-active-p ()
  "Whether is an active window."
  (eq (frame-selected-window) +mode-line-current-window))

;;; Check whether `window-total-width' is larger than the limit
(defconst +mode-line-window-width-limit 90)
(defvar-local +mode-line-enough-width-p nil)
(defun +mode-line-window-size-change-function (&rest _)
  "Function for `window-size-change-functions'."
  (setq +mode-line-enough-width-p
        (> (window-total-width) +mode-line-window-width-limit)))
(add-hook 'after-revert-hook #'+mode-line-window-size-change-function)
(add-hook 'buffer-list-update-hook #'+mode-line-window-size-change-function)
(add-hook 'window-size-change-functions #'+mode-line-window-size-change-function)

;;; face
(defgroup +mode-line nil
  "Mode-Line faces."
  :group 'faces)

(defface +mode-line-meta-active-unchanged-face
  '((t (:inherit (font-lock-function-name-face bold) :inverse-video t)))
  "Face used for meta panel on the mode-line of an active window."
  :group '+mode-line)

(defface +mode-line-meta-active-modified-face
  '((t (:inherit (font-lock-keyword-face bold) :inverse-video t)))
  "Face used for meta panel on the mode-line of an active window."
  :group '+mode-line)

(defface +mode-line-meta-active-autosaved-face
  '((t (:inherit (font-lock-doc-face bold) :inverse-video t)))
  "Face used for meta panel on the mode-line of an active window."
  :group '+mode-line)

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

(defface +mode-line-vc-mode-inactive-face
  '((t (:inherit (mode-line-inactive))))
  "The face for vc-mode on the mode-line of an active window."
  :group '+mode-line)

(defface +mode-line-line-number-active-face
  '((t (:inherit (mode-line-inactive) :inverse-video t)))
  "The face for line number on the mode-line of an active window."
  :group '+mode-line)

;;; Indicators
(defsubst +mode-line-get-window-name ()
  "Get window name for current window."
  (concat " " (window-parameter (selected-window) 'ace-window-path)))

(defsubst +mode-line-get-window-name-face (is-active)
  "Get face of window name for current window."
  (let ((modified (buffer-modified-p)))
    (cond ((eq modified t)
           (if is-active
               '+mode-line-meta-active-modified-face
             '+mode-line-meta-inactive-modified-face))
          ((eq modified nil)
           (if is-active
               '+mode-line-meta-active-unchanged-face
             '+mode-line-meta-inactive-unchanged-face))
          ((eq modified 'autosaved)
           (if is-active
               '+mode-line-meta-active-autosaved-face
             '+mode-line-meta-inactive-autosaved-face)))))

(defsubst +mode-line-macro-indicator ()
  "Display current Emacs macro being recorded."
  (cond (defining-kbd-macro "| MacroDef ")
        (executing-kbd-macro "| MacroExc ")))

(defsubst +mode-line-use-region-indicator ()
  "Display selected region in current buffer."
  (when (use-region-p)
    (concat "| L" (number-to-string (count-lines (region-beginning) (region-end)))
            " W" (number-to-string (count-words (region-beginning) (region-end)))
            " C" (number-to-string (abs (- (mark t) (point)))) " ")))

(defsubst +mode-line-overwrite-readonly-indicator ()
  "Display whether it is in overwrite mode or read-only buffer."
  (let ((ro (when buffer-read-only " %%"))
        (ov (when overwrite-mode " #")))
    (concat ro ov " ")))

(defsubst +mode-line-symbol-overlay-indicator ()
  "Display the number of matches for symbol overlay."
  (when (and (bound-and-true-p symbol-overlay-keywords-alist)
             (not (bound-and-true-p symbol-overlay-temp-symbol)))
    (let* ((keyword (symbol-overlay-assoc (symbol-overlay-get-symbol t)))
           (symbol (car keyword))
           (before (symbol-overlay-get-list -1 symbol))
           (after (symbol-overlay-get-list 1 symbol))
           (count (length before)))
      (if (symbol-overlay-assoc symbol)
          (concat  "| " (number-to-string (1+ count))
                   "/" (number-to-string (+ count (length after)))
                   " sym "
                   (and (cadr keyword) "in scope "))))))


;;; Cache remote host name
(defvar-local +mode-line-remote-host-name nil)
(defsubst +mode-line-update-remote-host-name ()
  "Hostname for remote buffers."
  (setq +mode-line-remote-host-name
        (when-let ((hostname (and default-directory
                                  (file-remote-p default-directory 'host))))
          (when (not (string-equal hostname "localhost"))
            (concat "@" hostname)))
        ))
(add-hook 'find-file-hook #'+mode-line-update-remote-host-name)

;;; Cache flymake report
(defvar-local +mode-line-flymake-indicator nil)
(defun +mode-line-update-flymake (&rest _)
  "Display flymake info for current buffer."
  (setq +mode-line-flymake-indicator
        (when (and flymake-mode (flymake-running-backends))
          (let* ((err-count (cadadr (flymake--mode-line-counter :error)))
                 (warning-count (cadadr (flymake--mode-line-counter :warning)))
                 (note-count (cadadr (flymake--mode-line-counter :note)))
                 (err (when err-count (propertize err-count 'face '(:inherit compilation-error))))
                 (warning (when warning-count (concat "/" (propertize warning-count 'face '(:inherit compilation-warning)))))
                 (note (when note-count (concat "/" (propertize note-count 'face '(:inherit compilation-info))))))
            (concat " " err warning note)))))
(advice-add #'flymake--handle-report :after #'+mode-line-update-flymake)
(add-hook 'flymake-mode-hook #'+mode-line-update-flymake)

;;; Cache encoding info
(defvar-local +mode-line-encoding nil)
(defsubst +mode-line-update-encoding (&rest _)
  "Get encoding and EOL type of current buffer."
  (setq +mode-line-encoding
        `(,(if (memq (coding-system-category buffer-file-coding-system)
                     '(coding-category-undecided coding-category-utf-8))
               (when +mode-line-show-common-encodings "UTF-8")
             (upcase (symbol-name (coding-system-get buffer-file-coding-system :name))))
          ,(pcase (coding-system-eol-type buffer-file-coding-system)
             (0 (when +mode-line-show-common-encodings ":LF "))
             (1 ":CRLF ")
             (2 ":CR ")
             (_ " ")))))
(add-hook 'find-file-hook #'+mode-line-update-encoding)
(advice-add #'after-insert-file-set-coding :after #'+mode-line-update-encoding)
(advice-add #'set-buffer-file-coding-system :after #'+mode-line-update-encoding)

(defsubst +mode-line-active ()
  "Formatting active-long mode-line."
  (let* ((meta-face (+mode-line-get-window-name-face t))
         (lhs `((:propertize ,(+mode-line-get-window-name)
                             face ,meta-face)
                (:propertize ,(+mode-line-overwrite-readonly-indicator) face ,meta-face)
                (:propertize ,(when (+mode-line-window-active-p)
                                (concat (+mode-line-macro-indicator)
                                        (+mode-line-symbol-overlay-indicator)
                                        (+mode-line-use-region-indicator)))
                             face ,meta-face)
                " "
                (:propertize "%b" face ,(+mode-line-get-window-name-face nil))
                ;; (:eval (breadcrumb-project-crumbs))
                ;; (:eval (when-let ((imenu (and +mode-line-enough-width-p
                ;;                               (breadcrumb-imenu-crumbs))))
                ;;          (concat "▸" imenu)))
                (:propertize +mode-line-remote-host-name
                             face +mode-line-host-name-active-face)
                ))
         (rhs '((:eval +mode-line-vcs-info)
                " "
                (:propertize mode-name
                             face +mode-line-mode-name-active-face)
                (:eval +mode-line-flymake-indicator)
                " "
                (:eval +mode-line-encoding)
                "%l "
                (-3 "%p")
                "%%"))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,rhs-w))))
      ,rhs-str)))


(defsubst +mode-line-inactive ()
  "Formatting active-long mode-line."
  (let* ((meta-face (+mode-line-get-window-name-face nil))
         (lhs `((:propertize ,(+mode-line-get-window-name)
                             face ,meta-face)
                (:propertize ,(+mode-line-overwrite-readonly-indicator) face ,meta-face)
                (:propertize "%b" face ,meta-face)))
         ;; (:eval (breadcrumb-project-crumbs))
         ;; (:eval (when-let ((imenu (and +mode-line-enough-width-p
         ;;                               (breadcrumb-imenu-crumbs))))
         ;;          (concat "▸" imenu)))))
         (rhs `((:propertize +mode-line-vcs-info
                             face +mode-line-vc-mode-inactive-face)
                " "
                (:eval mode-name)
                " "
                (:eval +mode-line-encoding)
                "%l "
                (-3 "%p")
                "%%"))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,rhs-w))))
      ,rhs-str)))

(setq-default mode-line-format
              '((:eval (if (+mode-line-window-active-p)
                           (+mode-line-active)
                         (+mode-line-inactive)))))


;;; Header Line

;; TODO: The performance of bc is a little bad, so I disable it for now.
;;      Maybe I will solve the problem in the future.
;; [breadcrumb] Add breadcrumb navigation in header-line
;; (use-package breadcrumb
;;   :custom-face
;;   (breadcrumb-project-base-face ((t (:inherit breadcrumb-project-crumbs-face :bold t))))
;;   (breadcrumb-project-leaf-face ((t (:inherit font-lock-function-name-face :bold t))))
;;   (breadcrumb-imenu-leaf-face ((t (:inherit font-lock-function-name-face :foreground unspecified))))
;;   :straight (:host github :repo "joaotavora/breadcrumb" :files ("*.el"))
;;   :commands breadcrumb--header-line
;;   :config
;;   (setq breadcrumb-imenu-crumb-separator "▸"
;;         breadcrumb-project-max-length 0.3
;;         breadcrumb-imenu-max-length 0.2))


;; [vcs-info] cache for vcs
(defvar-local +mode-line-vcs-info nil)
(defsubst +mode-line-update-vcs-info ()
  (when (and vc-mode buffer-file-name)
    (setq +mode-line-vcs-info
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state buffer-file-name backend))
                 (rev     (if +mode-line-show-common-vc-tools-name
                              (substring-no-properties vc-mode 1)
                            (substring-no-properties vc-mode (+ (if (eq backend 'Hg) 2 3) 2))))
                 (face (cond ((eq state 'up-to-date) '(vc-dir-status-up-to-date))
                             ((eq state 'ignored) '(vc-dir-status-ignored))
                             ((memq state '(needs-update needs-merge conflict missing)) '(vc-dir-status-warning))
                             (t '(vc-dir-status-edited))))
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
                                'help-echo (get-text-property 1 'help-echo vc-mode)
                                'local-map vc-mode-line-map
                                'mouse-face 'mode-line-highlight))))))
(add-hook 'find-file-hook #'+mode-line-update-vcs-info)
(add-hook 'after-save-hook #'+mode-line-update-vcs-info)
(advice-add #'vc-refresh-state :after #'+mode-line-update-vcs-info)

(setq-default header-line-format nil)
