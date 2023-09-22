;;; -*- lexical-binding: t -*-

(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'cl-lib))

; show encodings for UTF-8:LF
(defvar +modeline-show-common-encodings nil)
; show VC tools name for Git
(defvar +modeline-show-common-vc-tools-name nil)

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
(defconst +modeline-window-width-limit 90)
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
  '((t (:inherit (mode-line-inactive) :inverse-video t)))
  "The face for line number on the mode-line of an active window."
  :group '+modeline)

;; (defface +modeline-vc-mode-active-face
;;   '((t (:inherit (font-lock-constant-face))))
;;   "The face for vc-mode on the mode-line of an active window."
;;   :group '+modeline)

(defface +modeline-meta-active-face
  '((t (:inherit (font-lock-function-name-face bold) :inverse-video t)))
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

;; (defface +modeline-project-name-active-face
;;   '((t (:inherit (bold font-lock-variable-name-face))))
;;   "The face for project name on the mode-line of an active window."
;;   :group '+modeline)
;;
;; (defface +modeline-project-name-inactive-face
;;   '((t (:inherit (mode-line-inactive))))
;;   "The face for project name on the mode-line of an inactive window."
;;   :group '+modeline)

(defface +modeline-buffer-name-active-face
  '((t (:inherit (font-lock-function-name-face bold))))
  "The face for buffer name on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-buffer-name-inactive-face
  '((t (:inherit (mode-line-inactive bold))))
  "The face for buffer name on the mode-line of an inactive window."
  :group '+modeline)

(defface +modeline-host-name-active-face
  '((t (:inherit (font-lock-function-name-face bold italic))))
  "The face for host name on the mode-line of an active window."
  :group '+modeline)

;;; Indicators
(with-eval-after-load 'popper
  ;; modeline indicator
  (setq popper-mode-line
        '(:propertize " POP |"
                      face +modeline-meta-active-face)))

(defsubst +modeline-get-window-name ()
  "Get window name for current window."
  (concat " " (window-parameter (selected-window) 'ace-window-path) " "))

(defsubst +modeline-macro-indicator ()
  "Display current Emacs macro being recorded."
  (cond (defining-kbd-macro "| MacroDef ")
        (executing-kbd-macro "| MacroExc ")))

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

(defsubst +modeline-symbol-overlay-indicator ()
  "Display the number of matches for symbol overlay."
  (when (and (bound-and-true-p symbol-overlay-keywords-alist)
             (not (bound-and-true-p symbol-overlay-temp-symbol)))
    (let* ((keyword (symbol-overlay-assoc (symbol-overlay-get-symbol t)))
           (symbol (car keyword))
           (before (symbol-overlay-get-list -1 symbol))
           (after (symbol-overlay-get-list 1 symbol))
           (count (length before)))
      (if (symbol-overlay-assoc symbol)
          (format (concat  "| %d/%d sym " (and (cadr keyword) "in scope "))
                  (+ count 1)
                  (+ count (length after)))))))


;;; Cache project name
;; (defvar-local +modeline-project-name nil)
;; (defsubst +modeline-update-project-name ()
;;   "Get project name for current buffer."
;;   (setq +modeline-project-name
;;         (when (buffer-file-name)
;;           (when-let ((project (project-current)))
;;             (concat " "
;;                     (file-name-nondirectory
;;                      (directory-file-name (project-root project))))))))
;; (add-hook 'find-file-hook #'+modeline-update-project-name)
;; (add-hook 'after-change-major-mode-hook #'+modeline-update-project-name)

;;; Cache remote host name
(defvar-local +modeline-remote-host-name nil)
(defsubst +modeline-update-remote-host-name ()
  "Hostname for remote buffers."
  (setq +modeline-remote-host-name
        (when-let ((hostname (and default-directory
                              (file-remote-p default-directory 'host))))
          (when (not (string-equal hostname "localhost"))
            (format "@%s" hostname)))
        ))
(add-hook 'find-file-hook #'+modeline-update-remote-host-name)

;;; Cache flymake report
(defvar-local +modeline-flymake-indicator nil)
(defun +modeline-flymake-update (&rest _)
  "Display flymake info for current buffer."
  (setq +modeline-flymake-indicator
        (when (and flymake-mode (flymake-running-backends))
          (let* ((err-count (cadadr (flymake--mode-line-counter :error)))
                 (warning-count (cadadr (flymake--mode-line-counter :warning)))
                 (note-count (cadadr (flymake--mode-line-counter :note)))
                 (err (when err-count (propertize err-count 'face '(:inherit compilation-error))))
                 (warning (when warning-count (propertize (concat " " warning-count) 'face '(:inherit compilation-warning))))
                 (note (when note-count (propertize (concat " " note-count) 'face '(:inherit compilation-info)))))
            (concat " [" err warning note "]"))))
  )
(advice-add #'flymake--handle-report :after #'+modeline-flymake-update)
(add-hook 'flymake-mode-hook #'+modeline-flymake-update)

;;; Cache encoding info
(defvar-local +modeline-encoding nil)
(defsubst +modeline-update-encoding (&rest _)
  "Get encoding and EOL type of current buffer."
  (setq +modeline-encoding
        `(,(if (memq (coding-system-category buffer-file-coding-system)
                     '(coding-category-undecided coding-category-utf-8))
               (when +modeline-show-common-encodings "UTF-8")
             (upcase (symbol-name (coding-system-get buffer-file-coding-system :name))))
          ,(pcase (coding-system-eol-type buffer-file-coding-system)
             (0 (when +modeline-show-common-encodings ":LF "))
             (1 ":CRLF ")
             (2 ":CR ")
             (_ " ")))))
(add-hook 'find-file-hook #'+modeline-update-encoding)
(advice-add #'after-insert-file-set-coding :after #'+modeline-update-encoding)
(advice-add #'set-buffer-file-coding-system :after #'+modeline-update-encoding)

(defsubst +mode-line-active-long ()
  "Formatting active-long modeline."
  (let* ((lhs `((:propertize ,(+modeline-get-window-name)
                             face +modeline-meta-active-face)
                (:propertize ,(when (+modeline-window-active-p)
                                (concat (+modeline-macro-indicator)
                                        (+modeline-multiple-cursors-indicator)
                                        (+modeline-symbol-overlay-indicator)
                                        (+modeline-use-region-indicator)
                                        (+modeline-overwrite-indicator)))
                             face +modeline-meta-active-face)
                (:propertize " %*" face +modeline-modification-active-face)
                " %I "
                (:propertize "%b" face +modeline-buffer-name-active-face)
                (:propertize +modeline-remote-host-name
                             face +modeline-host-name-active-face)
                ))
         (rhs '((:propertize vc-mode face +modeline-vc-mode-active-face)
                " "
                (:propertize mode-name
                             face +modeline-buffer-name-active-face)
                (:eval +modeline-flymake-indicator)
                " "
                (:eval +modeline-encoding)
                (:propertize " %l:%C "
                             face +modeline-line-number-active-face)
                " "
                (-3 "%p")
                "%%"))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,rhs-w))))
      ,rhs-str)))


(defsubst +mode-line-inactive-long ()
  "Formatting active-long modeline."
  (let* ((lhs `((:propertize ,(+modeline-get-window-name)
                             face +modeline-meta-inactive-face)
                "%* %I "
                (:propertize "%b" face +modeline-buffer-name-active-face)
                ))
         (rhs '((:eval vc-mode)
                " "
                (:eval mode-name)
                " "
                (:eval +modeline-encoding)
                "%l:%C "
                (-3 "%p")
                "%%"))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,rhs-w))))
      ,rhs-str)))


(defsubst +mode-line-active-short ()
  "Formatting active-long modeline."
  (let* ((lhs `((:propertize ,(+modeline-get-window-name)
                             face +modeline-meta-active-face)
                (:propertize ,(when (+modeline-window-active-p)
                                (concat (+modeline-macro-indicator)
                                        (+modeline-multiple-cursors-indicator)
                                        (+modeline-symbol-overlay-indicator)
                                        (+modeline-use-region-indicator)
                                        (+modeline-overwrite-indicator)))
                             face +modeline-meta-active-face)
                (:propertize " %*" face +modeline-modification-active-face)
                " "
                (:propertize "%b" face +modeline-buffer-name-active-face)
                (:propertize +modeline-remote-host-name
                             face +modeline-host-name-active-face)
                ))
         (rhs '((:propertize mode-name
                             face +modeline-buffer-name-active-face)
                " "
                (:eval +modeline-encoding)
                (:propertize " %l "
                             face +modeline-line-number-active-face)
                " "
                (-3 "%p")
                "%%"))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,rhs-w))))
      ,rhs-str)))

(defsubst +mode-line-inactive-short ()
  "Formatting active-long modeline."
  (let* ((lhs `((:propertize ,(+modeline-get-window-name)
                             face +modeline-meta-inactive-face)
                "%* "
                (:propertize "%b" face +modeline-buffer-name-active-face)
                (:propertize +modeline-remote-host-name
                             face +modeline-host-name-active-face)
                ))
         (rhs '(" %l  "
                (-3 "%p")
                "%%"))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,rhs-w))))
      ,rhs-str)))

(setq-default mode-line-format
              '((:eval (if (+modeline-window-active-p)
                           (if +modeline-large-width-p (+mode-line-active-long) (+mode-line-active-short))
                         (if +modeline-large-width-p (+mode-line-inactive-long) (+mode-line-inactive-short))))))


;; Cache project info
(defvar-local +mode-line-project-info nil)
(defsubst +mode-line-update-project-info ()
  (setq +mode-line-project-info
        ))

(setq-default header-line-format nil)

(defsubst +header-line-update ()
  (setq-local header-line-format
              (when (and buffer-file-name (project-current))
                '((:eval (let* ((lhs '(" "
                                       (:eval breadcrumb--header-line)))
                                (rhs '((:eval vc-mode)
                                       " "))
                                (rhs-str (format-mode-line rhs))
                                (rhs-w (string-width rhs-str)))
                           `(,lhs
                             ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,rhs-w))))
                             ,rhs-str))))))
  )
(add-hook 'find-file-hook #'+header-line-update)
(add-hook 'after-change-major-mode-hook #'+header-line-update)
