;;; -*- lexical-binding: t -*-

(require 'cl-lib)

;; [hide-mode-line] Hide modeline in certain major modes
(use-package hide-mode-line
  :straight t
  :hook (((
           ;; completion-list-mode
           ;; completion-in-region-mode
           eshell-mode
           shell-mode
           term-mode
           vterm-mode
           pdf-annot-list-mode
           flycheck-error-list-mode) . hide-mode-line-mode)))

;; [modeline] Modeline

;;; Get current window
(defvar +modeline-current-window nil)

(defsubst +modeline-get-current-window-exclude-child-window (&optional frame)
  "Get the current window but should exclude the child windows.
If FRAME is nil, it means the current frame."
  (frame-selected-window (or (frame-parent frame) frame)))

(defun +modeline-set-selected-window (&rest _)
  "Set `+modeline-current-window' appropriately."
  (let ((win (+modeline-get-current-window-exclude-child-window)))
    (setq +modeline-current-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))

(defsubst +modeline-window-active-p ()
  "Whether is an active window."
  (eq (+modeline-get-current-window-exclude-child-window) +modeline-current-window))

(add-hook 'pre-redisplay-functions #'+modeline-set-selected-window)


;;; Faces for modeline

(defsubst +modeline-update-face (active &optional inactive)
  "Return ACTIVE if the current window is selected, if not, return INACTIVE."
  (if (+modeline-window-active-p)
      active
    (or inactive 'mode-line-inactive)))


;;;; face
(defgroup +modeline nil
  "Modeline faces."
  :group 'faces)

(defface +modeline-line-number-active-face
  `((t (:background ,(face-background 'mode-line-inactive))))
  "The face for specific parts on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-vc-mode-active-face
  '((t (:inherit (bold font-lock-constant-face))))
  "The face for specific parts on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-meta-active-face
  `((t (:background ,(face-foreground 'font-lock-function-name-face)
                    :foreground ,(face-background 'mode-line))))
  "Face used for the panel in the mode-line."
  :group '+modeline)

(defface +modeline-meta-inactive-face
  '((t (:inherit (bold font-lock-function-name-face))))
  "The face for winum on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-modification-active-face
  '((t (:inherit (bold font-lock-function-name-face))))
  "The face for winum on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-buffer-name-active-face
  '((t (:inherit (bold font-lock-function-name-face))))
  "The face for winum on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-buffer-name-inactive-face
  `((t (:foreground ,(face-foreground 'mode-line)
                    :bold t)))
  "The face for winum on the mode-line of an active window."
  :group '+modeline)

(defface +modeline-encoding-face
  '((t (:inherit (italic))))
  "The face for winum on the mode-line of an active window."
  :group '+modeline)

;;; Indicators
(defsubst +modeline-macro-indicator ()
  "Display current Emacs macro being recorded."
  (cond (defining-kbd-macro "| Macro ▶ ")
        (executing-kbd-macro "| Macro ▷ ")))

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

(defsubst +modeline-multiple-cursors-indicator ()
  "Display the number of multiple cursors."
  (when (bound-and-true-p multiple-cursors-mode)
    (format "| %d cursors " (mc/num-cursors))))

(defsubst +modeline-use-region-indicator ()
  "Display selected region in current buffer."
  (when (use-region-p)
    (concat (format "| L%d W%d C%d "
                    (count-lines (region-beginning) (region-end))
                    (count-words (region-beginning) (region-end))
                    (abs (- (mark t) (point)))))))

(defsubst +modeline-modal-indicator ()
  ""
  "")

(defsubst +modeline-project-name ()
  "Get project name for current buffer."
  (when-let ((project (project-current)))
    (concat "["
            (file-name-nondirectory
             (directory-file-name (project-root project)))
            "] ")))

(defsubst +modeline-flymake ()
  "Display flymake info for current buffer."
  (let* ((err (propertize (cadadr (flymake--mode-line-counter :error t))
                          'face (+modeline-update-face '(:inherit error))))
         (warning (propertize (cadadr (flymake--mode-line-counter :warning t))
                              'face (+modeline-update-face '(:inherit warning))))
         (note (propertize (cadadr (flymake--mode-line-counter :note t))
                           'face (+modeline-update-face '(:inherit success))))
         (str (concat " ["  err ":" warning ":" note "]")))
    str))

(defsubst +modeline-encoding ()
  "Get encoding and EOL type of current buffer."
  (concat (let* ((sys (coding-system-plist buffer-file-coding-system))
                 (sym (if (memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                          'utf-8
                        (plist-get sys :name))))
            (upcase (symbol-name sym)))
          (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "/LF")
            (1 "/CRLF")
            (2 "/CR")
            (_ ""))
          ))

(defsubst +format-mode-line ()
  "Formatting modeline."
  (let* ((lhs `(;; ace window
                (:propertize ,(concat " "
                                      (window-parameter (selected-window) 'ace-window-path)
                                      " ")
                             face ,(+modeline-update-face
                                    '+modeline-meta-active-face
                                    '+modeline-meta-inactive-face))
                ;; indicators
                (:propertize ,(when (+modeline-window-active-p)
                                (concat (+modeline-macro-indicator)
                                        (+modeline-anzu-indicator)
                                        (+modeline-multiple-cursors-indicator)
                                        (+modeline-symbol-overlay-indicator)
                                        (+modeline-use-region-indicator)))
                             face +modeline-meta-active-face)
                ;; modified
                (:propertize " %*" face +modeline-modification-active-face)
                ;; buffer size
                " %I"
                ;; buffer name
                (:propertize ,(concat  " " (+modeline-project-name) "%b")
                             face ,(+modeline-update-face
                                    '+modeline-buffer-name-active-face
                                    '+modeline-buffer-name-inactive-face))
                ;; TODO: meow
                ))
         (rhs `(;; major mode
                (:propertize mode-name
                             face ,(+modeline-update-face
                                    '+modeline-buffer-name-active-face
                                    '+modeline-buffer-name-inactive-face))
                ;; flymake
                (:eval (when (bound-and-true-p flymake-mode)
                         (+modeline-flymake)))
                ;; vcs
                (:propertize vc-mode
                             face ,(+modeline-update-face '+modeline-vc-mode-active-face))
                ;; encoding
                " "
                (:eval (+modeline-encoding))
                ;; line & col
                " "
                (:propertize " %l:%C "
                             face +modeline-line-number-active-face)
                ;; position
                " "
                (-3 "%p")
                "%%"))
         (lhs-str (format-mode-line lhs))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    (format "%s%s%s"
            lhs-str
            (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,rhs-w)))))
            rhs-str)))
(setq-default mode-line-format '((:eval (+format-mode-line))))
(setq-default header-line-format nil)

(provide 'init-modeline)
