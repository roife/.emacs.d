;;; init-modeline.el ---  Initialize modeline configurations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  roife

;; Author: roife <roife@outlook.com>
;; Keywords: lisp, faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(eval-when-compile (require 'init-define))
(require 'cl-lib)

;;;; face
(defgroup roife/modeline nil
  "roife/mode-line faces."
  :group 'faces)

;;;;; winum
(defface roife/modeline-panel-inactive-face
  `((t (:bold t
              :inherit 'font-lock-function-name-face)))
  "The face for winum on the mode-line of an active window."
  :group 'roife/modeline)

;;;;; position
(defface roife/modeline-specific-active-face
  `((t (:background ,(face-background 'mode-line-inactive))))
  "The face for specific parts on the mode-line of an active window."
  :group 'roife/modeline)

;;;;; major-mode
(defface roife/modeline-major-mode-active-face
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group 'roife/modeline)

;;;;; modified
(defface roife/modeline-buffer-modified-face
  '((t (:inherit (warning bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group 'roife/modeline)

(defface roife/modeline-projectile-face
  '((t (:inherit 'font-lock-function-name-face
                 :background nil
                 :bold t)))
  "Face used for the projectile in the mode-line."
  :group 'roife/modeline)

(defface roife/modeline-region-face
  '((t (:inherit 'font-lock-function-name-face
                 :background nil
                 :bold t)))
  "Face used for the region in the mode-line."
  :group 'roife/modeline)

(defface roife/modeline-panel-active-face
  `((t (:background ,(face-foreground 'font-lock-function-name-face)
                    :foreground ,(face-background 'mode-line)
                    :bold t)))
  "Face used for the panel in the mode-line."
  :group 'roife/modeline)

(defface roife/modeline-major-mode-panel-face
  `((t (:background ,(face-foreground 'font-lock-keyword-face)
                    :foreground ,(face-background 'mode-line)
                    :bold t)))
  "Face used for the major-mode panel in the mode-line."
  :group 'roife/modeline)

(defface roife/modeline-rifkey-insert-face
  `((t (:background ,(face-foreground 'font-lock-string-face)
                    :foreground ,(face-background 'mode-line)
                    :bold t)))
  "Face used for rifkey-insert in the mode-line."
  :group 'roife/modeline
  )

(defface roife/modeline-rifkey-normal-face
  `((t (:background ,(face-foreground 'font-lock-function-name-face)
                    :foreground ,(face-background 'mode-line)
                    :bold t)))
  "Face used for rifkey-normal in the mode-line."
  :group 'roife/modeline
  )

(defface roife/modeline-rifkey-visual-face
  `((t (:background ,(face-foreground 'font-lock-builtin-face)
                    :foreground ,(face-background 'mode-line)
                    :bold t)))
  "Face used for rifkey-visual in the mode-line."
  :group 'roife/modeline
  )

(defface roife/modeline-rifkey-window-face
  `((t (:background ,(face-foreground 'font-lock-variable-name-face)
                    :foreground ,(face-background 'mode-line)
                    :bold t)))
  "Face used for rifkey-window in the mode-line."
  :group 'roife/modeline
  )

(defface roife/modeline-rifkey-op-face
  `((t (:background ,(face-foreground 'font-lock-keyword-face)
                    :foreground ,(face-background 'mode-line)
                    :bold t)))
  "Face used for rifkey-op in the mode-line."
  :group 'roife/modeline
  )

(defface roife/modeline-rifkey-others-face
  `((t (:background ,(face-foreground 'font-lock-keyword-face)
                    :foreground ,(face-background 'mode-line)
                    :bold t)))
  "Face used for rifkey-others in the mode-line."
  :group 'roife/modeline
  )

;;;; update modeline
(defun roife/modeline-active-p ()
  (eq roife/current-window (selected-window)))

(add-hook 'buffer-list-update-hook '(lambda () (force-mode-line-update)))

(defun roife/modeline-update-face (active &optional inactive)
  "Return ACTIVE if the current window is selected, if not, return INACTIVE."
  (unless inactive (setq inactive 'mode-line-inactive))
  (if (roife/modeline-active-p) active inactive))

;;;; modules

(defun roife/modeline-module-fill (reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to
                                (- (+ right right-fringe right-margin) ,reserve)))))

(defun roife/modeline-module-flymake ()
  (let* ((running (flymake-running-backends))
         (disabled (flymake-disabled-backends))
         (diags-by-type (make-hash-table)))
    (maphash (lambda (_b state)
               (mapc (lambda (diag)
                       (push diag
                             (gethash (flymake--diag-type diag)
                                      diags-by-type)))
                     (flymake--backend-state-diags state)))
             flymake--backend-state)
    (apply #'concat
           (mapcar (lambda (args)
                     (apply (lambda (num str face)
                              (propertize
                               (format str num) 'face face))
                            args))
                   `((,(length (gethash :error diags-by-type)) "•%d " (:inherit error
                                                                                :background ,(face-background 'mode-line-inactive)))
                     (,(length (gethash :warning diags-by-type)) "•%d " (:inherit warning
                                                                                  :background ,(face-background 'mode-line-inactive)))
                     (,(length (gethash :note diags-by-type)) "•%d" (:inherit success
                                                                              :background ,(face-background 'mode-line-inactive))))))))

(defun roife/modeline-module-file ()
  (let* ((prj-p (when (and (bound-and-true-p projectile-mode)
                           (projectile-project-p))
                  (abbreviate-file-name (projectile-project-p))))
         (prj-dir (file-name-directory ;; get parent
                   (directory-file-name
                    (if prj-p prj-p
                      (abbreviate-file-name default-directory)))))
         (prj-name (if prj-p
                       (file-name-nondirectory
                        (directory-file-name prj-p))
                     (file-name-nondirectory
                      (directory-file-name default-directory))))
         (file-name (file-relative-name
                     buffer-file-name
                     (if prj-p prj-p
                       (abbreviate-file-name default-directory))))
         )
    (concat (propertize (roife/shorten-path prj-dir)
                        'face `(:foreground ,(face-foreground 'mode-line-inactive)
                                            :weight bold))
            (propertize (if (eq prj-dir nil)
                            "~"
                          prj-name)
                        'face 'roife/modeline-projectile-face)
            (propertize "/" 'face `(:weight bold :foreground ,(face-foreground 'mode-line)))
            (propertize file-name 'face `(:weight bold :foreground ,(face-foreground 'mode-line))))
    )
  )

(defun roife/modeline-module-region ()
  (concat (propertize (format "| C:%d W:%d L:%d "
                              (abs (- (mark t) (point)))
                              (count-words (region-beginning) (region-end))
                              (count-lines (region-beginning) (region-end)))
                      'face 'roife/modeline-panel-active-face))
  )

(defun roife/modeline-module-macro ()
  "Display current Emacs macro being recorded."
  (when (roife/modeline-active-p)
    (cond (defining-kbd-macro (propertize "| Def Macro ▶ " 'face 'roife/modeline-panel-active-face))
          (executing-kbd-macro (propertize "| Exec Macro ▶ " 'face 'roife/modeline-panel-active-face))
          ))
  )

(defun roife/modeline-module-major-mode-panel ()
  "Panel for major mode in modeline."
  (let ((info (cond
               ((derived-mode-p 'calc-mode) (prin1-to-string calc-angle-mode))
               (t nil))))
    (when info (propertize (concat " " info  " ")
                           'face (roife/modeline-update-face
                                  'roife/modeline-major-mode-panel-face))))
  )

(defun roife/modeline-module-overwrite-mode-panel ()
  "Overwrite panel for major mode in modeline."
  (when (and overwrite-mode
             (roife/modeline-active-p)) (propertize "| Ovr "
             'face 'roife/modeline-panel-active-face))
  )

(defun roife/modeline-module-rifkey ()
  "Indicator for rifkey in modeline."
  (let ((name (pcase roife/rifkey-mode
                ('insert "I")
                ('normal "N")
                ('visual "V")
                ('window "W")
                ('op "O")
                (_ (s-capitalized-words (prin1-to-string roife/rifkey-mode)))
                ))
        (face (pcase roife/rifkey-mode
                ('insert 'roife/modeline-rifkey-insert-face)
                ('normal 'roife/modeline-rifkey-normal-face)
                ('visual 'roife/modeline-rifkey-visual-face)
                ('window 'roife/modeline-rifkey-window-face)
                ('op 'roife/modeline-rifkey-op-face)
                (_ 'roife/modeline-rifkey-others-face)
                ))
        )
    (when (roife/modeline-active-p)
      (propertize (concat " " name " ") 'face face)))
  )

;;; update
(defun roife/modeline-update ()
  "Update mode-line."
  (let* ((lhs (list
               ;; A window number segment for winum
               '(:eval (when (bound-and-true-p winum-mode)
                         (propertize (concat " " (winum-get-number-string) " ")
                                     'face (roife/modeline-update-face
                                            'roife/modeline-panel-active-face
                                            'roife/modeline-panel-inactive-face))))
               ;; recording macro
               '(:eval (roife/modeline-module-macro))
               ;; overwrite
               '(:eval (roife/modeline-module-overwrite-mode-panel))
               ;; region
               '(:eval (when (and (use-region-p)
                                  (roife/modeline-active-p))
                         (roife/modeline-module-region)))
               ;; modified
               " "
               '(:eval (if (or (buffer-modified-p)
                               buffer-read-only)
                           (propertize "%*" 'face (roife/modeline-update-face
                                                   'roife/modeline-buffer-modified-face))
                         "-"))
               ;; size & buffer-name
               " %I "
               '(:eval (if (buffer-file-name) (roife/modeline-module-file)
                         (propertize "%b" 'face '(:weight bold))))
               ;; TODO: A key mode indicator.
               " "
               '(:eval (roife/modeline-module-rifkey))
               ))
         (rhs (list
               ;; major-mode panel
               '(:eval (roife/modeline-module-major-mode-panel))
               ;; flymake
               '(:eval (when (bound-and-true-p flymake-mode)
                         (concat
                          (propertize " " 'face 'roife/modeline-specific-active-face)
                          (if (eq roife/current-window (selected-window))
                              (roife/modeline-module-flymake)
                            (propertize (roife/modeline-module-flymake) 'face 'roife/modeline-specific-active-face))
                          (propertize " " 'face 'roife/modeline-specific-active-face)
                          )))
               ;; vc-mode
               '(vc-mode vc-mode)
               ;; mode
               " "
               '(:eval (propertize "%m"
                                   'face (roife/modeline-update-face
                                          'roife/modeline-major-mode-active-face)))
               ;; encoding
               "  "
               '(:eval (propertize
                        (let ((buf-coding (format "%s" buffer-file-coding-system)))
                          (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
                              (match-string 1 buf-coding)
                            buf-coding))
                        ))
               ;; line & column
               " "
               '(:eval (propertize " %l: %C " 'face (roife/modeline-update-face 'roife/modeline-specific-active-face)))
               " "
               ;; positon
               '(-3 "%p")
               " "
               ))
         (fill-space (roife/modeline-module-fill (string-width (format-mode-line rhs))))
         )
    (list lhs fill-space rhs)
    ))

(setq-default mode-line-format '(:eval (roife/modeline-update)))

(provide 'init-modeline)
;;; init-modeline.el ends here
