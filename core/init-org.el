;;; init-org.el --- Initialize org configurations.   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  roife

;; Author: roife <roife@outlook.com>
;; Keywords: lisp

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

(defun org-paste-image ()
  "insert images from clipboard"
  (interactive)
  (let* ((path (concat default-directory "img/"))
         (image-file (concat
                      path
                      (buffer-name)
                      (format-time-string "_%Y%m%d_%H%M%S.png"))))
    (if (not (file-exists-p path))
        (mkdir path))
    (do-applescript (concat
                     "set the_path to \"" image-file "\" \n"
                     "set png_data to the clipboard as «class PNGf» \n"
                     "set the_file to open for access (POSIX file the_path as string) with write permission \n"
                     "write png_data to the_file \n"
                     "close access the_file"))
    (org-insert-link nil (concat "file:" image-file) "")
    (org-display-inline-images)
    (message image-file))
  )

(use-package org
  :ensure nil
  :bind (("C-c i" . 'org-paste-image))
  :config
  (add-hook 'org-mode-hook (lambda () (define-key org-mode-map (kbd "C-c i") 'org-insert-image)))

  ;; [capture]
  (setq org-agenda-dir "~/Documents/org/gtd"
        org-agenda-file-inbox (expand-file-name "inbox.org" org-agenda-dir)
        org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir)
        org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir)
        org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir)
        org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir)
        org-agenda-file-finish (expand-file-name "finish.org" org-agenda-dir)
        org-agenda-file-abort (expand-file-name "abort.org" org-agenda-dir)
        org-agenda-files (list org-agenda-dir))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-agenda-file-inbox "ToDo")
           "* TODO [#B] %?\n"
           :empty-lines 1)

          ("p" "Paste" entry (file+headline org-agenda-file-note "Collect")
           "* %?\n %c\n %U"
           :empty-lines 1)

          ("i" "Ideas" entry (file+headline org-agenda-file-gtd "ideas")
           "* %?\n %i\n %U"
           :empty-lines 1)

          ("c" "Code Snippet" entry (file org-agenda-file-code-snippet)
           "* %?%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")

          ("r" "Reading List" entry (file+headline org-agenda-file-gtd "Reading List")
           "* TODO [#C] [[%c][%?]]\n %i"
           :empty-lines 1)

          ("j" "Journal"
           entry (file+datetree org-agenda-file-journal)
           "* %?"
           :empty-lines 1)))
  ;; [agenda]
  (setq org-agenda-custom-commands
        '(("w" . "任务安排")
          ("wa" "重要&紧急" tags-todo "+PRIORITY=\"A\"")
          ("wb" "重要&不紧急" tags-todo "PRIORITY=\"B\"")
          ("wc" "不重要&紧急" tags-todo "+PRIORITY=\"C\"")
          ("W" "Weekly Review" ((stuck "")
                                (tags-todo "PROJECT")))))

  (setq org-agenda-inhibit-startup t) ;; ~50x speedup
  (setq org-agenda-span 'day)
  (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
  (setq org-agenda-window-setup 'current-window)
  (setq org-log-done t)

  (setq org-use-fast-todo-selection t) ;; fast choose status
  (setq org-enforce-todo-dependencies t) ;; only when child-task completed could parent be completed
  (setq org-todo-keywords
        '((sequence "TODO(t)" "Doing(i!)" "Waiting(w!/!)" "Someday(s@)" "|" "DONE(d!/@)" "ABOUT(a@/!)")))
  (setq org-todo-keyword-faces '(("TODO" . '(:foreground "#cc6666"))
                                 ("Doing" . '(:foreground "#f0c674"))
                                 ("DONE" . '(:foreground "#81a2be"))
                                 ("Someday" . '(:foreground "#b5bd68"))
                                 ("Waiting" . '(:foreground "#b294bb"))
                                 ("ABOUT" . (:foreground "#c5c8c6"))))

  (setq org-refile-targets '((org-agenda-file-finish :maxlevel . 1)
                             (org-agenda-file-abort :maxlevel . 1)))
  ;; [better-default]
  (setq org-src-fontify-natively t) ; code syntax highlighting
  (setq org-support-shift-select t) ; use shift to select in org-mode
  (setq org-image-actual-width '(600)) ; set inline image size
  (setq org-startup-indented t) ; indent in org

  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :bold t :height 1.3))))
   '(org-level-2 ((t (:inherit outline-2 :bold t :height 1.2))))
   '(org-level-3 ((t (:inherit outline-3 :bold t :height 1.1))))
   '(org-level-4 ((t (:inherit outline-4 :bold t :height 1.1))))
   '(org-level-5 ((t (:inherit outline-5 :bold t :height 1.1))))
   '(org-level-6 ((t (:inherit outline-6 :bold t :height 1.1))))
   '(org-level-7 ((t (:inherit outline-7 :bold t :height 1.1))))
   '(org-level-8 ((t (:inherit outline-8 :bold t :height 1.1))))
   )

  (custom-set-faces
   '(org-block ((t (:background "#292F39"))))
   '(org-block-begin-line ((t (:foreground "#55595F" :background "#292F39"))))
   '(org-block-end-line ((t (:foreground "#55595F" :background "#292F39")))))
  )
(use-package olivetti
  :hook ((org-mode . olivetti-mode))
  :config
  (setq olivetti-body-width 100))

(use-package org-bullets
  :if (char-displayable-p ?◉)
  :hook (org-mode . org-bullets-mode))

(use-package org-rich-yank
  :bind (:map org-mode-map
              ("C-M-y" . org-rich-yank)))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package org-plus-contrib)

(add-hook 'org-mode-hook #'(lambda ()
                             (setq line-spacing 2)))

(provide 'init-org)
;;; init-org.el ends here
