;;; init-ivy.el --- Initialize ivy configurations.   -*- lexical-binding: t; -*-

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

;; Initialize ivy configurations.

;;; Code:
(eval-when-compile (require 'init-define))

;;;; counsel
(use-package counsel
  :defines (projectile-completion-system magit-completing-read-function)
  :bind (("C-x C-b" . ivy-switch-buffer)
         ("C-s" . swiper)
         ("C-S-s" . swiper-all)

         ("C-c v p" . ivy-push-view)
         ("C-c v o" . ivy-pop-view)
         ("C-c v ." . ivy-switch-view)

         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ("C-x C-r" . counsel-recentf)
         ("C-x j" . counsel-mark-ring)

         ("C-c g" . counsel-grep)
         ("C-c h" . counsel-command-history)
         ("C-c i" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c l" . counsel-locate)
         ("C-c r" . counsel-rg)

         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)

         ;; Search at point
         ;; "M-j": word-at-point
         ;; "M-n"/"C-w": symbol-at-point
         ;; Refer to https://www.emacswiki.org/emacs/SearchAtPoint#toc8
         ;; and https://github.com/abo-abo/swiper/wiki/FAQ
         ;; ("C-w" . (lambda ()
         ;;            (interactive)
         ;;            (insert (format "%s" (with-ivy-window (ivy-thing-at-point))))))

         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)

         :map swiper-map
         ("M-%" . swiper-query-replace))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :config
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t)    ; Enable bookmarks and recentf
  (setq ivy-height 20)
  (setq ivy-count-format "(%d / %d) ")
  (setq ivy-on-del-error-function nil)
  ;; (setq ivy-initial-inputs-alist nil)

  (setq ivy-format-function '(lambda (cands)
                               "Transform CANDS into a string for minibuffer."
                               (ivy--format-function-generic
                                (lambda (str)
                                  (concat (if (char-displayable-p ?▶) "▶ " "> ")
                                          (ivy--add-face str 'ivy-current-match)))
                                (lambda (str)
                                  (concat "  " str))
                                cands
                                "\n")))

  (setq swiper-action-recenter t)
  (setq counsel-find-file-at-point t)
  (setq counsel-yank-pop-separator "\n-------\n")

  ;; Use faster search tools: ripgrep or the silver search
  (let ((cmd (cond ((executable-find "rg")
                    "rg -S --no-heading --line-number --color never '%s' %s")
                   (t counsel-grep-base-command))))
    (setq counsel-grep-base-command cmd))

  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  ;; Improve `counsel-ag', also impact `counsel-rg', `counsel-pt'.
  ;; search the selection or current symbol by default
  (eval-and-compile
    (declare-function ivy-thing-at-point 'ivy)
    (defun my-counsel-ag(-counsel-ag &optional initial-input initial-directory extra-ag-args ag-prompt)
      "Search the selection or current symbol via `ag' by default."
      (funcall -counsel-ag
               (or initial-input
                   (if (region-active-p)
                       (buffer-substring-no-properties (region-beginning) (region-end))
                     (ivy-thing-at-point)))
               (or initial-directory default-directory)
               extra-ag-args
               ag-prompt))
    (advice-add #'counsel-ag :around #'my-counsel-ag))
  )

;;;; Richer information: [ivy-rich]
(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

;;;; Enhance fuzzy matching: [flx]
        (use-package flx)

;;;; Enhance M-x: [amx]
        (use-package amx)

        (provide 'init-ivy)
;;; init-ivy.el ends here
