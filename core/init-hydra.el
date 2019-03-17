;;; init-hydra.el --- Initialize hydra configurations.  -*- lexical-binding: t; -*-

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

(use-package hydra)

;;;; navigation
;; (defhydra
;;   (:pre ))

;; (defhydra hydra-reading
;;   (:pre (progn (setq hydra-is-helpful nil) (overwrite-mode -1) (hydra-refresh-mode-line "  [N]  "))
;;         :before-exit (progn (setq hydra-is-helpful t) (hydra-refresh-mode-line "  [I]  "))
;;         :foreign-keys run
;;         :color amaranth
;;         :hint nil)
;;   " "
;;   ("!" shell-command)
;;   ("-" er/expand-region)
;;   ("%" view-jump-brace)
;;   ("/" (progn (toggle-org-hydra) (hydra-push '(hydra-reading/body))) :color teal)
;;   ("." (progn (call-interactively 'avy-goto-char-timer)))
;;   (":" (progn (call-interactively 'eval-expression)))
;;   (";d" delete-window)
;;   (";v" (progn (elfeed) (elfeed-update)) :color blue)
;;   (";e" eval-buffer)
;;   (";t" twit)
;;   (";s" (sx-tab-all-questions t "emacs"))
;;   ("C-c C-]" helm-bibtex :color blue)
;;   ("C-c a" modi/switch-to-scratch-and-back :color blue)
;;   ("C-c z" (call-interactively 'helm-org-dwim))
;;   ("C-M-S-i" (if org-src-mode (org-edit-src-exit) (call-interactively 'narrow-or-widen-dwim)) :color blue)
;;   ("<mouse-1>" mouse-set-point :color blue)
;;   ("<mouse-2>" helm-for-files)
;;   ("<mouse-3>" kill-this-buffer)
;;   ("<" beginning-of-buffer)
;;   (">" end-of-buffer)
;;   ("@" avy-goto-line)
;;   ("A" (progn (beginning-of-line) (indent-according-to-mode)) :color blue)
;;   ("D" kill-line :color blue)
;;   ("E" end-of-line :color blue)
;;   ("F" (progn (call-interactively 'avy-goto-word-1-backward-in-line)))
;;   ("H" (progn (ov-highlight/body) (hydra-push '(hydra-reading/body))) :color teal)
;;   ("I" (progn (forward-char 1)) :color blue)
;;   ("J" (progn (end-of-line) (newline-and-indent)) :color blue)
;;   ("K" (progn (beginning-of-line) (open-line 1) (indent-according-to-mode)) :color blue)
;;   ("N" next-user-buffer)
;;   ("P" previous-user-buffer)
;;   ("S" swiper-all)
;;   ("S-SPC" scroll-down)
;;   ("SPC" scroll-up)
;;   ("T" org-babel-tangle)
;;   ("U" word-example-in-sentence)
;;   ("V" (and (ignore-errors (other-window-for-scrolling) (scroll-other-window-down))))
;;   ("W" backward-word)
;;   ("X" (progn (kill-line 0)))
;;   ("Y" duplicate-line-or-region :color blue)
;;   ("[" org-backward-paragraph)
;;   ("]" org-forward-paragraph)
;;   ("a" (progn (beginning-of-line) (indent-according-to-mode)))
;;   ("b" (progn (ibuffer) (swiper)))
;;   ("c" (progn (overwrite-mode) (hydra-refresh-mode-line "  [C]  ")) :color blue)
;;   ("dd" kill-whole-line)
;;   ("dw" kill-word)
;;   ("df" zap-to-char :color blue)
;;   ("de" kill-line :color blue)
;;   ("da" (progn (kill-line 0) (indent-according-to-mode)) :color blue)
;;   ("dp" duplicate-line-or-region :color blue)
;;   ("e" end-of-line)
;;   ("f" (progn (call-interactively 'avy-goto-word-1-forward-in-line)))
;;   ("g" google-this)
;;   ("h" backward-char)
;;   ("i" nil)
;;   ("j" next-line)
;;   ("k" previous-line)
;;   ("l" forward-char)
;;   ("ma" magit-log-all :color blue)
;;   ("mc" magit-stage-all-and-commit :color blue)
;;   ("mg" magit-status :color blue)
;;   ("md" magit-diff :color blue)
;;   ("ml" magit-log-current :color blue)
;;   ("mt" git-timemachine :color blue)
;;   ("mw" mark-word)
;;   ("ms" mark-sexp)
;;   ("mp" mark-paragraph)
;;   ("n" (progn (ded/org-show-next-heading-tidily)))
;;   ("o" ace-link)
;;   ("p" (progn (ded/org-show-previous-heading-tidily)))
;;   ("r" undo-tree-redo)
;;   ("s" swiper)
;;   ("t" other-window)
;;   ("u" undo)
;;   ("v" (save-excursion (and (ignore-errors (other-window-for-scrolling)) (scroll-other-window))))
;;   ("w" forward-word)
;;   ("x" delete-char)
;;   ("y" yank))

;; (bind-key "<escape>" 'hydra-reading/body)

;; (defun avy-goto-word-1-backward-in-line (char &optional arg)
;;   (interactive (list (read-char "char: " t)
;;                      current-prefix-arg))
;;   (avy-goto-word-1 char arg (point-at-bol) (point) nil))

;; (defun avy-goto-word-1-forward-in-line (char &optional arg)
;;   (interactive (list (read-char "char: " t)
;;                      current-prefix-arg))
;;   (avy-goto-word-1 char arg (point) (point-at-eol) nil))

;; (defun view-jump-brace ()
;;   "Jump to correspondence parenthesis"
;;   (interactive)
;;   (let ((c (following-char))
;;         (p (preceding-char)))
;;     (if (eq (char-syntax c) 40) (forward-list)
;;       (if (eq (char-syntax p) 41) (backward-list)
;;         (backward-up-list)))))

;; (setq cursor-in-non-selected-windows nil)

;; (defvar hydra-stack nil)

;; (defun hydra-push (expr)
;;   (push `(lambda () ,expr) hydra-stack))

;; (defun hydra-pop ()
;;   (interactive)
;;   (let ((x (pop hydra-stack)))
;;     (when x
;;       (funcall x))))

;;;;
(provide 'init-hydra)
;;; init-hydra.el ends here
