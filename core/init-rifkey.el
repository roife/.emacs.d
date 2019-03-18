;;; init-rifkey.el --- Initialize rifkey configurations.  -*- lexical-binding: t; -*-

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

;;normal
(defhydra roife/hydra-rifkey-normal
  (:body-pre (progn (setq-default hydra-is-helpful nil
                                  roife/rifkey-mode 'normal
                                  cursor-type 'box))
             :before-exit (progn (setq-default hydra-is-helpful t
                                               roife/rifkey-mode 'insert
                                               cursor-type 'bar)) ;; TODO
             :color amaranth ;; eq to :foreign-keys warn
             )
  " "
  ;; Mode
  ("SPC" roife/hydra-rifkey-visual/body "Visual" :column "Mode" :color blue)
  ;; Move
  ("n" next-line "↓" :column "Move")
  ("p" previous-line "↑")
  ("f" forward-char "↓")
  ("b" backward-char "←")
  ("v" scroll-up-command "scroll↓")
  ("V" scroll-down-command "scroll↑")
  ("a" mwim-beginning-of-code-or-line "bol")
  ("e" mwim-end-of-code-or-line "eol")
  ("A" mwim-beginning-of-code-or-line "bol-edit")
  ("E" mwim-end-of-code-or-line "beol-edit")
  ("l" recenter-top-bottom "recenter")
  ;; kill
  ("kl" (progn (mwim-beginning-of-code-or-line) (paredit-kill)))
  ;; undo
  ("u"   undo-tree-undo "undo" :column "Undo")
  ("U"   undo-tree-redo "redo")
  ;; search / replace
  ("s"   swiper :color blue :column "Search")
  ;; help
  ("?" (progn (setq hydra-is-helpful (not hydra-is-helpful))) "hint" :column "help")
  ("i" nil "insert mode")
  )
(bind-key "<escape>" 'roife/hydra-rifkey-normal/body)

(defhydra roife/hydra-rifkey-visual
  (:body-pre (progn (setq-default hydra-is-helpful nil
                                  roife/rifkey-mode 'visual
                                  cursor-type 'box)
                    (call-interactively 'set-mark-command))
             :before-exit (progn (setq-default hydra-is-helpful t
                                               roife/rifkey-mode 'insert
                                               cursor-type 'bar)
                                 (deactivate-mark)) ;; TODO
             :color amaranth ;; eq to :foreign-keys warn
             )
  " "
  ;; Mode
  ("<escape>" roife/hydra-rifkey-normal/body "Normal" :column "Mode" :color blue)
  ;; Move
  ("n" next-line "↓" :column "Move")
  ("p" previous-line "↑")
  ("f" forward-char "↓")
  ("b" backward-char "←")
  ("v" scroll-up-command "scroll↓")
  ("V" scroll-down-command "scroll↑")
  ("a" mwim-beginning-of-code-or-line "bol")
  ("e" mwim-end-of-code-or-line "eol")
  ;; mark
  ("l" mark-line "line")
  ;; expand-region
  ("SPC" er/expand-region "expand" :column "er")
  ("S-SPC" er/contract-region "contract")
  ("d" er/mark-defun "func")
  ("w" er/mark-word "word")
  ("u" er/mark-url "url")
  ("e" mark-sexp "s-exp")
  ("E" er/mark-email "email")
  ("P" er/mark-text-paragraph "paragraph")
  ("s" er/mark-symbol "symbol")
  ("S" er/mark-symbol-with-prefix "pre-symbol")
  ;; brackets
  ("q" er/mark-inside-quotes "quotes-in" :column "brackets")
  ("Q" er/mark-outside-quotes "quotes-out")
  ("[" er/mark-inside-pairs "pairs-in")
  ("]" er/mark-outside-pairs "pairs-out")
  ("t" er/mark-inner-tag "tag-in")
  ("T" er/mark-outer-tag "tag-out")
  ("c" er/mark-comment "Comment")
  ("a" er/mark-html-attribute "HTML attribute")
  ;; search / replace
  ;; ("s" (progn (swiper ())) :color blue :column "Search")
  ;; edit
  ("k" kill-region "kill" :column "edit" :color blue)
  ;; help
  ("?" (progn (setq hydra-is-helpful (not hydra-is-helpful))) "hint" :column "help")
  ("i" delete-active-region "insert mode" :color blue)
  )

;; (defhydra hydra-reading
;; (:pre (progn (setq hydra-is-helpful nil) (overwrite-mode -1) (hydra-refresh-mode-line "  [N]  "))
;;       :before-exit (progn (setq hydra-is-helpful t) (hydra-refresh-mode-line "  [I]  "))
;;       :foreign-keys run
;;       :color amaranth
;;       :hint nil)
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
(provide 'init-rifkey)
;;; init-rifkey.el ends here
