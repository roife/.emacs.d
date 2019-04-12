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

(defconst roife/origin-cursor-color (face-attribute 'cursor :background))

;; ;;; thing-edit
;; (defun thing-position (thing)
;;   "Return the beginning and end of `THING'."

;;   )

;; ;;normal
;; (defhydra roife/hydra-rifkey-normal
;;   (:body-pre (progn (setq-default hydra-is-helpful nil
;;                                   roife/rifkey-mode 'normal
;;                                   cursor-type 'box)
;;                     (set-cursor-color "red3"))
;;              :before-exit (progn (setq-default hydra-is-helpful t
;;                                                roife/rifkey-mode 'insert
;;                                                cursor-type 'bar)
;;                                  (set-cursor-color roife/origin-cursor-color))
;;              :color amaranth ;; eq to :foreign-keys warn
;;              )
;;   " "
;;   ;; Mode
;;   ("SPC" roife/hydra-rifkey-visual/body "Visual" :column "Mode" :color blue)
;;   ;; Move
;;   ("n" next-line :column "Move")
;;   ("p" previous-line "↑")
;;   ("f" forward-char "↓")
;;   ("b" backward-char "←")
;;   ("v" scroll-up-command "scr↓")
;;   ("V" scroll-down-command "scr↑")
;;   ("a" mwim-beginning-of-code-or-line "bol")
;;   ("e" mwim-end-of-code-orb-line "eol")
;;   ;; ("<" beginning-of-buffer "bob")
;;   ;; (">" end-of-buffer "eob")
;;   ("A" mwim-beginning-of-code-or-line "bol-e" :color blue)
;;   ("E" mwim-end-of-code-or-line "eol-e" :color blue)
;;   ;; kill
;;   ("kk" (progn (mwim-beginning-of-code-or-line)
;;                (paredit-kill)
;;                (paredit-kill))
;;    "k-l" :column "kill")
;;   ("kw" paredit-forward-kill-word "k-w")
;;   ;; delete
;;   ("d" paredit-forward-delete "d-ch" :column "kill")
;;   ("D" paredit-backward-delete "d-Fch")
;;   ("y" yank "yank")
;;   ;; ("yl"  "k-l")
;;   ;; edit
;;   ("u"   undo-tree-undo "undo" :column "Edit")
;;   ("U"   undo-tree-redo "redo")
;;   ("O" (progn (unless (bolp)
;;                 (beginning-of-line))
;;               (newline)
;;               (forward-line -1)
;;               (indent-according-to-mode)) "o-Fl" :color blue)
;;   ("o" (progn (unless (eolp)
;;                 (end-of-line))
;;               (newline-and-indent)) "o-l" :color blue)
;;   ;; search / replace
;;   ("s" swiper "swiper" :color blue :column "Swiper")
;;   ("S" swiper-all "swiper-all" :color blue)
;;   ;; command
;;   (";f" counsel-find-file "f-file" :color blue :column "command")
;;   (";x" counsel-M-x "M-x" :color blue)
;;   (";s" save-buffer "save-b")
;;   (";b" ivy-switch-buffer "switch-b")
;;   ;; misc
;;   ("?" (progn (setq hydra-is-helpful (not hydra-is-helpful))) "hint" :column "misc")
;;   ("." hydra-repeat "repeat")
;;   ("i"  "insert" :color blue)
;;   ("I" (forward-char) "append" :color blue)
;;   (":" (call-interactively 'eval-expression) :color blue))

;; (bind-key "<escape>" 'roife/hydra-rifkey-normal/body)

(provide 'init-rifkey)
;;; init-rifkey.el ends here
