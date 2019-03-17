;;; init-prog.el --- Initialize programming configurations.  -*- lexical-binding: t; -*-

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

;; Initialize programming configurations.

;;; Code:
(eval-when-compile (require 'init-define))

;;;; default
(use-package prog-mode
  :ensure nil
  :hook ((after-init . global-prettify-symbols-mode)
         (emacs-lisp-mode . (lambda () (push '("<=" . ?≤) prettify-symbols-alist)))))

;;;; Run a piece of code conveniently: [quickrun]
(use-package quickrun)

;;;; Jump between source code files: [dumb-jump]
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :hook (prog-mode . dumb-jump-mode)
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (with-eval-after-load 'ivy
    (setq dumb-jump-selector 'ivy))

  ;;;;; hydra
  ;; (defhydra dumb-jump-hydra (:color blue :columns 3)
  ;;   "Dumb Jump"
  ;;   ("j" dumb-jump-go "Go")
  ;;   ("o" dumb-jump-go-other-window "Other window")
  ;;   ("e" dumb-jump-go-prefer-external "Go external")
  ;;   ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
  ;;   ("i" dumb-jump-go-prompt "Prompt")
  ;;   ("b" dumb-jump-back "Back")
  ;;   ("q" nil "quit"))
  ;; (bind-key "C-M-j" #'dumb-jump-hydra/body dumb-jump-mode-map)
  )

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode))

(use-package flymake-diagnostic-at-point
  :after flymake
  :hook
  (flymake-mode . flymake-diagnostic-at-point-mode))


(provide 'init-prog)
;;; init-prog.el ends here
