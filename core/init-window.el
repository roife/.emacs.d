;;; init-window.el --- Initialize window configurations.  -*- lexical-binding: t; -*-

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
;;;; Move between windows: [windmove]
(use-package windmove
  :ensure nil
  :bind (("C-s-<up>" . windmove-up)
         ("C-s-<down>" . windmove-down)
         ("C-s-<left>" . windmove-left)
         ("C-s-<right>" . windmove-right)))

;;;; Restore old window configurations: [winner]
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

;;;; Enforce rules for popups: [shackle]
;; (defvar shackle--popup-window-list nil) ; all popup windows
;; (defvar-local shackle--current-popup-window nil) ; current popup window
;; (put 'shackle--current-popup-window 'permanent-local t)

;; (use-package shackle
;;   :commands shackle-display-buffer
;;   :hook (after-init . shackle-mode)
;;   :config
;;   (eval-and-compile
;;     (defun shackle-last-popup-buffer ()
;;       "View last popup buffer."
;;       (interactive)
;;       (ignore-errors
;;         (display-buffer shackle-last-buffer)))
;;     (bind-key "C-h z" #'shackle-last-popup-buffer)

;;     ;; Add keyword: `autoclose'
;;     (defun shackle-display-buffer-hack (fn buffer alist plist)
;;       (let ((window (funcall fn buffer alist plist)))
;;         (setq shackle--current-popup-window window)

;;         (when (plist-get plist :autoclose)
;;           (push (cons window buffer) shackle--popup-window-list))
;;         window))

;;     (defun shackle-close-popup-window-hack (&rest _)
;;       "Close current popup window via `C-g'."
;;       (setq shackle--popup-window-list
;;             (cl-loop for (window . buffer) in shackle--popup-window-list
;;                      if (and (window-live-p window)
;;                              (equal (window-buffer window) buffer))
;;                      collect (cons window buffer)))
;;       ;; `C-g' can deactivate region
;;       (when (and (called-interactively-p 'interactive)
;;                  (not (region-active-p)))
;;         (let (window buffer)
;;           (if (one-window-p)
;;               (progn
;;                 (setq window (selected-window))
;;                 (when (equal (buffer-local-value 'shackle--current-popup-window
;;                                                  (window-buffer window))
;;                              window)
;;                   (winner-undo)))
;;             (setq window (caar shackle--popup-window-list))
;;             (setq buffer (cdar shackle--popup-window-list))
;;             (when (and (window-live-p window)
;;                        (equal (window-buffer window) buffer))
;;               (delete-window window)

;;               (pop shackle--popup-window-list))))))

;;     (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
;;     (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack))

;;   ;; rules
;;   (setq shackle-default-size 0.4)
;;   (setq shackle-default-alignment 'below)
;;   (setq shackle-default-rule nil)
;;   (setq shackle-rules
;;         '(("*Help*" :select t :size 0.3 :align 'below :autoclose t)
;;           ("*compilation*" :size 0.3 :align 'below :autoclose t)
;;           ("*Completions*" :size 0.3 :align 'below :autoclose t)
;;           ("*Pp Eval Output*" :size 15 :align 'below :autoclose t)
;;           ("*ert*" :align 'below :autoclose t)
;;           ("*Backtrace*" :select t :size 15 :align 'below)
;;           ("*Warnings*" :size 0.3 :align 'below :autoclose t)
;;           ("*Messages*" :size 0.3 :align 'below :autoclose t)
;;           ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
;;           ("\\*[Wo]*Man.*\\*" :regexp t :select t :align 'below :autoclose t)
;;           ("*Calendar*" :select t :size 0.3 :align 'below)
;;           (" *undo-tree*" :select t)
;;           ("*Paradox Report*" :size 0.3 :align 'below :autoclose t)
;;           ("*quickrun*" :select t :size 15 :align 'below)
;;           ("*tldr*" :align 'below :autoclose t)
;;           ("*Youdao Dictionary*" :size 0.3 :align 'below :autoclose t)
;;           ("*Finder*" :select t :size 0.3 :align 'below :autoclose t)

;;           (ag-mode :select t :align 'below)
;;           (grep-mode :select t :align 'below)
;;           (ivy-occur-grep-mode :select t :align 'below)
;;           (pt-mode :select t :align 'below)
;;           (rg-mode :select t :align 'below)

;;           (flycheck-error-list-mode :select t :size 0.3 :align 'below :autoclose t)
;;           (flymake-diagnostics-buffer-mode :select t :size 0.3 :align 'below :autoclose t)

;;           (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
;;           (comint-mode :align 'below)
;;           (helpful-mode :select t :size 0.4 :align 'below :autoclose t)
;;           (process-menu-mode :select t :size 0.3 :align 'below :autoclose t)
;;           (list-environment-mode :select t :size 0.3 :align 'below :autoclose t)
;;           (profiler-report-mode :select t :size 0.5 :align 'below)
;;           (tabulated-list-mode :align 'below))))

;;;; Add numbers to each window: [winum]
(use-package winum
  :hook (after-init . winum-mode)
  ;; :init (winum-mode)
  :bind (("s-0" . winum-select-window-0)
         ("s-1" . winum-select-window-1)
         ("s-2" . winum-select-window-2)
         ("s-3" . winum-select-window-3)
         ("s-4" . winum-select-window-4)
         ("s-5" . winum-select-window-5)
         ("s-6" . winum-select-window-6)
         ("s-7" . winum-select-window-7)
         ("s-8" . winum-select-window-8)
         ("s-9" . winum-select-window-9))
  :config (setq winum-auto-setup-mode-line nil))

(provide 'init-window)
;;; init-window.el ends here
