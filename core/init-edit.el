;;; init-edit.el --- Initialize editing configurations.  -*- lexical-binding: t; -*-

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

;; Initialize editing configurations.

;;; Code:
(eval-when-compile (require 'init-define))

;;;; Coding System
;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))

;;;; Misc
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same

(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$")

(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder

(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

;;;; Tab & Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)


;;;; Selected region
;;;;; Delete selection if you insert: [delsel]
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;;;;; Rectangle: [rect]
(use-package rect
  :ensure nil
  :bind (("<C-return>" . rectangle-mark-mode)))

;;;;; Drag stuff (lines, words, region, etc...) around: [drag-stuff]
(use-package drag-stuff
  :commands drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;;;;; Expand region: [expand-region]
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;;; URL
;;;;; Pass a URL to a WWW browser: [browse-url]
(use-package browse-url
  :ensure nil
  :defines dired-mode-map
  :bind (("C-c C-z ." . browse-url-at-point)
         ("C-c C-z b" . browse-url-of-buffer)
         ("C-c C-z r" . browse-url-of-region)
         ("C-c C-z u" . browse-url)
         ("C-c C-z v" . browse-url-of-file))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map)))

;;;;; Click to browse URL or to send to e-mail address: [goto-addr]
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;;;; Avy
;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :config (setq avy-background t))

;;;;; Kill text between the point and the character CHAR: [avy-zap]
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;;;;; Quickly follow links: [ace-link]
(use-package ace-link
  :bind (("M-o" . ace-link-addr))
  :hook (after-init . ace-link-setup-default))

;;;;; Jump to Chinese characters: [ace-pinyin]
(use-package ace-pinyin
  :hook (after-init . ace-pinyin-global-mode))

;;;; Programming
;;;;; Minor mode to aggressively keep your code always indented: [aggressive-indent]
(use-package aggressive-indent
  :hook ((after-init . global-aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode robot-mode go-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or (derived-mode-p 'c-mode)
             (derived-mode-p 'c++-mode)
             (derived-mode-p 'csharp-mode)
             (derived-mode-p 'java-mode)
             (derived-mode-p 'go-mode)
             (derived-mode-p 'swift-mode))
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;; ;;;;; Use org-mode in comment: [outshine]
;; (use-package outshine
;;   :hook ((prog-mode . outshine-mode)))

;;;;; [Paredit]
(use-package paredit
  :hook (prog-mode . paredit-mode)
  :config
  ;; Don't insert space automatically
  (add-to-list 'paredit-space-for-delimiter-predicates
               '(lambda (endp delim)
                  (derived-mode-p 'lisp-mode
                                  'lisp-interaction-mode
                                  'emacs-lisp-mode)))
  (define-key paredit-mode-map ";" nil)
  (define-key paredit-mode-map (kbd "M-<up>") nil)
  (define-key paredit-mode-map (kbd "M-<down>") nil)
  ;; Delete region
  (define-key paredit-mode-map (kbd "DEL") '(lambda () (interactive)
                                              (if (use-region-p) (paredit-delete-region (region-beginning) (region-end))
                                                (paredit-backward-delete))))
  )

;;;;; Comment: [comment-dwim-2]
(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2))

;; better C-a C-e for programmming
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Framework for mode-specific buffer indexes
(use-package imenu
  :ensure nil
  :bind (("C-." . imenu)))

;; Hungry deletion
;; ISSUE: Hungry delete doesn't work with paredit
;; See: https://emacs.stackexchange.com/questions/33734/how-to-get-hungry-delete-working-in-paredit-mode
(setq backward-delete-char-untabify-method 'all)
;; (use-package hungry-delete
;;   :diminish
;;   :hook (after-init . global-hungry-delete-mode)
;;   :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; Goto last change
(use-package goto-chg
  :bind ("C-," . goto-last-change))

;; Undo-tree
(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :bind (("C-x u" . undo-tree-visualize)))

;; 移动时把包含大写字母的单词看成多单词
(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; auto-insert
(use-package auto-insert-mode
  :ensure nil
  :hook (after-init . auto-insert-mode)
  :config
  (setq auto-insert-directory "~/.emacs.d/template/"
        auto-insert-query nil ;; If you don't want to be prompted before insertion
        auto-insert-alist (append '(
                                    ;; (c++-mode . "template.cpp")
                                    )
                                  auto-insert-alist)
        )
  )

(use-package visual-line-mode
  :ensure nil
  :hook (after-init . visual-line-mode)
  )

(use-package hideshow
  :ensure nil
  :hook ((prog-mode . hs-minor-mode))
  :bind (:map hs-minor-mode-map
              ("C-`" . hs-toggle-hiding)))

;; [Spell]
(use-package flyspell
  :ensure nil
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init
  (setq flyspell-issue-message-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

;; []
(use-package multiple-cursors
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("s-<mouse-1>"   . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space))
  :config (setq mc/cmds-to-run-for-all '(mwim-beginning-of-code-or-line
                                         mwim-end-of-code-or-line))
  )

(use-package page-break-lines
  :hook ((after-init . global-page-break-lines-mode))
  )

(use-package projectile
  :hook (after-init . projectile-mode))

;; TODO: anzu(setq isearch-lazy-count t)

(provide 'init-edit)
;;; init-edit.el ends here
