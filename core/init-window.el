;;; -*- lexical-binding: t -*-

;; [winum]
(use-package winum
  :straight t
  :hook (after-init . winum-mode)
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
  :config (setq winum-auto-setup-mode-line nil)
  )


;; [winner] Restore old window configurations
(use-package winner
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :config
  (setq winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*esh command on file*"))
  )


;; [popper] Enforce rules for popup windows like *Help*
(use-package popper
  :straight t
  :defines popper-echo-dispatch-actions
  :commands popper-group-by-projectile
  :bind (:map popper-mode-map
              ("C-<tab>"   . popper-cycle)
              ("M-`" . popper-toggle-type))
  :hook (emacs-startup . popper-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Calendar\\*"
          "\\*Embark Actions\\*"
          "\\*Finder\\*"
          "\\*Kill Ring\\*"
          "\\*Go-Translate\\*"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode ag-mode pt-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode

          "^\\*Process List\\*" process-menu-mode
          list-environment-mode cargo-process-mode

          "^\\*eshell.*\\*.*$" eshell-mode
          "^\\*shell.*\\*.*$"  shell-mode
          "^\\*terminal.*\\*.*$" term-mode
          "^\\*vterm.*\\*.*$"  vterm-mode
          "^\\*eldoc.*\\*.*$" eldoc-mode

          "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\*$"
          "^\\*elfeed-entry\\*$"
          "^\\*macro expansion\\**"

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
          "\\*docker-.+\\*"
          "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-test-moed))

  (setq popper-echo-dispatch-actions t)

  :config
  ;; Enable echo in minibuffer
  (popper-echo-mode 1)

  ;; Determine the height of popup window WIN by fitting it to the buffer's content.
  (setq popper-window-height
        (lambda ()
          (fit-window-to-buffer
           win
           (floor (frame-height) 3)
           (floor (frame-height) 3))))

  (defun +popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add #'keyboard-quit :before #'+popper-close-window-hack)
  )


;; [zoom] Managing the window sizes automatically
(use-package zoom
  :straight t
  :hook (after-init . zoom-mode))


(provide 'init-window)
