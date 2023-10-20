;;; -*- lexical-binding: t; -*-

(defvar meow-leave-insert-mode-hook nil
  "Hook to run when leaving meow insert mode.")
(defvar meow-enter-insert-mode-hook nil
  "Hook to run when entering meow insert mode.")
(add-hook! 'meow-insert-mode-hook
  (defun +meow-insert-mode-run-hook-on-mode ()
    (run-hooks
     (if meow-insert-mode 'meow-enter-insert-mode-hook
       'meow-leave-insert-mode-hook))))

(defvar meow-leave-motion-mode-hook nil
  "Hook to run when leaving meow insert mode.")
(defvar meow-enter-motion-mode-hook nil
  "Hook to run when entering meow insert mode.")
(add-hook! 'meow-motion-mode-hook
  (defun +meow-motion-mode-run-hook-on-mode ()
    (run-hooks
     (if meow-motion-mode 'meow-enter-motion-mode-hook
       'meow-leave-motion-mode-hook))))

;; [sis] automatically switch input source
(use-package sis
  :straight t
  :hook (;; Enable the inline-english-mode for all buffers.
         ;; When add space after chinese char, automatically switch to english mode
         (after-init . sis-global-inline-mode)
         ;; Enable the context-mode for all buffers
         (after-init . sis-global-context-mode)
         ;; Auto refresh sis state
         (after-init . sis-auto-refresh-mode)
         ;; Respect mode
         ;; (after-init . sis-global-respect-mode)
         )
  :config
  (setq sis-english-source "com.apple.keylayout.ABC"
        sis-inline-tighten-head-rule nil
        sis-prefix-override-keys (list "C-c" "C-x" "C-h" "M-x"))
  (cond
   ((eq system-type 'darwin)
    (sis-ism-lazyman-config
     "com.apple.keylayout.ABC"
     "com.apple.inputmethod.SCIM.Shuangpin" 'emp))
   ((eq system-type 'gnu/linux)
    (sis-ism-lazyman-config "1" "2" 'fcitx5)))

  ;; Meow
  (add-hook 'meow-leave-insert-mode-hook #'sis-set-english)
  (add-to-list 'sis-context-hooks 'meow-enter-insert-mode-hook)

  (add-hook 'meow-enter-motion-mode-hook #'sis-set-english)
  (add-to-list 'sis-context-hooks 'meow-leave-motion-mode-hook)

  ;; modes to switch to chinese input source
  (defun +sis-switch-to-chinese-detector (&rest _)
    (when (or (eq major-mode 'org-mode)
              (eq major-mode 'telega-chat-mode))
      'other))

  (add-to-list 'sis-context-detectors #'+sis-switch-to-chinese-detector)
  (advice-add 'org-agenda-todo :before #'sis-set-english)
  )
