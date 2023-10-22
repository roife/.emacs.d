;;; -*- lexical-binding: t; -*-

;; [sis] automatically switch input source
(defvar meow-leave-insert-mode-hook nil
  "Hook to run when leaving meow insert mode.")
(defvar meow-enter-insert-mode-hook nil
  "Hook to run when entering meow insert mode.")
(add-hook! 'meow-insert-mode-hook
  (defun +meow-insert-mode-run-hook-on-mode ()
    (run-hooks
     (if meow-insert-mode 'meow-enter-insert-mode-hook
       'meow-leave-insert-mode-hook))))

;; `defmacro' cannot be placed in use-package
;; (defmacro +sis-add-post-cmd-hook! (modes func)
;;   "Add post-command-hook to MODES."
;;   (declare (indent defun))
;;   (let ((func-name (cadr func)))
;;     `(add-hook! ,modes
;;        (defun ,(intern (format "%s-add-post-cmd-hook" func-name)) ()
;;          (add-hook! 'post-command-hook :local
;;            ,func)))))

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
         (after-init . sis-global-respect-mode)
         ;; Colored cursor
         (after-init . sis-global-cursor-color-mode))
  :config
  (setq sis-english-source "com.apple.keylayout.ABC"
        sis-inline-tighten-head-rule nil
        sis-prefix-override-keys (list "C-c" "C-x" "C-h" "C-,"))

  ;; HACK: Set cursor color automatically
  (add-hook! '+theme-changed-hook :call-immediately
    (defun +sis-set-other-cursor-color ()
      (setq sis-other-cursor-color (face-attribute 'success :foreground))))

  ;; Add IME according to system type
  (cond
   ((eq system-type 'darwin)
    (sis-ism-lazyman-config
     "com.apple.keylayout.ABC"
     "com.apple.inputmethod.SCIM.Shuangpin"
     'emp))
   ((eq system-type 'gnu/linux)
    (sis-ism-lazyman-config "1" "2" 'fcitx5)))

  ;; Context mode
  (add-hook 'meow-leave-insert-mode-hook #'sis-set-english)
  (add-to-list 'sis-context-hooks 'meow-enter-insert-mode-hook)

  (defun +sis-context-switching (back-detect fore-detect)
    (when (and meow-insert-mode
               (or (and (derived-mode-p 'org-mode 'markdown-mode 'text-mode 'fundamental-mode)
                        (sis--context-other-p back-detect fore-detect))
                   (and (derived-mode-p 'telega-chat-mode)
                        (or (and (= (point) telega-chatbuf--input-marker) ; beginning of input
                                 (eolp))
                            (sis--context-other-p back-detect fore-detect))
                        )))
      'other))

  (add-to-list 'sis-context-detectors #'+sis-context-switching)

  ;; Inline-mode
  (defvar-local +sis-inline-english-last-space-pos nil
    "The last space position in inline mode.")
  (add-hook! 'sis-inline-english-deactivated-hook
    (defun +sis-line-set-last-space-pos ()
      (when (eq (char-before) ?\s)
          (setq +sis-inline-english-last-space-pos (point)))))

  (add-hook! 'sis-inline-mode-hook
    (defun +sis-inline-add-post-self-insert-hook ()
      (add-hook! (post-self-insert-hook) :local
        (defun +sis-inline-remove-redundant-space ()
          (when (and (eq +sis-inline-english-last-space-pos (1- (point)))
                     (looking-back " [，。？！；：]"))
            (save-excursion
              (backward-char 2)
              (delete-char 1)
              (setq-local +sis-inline-english-last-space-pos nil)
            ))
          ))
      ))
  )
