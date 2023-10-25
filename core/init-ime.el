;;; -*- lexical-binding: t; -*-

;; [sis] automatically switch input source
(defvar meow-leave-insert-mode-hook nil
  "Hook to run when leaving meow insert mode.")
(defvar meow-enter-insert-mode-hook nil
  "Hook to run when entering meow insert mode.")
(add-hook! meow-insert-mode-hook
  (defun +meow-insert-mode-run-hook-on-mode ()
    (run-hooks
     (if meow-insert-mode 'meow-enter-insert-mode-hook
       'meow-leave-insert-mode-hook))
    )
  )


(use-package rime
  :straight (rime :type git
                  :host github
                  :repo "DogLooksGood/emacs-rime"
                  :files ("*.el" "Makefile" "lib.c"))
  :hook ((after-init . toggle-input-method)
         (kill-emacs . rime-lib-finalize))
  :custom-face
  (rime-default-face ((t (:inherit hl-line :background nil))))
  (rime-preedit-face ((t (:inherit mode-line-active :background nil
                                   :inverse-video nil :underline t))))
  :bind ("C-'" . toggle-input-method)
  :init
  (setq default-input-method "rime")
  :config
  (when (eq system-type 'darwin)
    (setq rime-librime-root "~/.emacs.d/librime/dist/"))
  (when (eq system-type 'gnu/linux)
    (setq rime-librime-root "~/Library/Rime"))

  (setq rime-share-data-dir
        (pcase system-type
          ('darwin "~/Library/Rime")
          ('gnu/linux "/usr/share/rime-data")))

  (setq rime-show-candidate 'posframe
        rime-show-preedit 'inline
        rime-posframe-properties '(:border-width 7))
  )

;; `defmacro' cannot be placed in use-package
;; (defmacro +sis-add-post-cmd-hook! (modes func)
;;   "Add post-command-hook to MODES."
;;   (declare (indent defun))
;;   (let ((func-name (cadr func)))
;;     `(add-hook! ,modes
;;        (defun ,(intern (format "%s-add-post-cmd-hook" func-name)) ()
;;          (add-hook! post-command-hook :local
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
  (setq sis-inline-tighten-head-rule nil
        sis-prefix-override-keys (list "C-c" "C-x" "C-h" "C-,"))

  ;; HACK: Set cursor color automatically
  (add-hook! +theme-changed-hook :call-immediately
    (defun +sis-set-other-cursor-color ()
      (setq sis-other-cursor-color (face-foreground 'success nil t))))

  ;; Add IME according to system type
  (cond
   ((eq system-type 'darwin)
    (sis-ism-lazyman-config nil "rime" 'native))
   ((eq system-type 'gnu/linux)
    (sis-ism-lazyman-config "1" "2" 'fcitx5)))

  ;; Context mode
  (add-hook 'meow-leave-insert-mode-hook #'sis-set-english)
  (add-to-list 'sis-context-hooks 'meow-enter-insert-mode-hook)

  ;; WORKAROUND: conflicts with keypad
  (add-hook! meow-leave-insert-mode-hook #'sis-prefix-override-buffer-disable)
  (add-hook! meow-enter-insert-mode-hook #'sis-prefix-override-buffer-enable)
  (add-hook! (meow-normal-mode meow-motion-mode)
    (defun +sis-meow-set-english ()
      (when (or meow-normal-mode
                meow-motion-mode)
        (sis-set-english))))

  ;; WORKAROUND: conflicts with kbd macro
  (defadvice! +sis-disable-prefix-override (&rest _)
    :before #'start-kbd-macro
    (sis-prefix-override-buffer-disable))
  (defadvice! +sis-enable-prefix-override (&rest _)
    :before #'end-kbd-macro
    (when (bound-and-true-p meow-insert-mode)
      (sis-prefix-override-buffer-enable)))

  ;; HACK: Ignore some mode with context mode
  (defadvice! +sis-context-guess-ignore-modes (fn &rest args)
    :around #'sis--context-guess
    (if (derived-mode-p 'pdf-view-mode)
        'english
      (apply fn args)))

  (defun +sis-context-switching-other (back-detect fore-detect)
    (when (and meow-insert-mode
               (or (and (derived-mode-p 'org-mode 'markdown-mode 'text-mode)
                        (sis--context-other-p back-detect fore-detect))
                   (and (derived-mode-p 'telega-chat-mode)
                        (or (and (= (point) telega-chatbuf--input-marker) ; beginning of input
                                 (eolp))
                            (sis--context-other-p back-detect fore-detect))
                        )))
      'other))

  (add-to-list 'sis-context-detectors #'+sis-context-switching-other)

  ;; Inline-mode
  (defvar-local +sis-inline-english-last-space-pos nil
    "The last space position in inline mode.")
  (add-hook! sis-inline-english-deactivated-hook
    (defun +sis-line-set-last-space-pos ()
      (when (eq (char-before) ?\s)
          (setq +sis-inline-english-last-space-pos (point)))))

  (add-hook! sis-inline-mode-hook
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
