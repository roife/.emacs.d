;;; -*- lexical-binding: t; -*-

(defun +liberime-prepend-env-path (name path)
  (when (file-directory-p path)
    (let ((value (getenv name)))
      (unless (member path (and value (split-string value path-separator t)))
        (setenv name
                (if (and value (not (string= value "")))
                    (concat path path-separator value)
                  path))))))

(when (eq system-type 'darwin)
  (+liberime-prepend-env-path "CPATH" "/opt/homebrew/include")
  (+liberime-prepend-env-path "LIBRARY_PATH" "/opt/homebrew/lib"))

(use-package liberime
  :straight (liberime :type git :host github :repo "emacs-rime/liberime")
  :require-incrementally (t)
  :init
  (setq liberime-shared-data-dir (if (eq system-type 'darwin)
                                     "~/Library/Rime/"
                                   "~/.local/share/fcitx5/rime")
        liberime-user-data-dir (no-littering-expand-var-file-name "rime/")))

(use-package liberime-regexp
  :straight (:host github :repo "roife/liberime-regexp")
  :require-incrementally (liberime t)
  :hook ((liberime-after-start . liberime-regexp-enable)
         (liberime-after-start . liberime-regexp-segment-mode))
  :config
  (setq liberime-regexp-candidate-limit 40))

(use-package rimel
  :straight (rimel :type git :host github :repo "emacs-rime/rimel")
  :require-incrementally (liberime liberime-regexp t)
  :custom-face
  (rimel-candidate-label-face ((t (:inherit font-lock-comment-face :height 0.85))))
  (rimel-page-indicator-face ((t (:inherit font-lock-comment-face :height 0.85))))
  (rimel-highlight-face ((t (:inherit hl-line))))
  :init
  (setq default-input-method "rimel"
        rimel-show-candidate 'posframe
        rimel-inline-preedit t
        rimel-candidate-show-preedit nil
        rimel-posframe-style 'horizontal
        rimel-posframe-properties nil
        rimel-candidate-label-format "%d "
        rimel-page-indicator-format "%d%s"
        rimel-disable-predicates '(rimel-predicate-prog-in-code-p
                                   rimel-predicate-after-alphabet-char-p
                                   rimel-predicate-current-uppercase-letter-p
                                   rimel-predicate-org-in-src-block-p
                                   rimel-predicate-org-latex-mode-p
                                   rimel-predicate-tex-math-or-command-p)))

(register-input-method "rimel" "Chinese" #'rimel-activate "中" "Rimel")

;; [sis] automatically switch input source
(use-package sis
  :straight t
  :hook (;; When add space after chinese char, automatically switch to english mode
         (liberime-after-start . sis-global-inline-mode)
         ;; Enable the context-mode for all buffers
         (liberime-after-start . sis-global-context-mode)
         ;; Colored cursor
         (liberime-after-start . sis-global-cursor-color-mode))
  :init
  ;; Use rimel as default
  (sis-ism-lazyman-config nil "rimel" 'native)
  (sis-get) ; HACK: set sis--ism
  :config
  (add-hook! buffer-list-update-hook
    (defun +sis-refresh-cursor-color ()
      (sis--get)
      (sis--update-cursor-color)))

  ;; HACK: Set cursor color automatically
  (add-hook! (enable-theme-functions server-after-make-frame-hook) :unless-daemonp-call-immediately
    (defun +sis-set-cursor-color (&rest _)
      (setq sis-other-cursor-color (face-foreground 'error nil t)
            sis-default-cursor-color (face-background 'cursor nil t))
      (+sis-refresh-cursor-color)))

  ;; Recover the terminal cursor color when leaving Emacs (TUI only).
  (add-hook! kill-emacs-hook
    (defun +sis-reset-terminal-cursor-color ()
      (unless (display-graphic-p)
        (send-string-to-terminal "\e]112\a"))))

  ;; Context mode
  (add-hook! meow-insert-exit-hook #'sis-set-english)
  (add-to-list 'sis-context-hooks 'meow-insert-enter-hook)

  ;; Ignore some mode with context mode
  (defadvice! +sis-context-guess-ignore-modes (fn &rest args)
    :around #'sis--context-guess
    (if (derived-mode-p 'pdf-view-mode)
        'english
      (apply fn args)))

  (defun +sis-context-switching-other (back-detect fore-detect)
    (and meow-insert-mode
         (or (and (derived-mode-p 'telega-chat-mode)
                  (or (and (= (point) telega-chatbuf--input-marker) ; beginning of input
                           (eolp)))))
         'other))
  (add-to-list 'sis-context-detectors #'+sis-context-switching-other)

  ;; Inline-mode
  (defconst +sis-chinese-puncs "，。？！；：（【「“")
  (defconst +sis-chinese-punc-chars (string-to-list +sis-chinese-puncs))

  (defvar-local +sis-inline-pending-space-pos nil
    "Position after the space to remove before the next Chinese punctuation.")

  (defun +sis-inline-clear-space ()
    (setq-local +sis-inline-pending-space-pos nil)
    (add-hook! pre-command-hook :local :remove #'+sis-inline-check-next-command)
    (add-hook! post-self-insert-hook :local :remove #'+sis-inline-remove-redundant-space))

  (defadvice! +sis-inline-record-space (fn &rest args)
    :around #'sis--inline-deactivate
    (let ((english-tail-p (and (eq sis--inline-lang 'english)
                               (overlayp sis--inline-overlay)
                               (= (point) (overlay-end sis--inline-overlay)))))
      (prog1 (apply fn args)
        (+sis-inline-clear-space)
        (when (and sis-inline-mode english-tail-p
                   (eq this-command 'self-insert-command)
                   (eq (char-before) ?\s))
          (setq-local +sis-inline-pending-space-pos (point))
          (add-hook! pre-command-hook :local #'+sis-inline-check-next-command)
          (add-hook! post-self-insert-hook :local #'+sis-inline-remove-redundant-space)))))

  (defun +sis-inline-check-next-command ()
    (unless (and sis-inline-mode
                 (eq this-command 'self-insert-command)
                 (eql (point) +sis-inline-pending-space-pos))
      (+sis-inline-clear-space)))

  (add-hook! sis-inline-mode-hook
    (defun +sis-inline-remove-redundant-space ()
      (when-let ((pos +sis-inline-pending-space-pos))
        ;; A composing input method may not have inserted anything yet.
        (unless (= (point) pos)
          (+sis-inline-clear-space)
          (when (and (> (point) pos)
                     (eq (char-before pos) ?\s)
                     (memq (char-after pos) +sis-chinese-punc-chars))
            (delete-region (1- pos) pos))))))

  ;; Chinese punc adjustment for inline mode
  (defun +sis-remove-head-space-after-cc-punc (_)
    (when (or (memq (char-before) +sis-chinese-punc-chars)
              (bolp))
      (delete-char 1)))
  (setq sis-inline-tighten-head-rule #'+sis-remove-head-space-after-cc-punc)

  (defun +sis-remove-tail-space-before-cc-punc (_)
    (when (eq (char-before) ? )
      (backward-delete-char 1)
      (when (and (eq (char-before) ? )
                 (memq (char-after) +sis-chinese-punc-chars))
        (backward-delete-char 1))))
  (setq sis-inline-tighten-tail-rule #'+sis-remove-tail-space-before-cc-punc))
