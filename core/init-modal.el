;;; -*- lexical-binding: t; -*-

(require 'subr-x)

(defun +thing--arg-parent-bounds-near-point ()
  "Return surrounding list-like bounds around point or nearby whitespace."
  (or (+thing--arg-parent-bounds)
      (save-excursion
        (skip-chars-forward " \t\n\r")
        (+thing--arg-parent-bounds))
      (save-excursion
        (skip-chars-backward " \t\n\r")
        (+thing--arg-parent-bounds))))

(defun +thing--arg-parent-bounds ()
  "Return bounds of the nearest surrounding list-like form."
  (save-excursion
    (let ((pt (point)))
      (when-let* ((open (nth 1 (syntax-ppss
                                (cond
                                 ((memq (char-after pt) '(?\) ?\] ?\}))
                                  (max (point-min) (1- pt)))
                                 ((memq (char-after pt) '(?\( ?\[ ?\{))
                                  (min (point-max) (1+ pt)))
                                 (t pt))))))
        (cons open (ignore-errors (scan-sexps open 1)))))))

(defun +bounds-of-thing-at-point-arg (&optional bounds)
  "Return bounds of the comma-separated argument at point.

When BOUNDS is non-nil, include the adjacent comma separator when
possible.  For non-final arguments the trailing separator is
included; for the final argument the leading separator is included."
  (when-let* ((pt (point))
              (parent (+thing--arg-parent-bounds-near-point))
              (open (car parent))
              (close (cdr parent)))
    (save-excursion
      (let (prev next)
        (goto-char (1+ open))
        (while (and (< (point) close) (not next))
          (when (and (eq (char-after) ?,)
                     (eq (nth 1 (syntax-ppss (point))) open))
            (if (< (point) pt)
                (setq prev (point))
              (setq next (point))))
          (condition-case nil
              (forward-sexp 1)
            (error (forward-char 1))))
        (let ((beg (if prev (1+ prev) (1+ open)))
              (end (if next next (1- close))))
          (goto-char beg)
          (skip-chars-forward " \t\n\r" end)
          (setq beg (point))
          (goto-char end)
          (skip-chars-backward " \t\n\r" beg)
          (let ((trimmed-end (point)))
            (when (< beg trimmed-end)
              (if (not bounds)
                  (cons beg trimmed-end)
                (cond
                 (next
                  (goto-char (1+ next))
                  (skip-chars-forward " \t\n\r" (1- close))
                  (cons beg (point)))
                 (prev
                  (cons prev trimmed-end))
                 (t
                  (cons beg trimmed-end)))))))))))

;; [meow] Modal editing
(use-package meow
  :straight t
  :hook (after-init . meow-global-mode)
  :demand t
  :custom-face
  (meow-normal-indicator ((t (:inherit (font-lock-function-name-face bold :inverse-video t)))))
  (meow-insert-indicator ((t (:inherit (font-lock-keyword-face bold :inverse-video t)))))
  (meow-keypad-indicator ((t (:inherit (font-lock-builtin-face bold :inverse-video t)))))
  (meow-beacon-indicator ((t (:inherit (font-lock-type-face bold :inverse-video t)))))
  (meow-motion-indicator ((t (:inherit (font-lock-doc-face bold :inverse-video t)))))
  :config
  (setq-default meow-replace-state-name-list '((normal . "N")
                                               (motion . "M")
                                               (keypad . "K")
                                               (insert . "I")
                                               (beacon . "B")))
  (add-to-list 'meow-char-thing-table '(?a . arg))
  (meow-thing-register 'arg
                       #'+bounds-of-thing-at-point-arg
                       (lambda () (+bounds-of-thing-at-point-arg t)))

  ;; [motion]
  (meow-motion-overwrite-define-key
   '("h" . meow-left)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   '("<escape>" . ignore))

  ;; [leader]
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  ;; [normal]
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))

  (dolist
      (state
       '((telega-root-mode . motion)
         (telega-chat-mode . normal)
         (View-mode . normal)
         ;; (compilation-mode . normal)
         (blink-search-mode . insert)
         (rcirc-mode . normal)
         (comint-mode . normal)         ; IELM
         (fundamental-mode . normal)
         (message-mode . normal)
         (emacs-lisp-mode . normal)
         (eshell-mode . insert)
         (shell-mode . insert)
         (term-mode . insert)
         (vterm-mode . insert)
         (help-mode . normal)
         (vundo-mode . motion)))
    (add-to-list 'meow-mode-state-list state))
  )
