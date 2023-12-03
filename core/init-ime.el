;;; -*- lexical-binding: t; -*-

(use-package macim
  :straight (:host github :repo "roife/macim.el"
                   :files ("*.el" "module/*" "module"))
  :hook (after-init . macim-mode)
  :config
  (setq +macim-chinese-punc-chars (mapcar #'string-to-char macim--chinese-punc-list))

  (defun +macim-remove-head-space-after-cc-punc (_)
    (when (or (memq (char-before) +macim-chinese-punc-chars)
              (bolp))
      (delete-char 1)))
  (setq macim-inline-head-handler #'+macim-remove-head-space-after-cc-punc)

  (defun +macim-remove-tail-space-before-cc-punc (tighten-back-to)
    (when (> (point) tighten-back-to)
      (backward-delete-char (1- (- (point) tighten-back-to))))
    (when (and (eq (char-before) ? )
               (memq (char-after) +macim-chinese-punc-chars))
      (backward-delete-char 1)))
  (setq macim-inline-tail-handler #'+macim-remove-tail-space-before-cc-punc)

  ;; context-switch
  (add-hook 'meow-insert-exit-hook #'macim-select-ascii)
  (add-hook 'meow-insert-enter-hook #'macim-context-switch)
  (add-hook 'buffer-list-update-hook #'macim-context-switch)

  ;; context-mode
  (defun +macim-context-meow ()
    (if meow-insert-mode nil 'ascii))

  (defun +macim-context-ignore-modes ()
    (when (derived-mode-p 'pdf-view-mode)
      'ascii))

  (add-to-list 'macim-context-early-predicates #'+macim-context-meow)
  (add-to-list 'macim-context-early-predicates #'+macim-context-ignore-modes)

  (defun +macim-context-switching-other (back-detect fore-detect)
    (when (or (and (derived-mode-p 'org-mode 'markdown-mode 'text-mode)
                   (macim--context-other-p back-detect fore-detect))
              (and (derived-mode-p 'telega-chat-mode)
                   (or (and (= (point) telega-chatbuf--input-marker) ; beginning of input
                            (eolp))
                       (macim--context-other-p back-detect fore-detect))))
      'other))

  (add-to-list 'macim-context-predicates #'+macim-context-switching-other)

  ;; inline-mode
  (defvar-local +macim-inline-english-last-space-pos nil
    "The last space position in inline mode.")

  (add-hook! macim-inline-deactivated-hook
    (defun +macim-line-set-last-space-pos ()
      (when (eq (char-before) ?\s)
        (setq +macim-inline-english-last-space-pos (point)))))

  (add-hook! macim-inline-deactivated-hook
    (defun +macim-inline-add-post-self-insert-hook ()
      (add-hook! post-self-insert-hook :local
        (defun +macim-inline-remove-redundant-space ()
          (when (eq +macim-inline-english-last-space-pos (1- (point)))
            (when (and (memq (char-before) +macim-chinese-punc-chars)
                       (eq (char-before (1- (point))) ?\s))
              (save-excursion
                (backward-char 2)
                (delete-char 1)
                (setq-local +macim-inline-english-last-space-pos nil)))
            (remove-hook 'post-self-insert-hook #'+macim-inline-remove-redundant-space t))
          ))
      ))
  )
