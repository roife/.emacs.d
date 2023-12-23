;;; -*- lexical-binding: t; -*-

(when (eq system-type 'gnu/linux)
  (use-package rime
    :straight (rime :type git
                    :host github
                    :repo "DogLooksGood/emacs-rime"
                    :files ("*.el" "Makefile" "lib.c"))
    :custom-face
    (rime-default-face ((t (:inherit hl-line
                                     :background unspecified :foreground unspecified))))
    (rime-preedit-face ((t (:inherit hl-line
                                     :background unspecified
                                     :inverse-video unspecified :underline t))))
    :init
    (require 'rime)
    :bind ("C-SPC" . toggle-input-method)
    :config
    (cond ((eq system-type 'darwin)
           (setq rime-librime-root "~/.emacs.d/librime/dist/"
                 rime-share-data-dir "~/Library/Rime"))
          ((eq system-type 'gnu/linux)
           (setq rime-librime-root "/usr/lib/")))

    (setq rime-show-candidate 'posframe
          rime-show-preedit 'inline
          rime-posframe-properties '(:internal-border-width 7))

    (defadvice! +rime-do-finalize-after-loading-module (&rest _)
      :after #'rime--load-dynamic-module
      (add-hook! kill-emacs-hook #'rime-lib-finalize))
    )


  ;; [sis] automatically switch input source
  (use-package sis
    :straight t
    :hook (;; Enable the inline-english-mode for all buffers.
           ;; When add space after chinese char, automatically switch to english mode
           (after-init . sis-global-inline-mode)
           ;; Enable the context-mode for all buffers
           (after-init . sis-global-context-mode)
           ;; Colored cursor
           (after-init . sis-global-cursor-color-mode))
    :config
    ;; Use emacs-rime as default
    (sis-ism-lazyman-config nil "rime" 'native)

    ;; HACK: Set cursor color automatically
    (add-hook! (+theme-changed-hook server-after-make-frame-hook) :call-immediately
      (defun +sis-set-other-cursor-color ()
        (setq sis-other-cursor-color (face-foreground 'error nil t))))

    (defun +sis-remove-head-space-after-cc-punc (_)
      (when (or (memq (char-before) '(?， ?。 ?？ ?！ ?； ?： ?（ ?【 ?「 ?“))
                                                                  (bolp))
                                          (delete-char 1)))
                (setq sis-inline-tighten-head-rule #'+sis-remove-head-space-after-cc-punc)

                (defun +sis-remove-tail-space-before-cc-punc (_)
                  (when (eq (char-before) ? )
                    (backward-delete-char 1)
                    (when (and (eq (char-before) ? )
                               (memq (char-after) '(?， ?。 ?？ ?！ ?； ?： ?（ ?【 ?「 ?“)))
                                                        (backward-delete-char 1))))
                          (setq sis-inline-tighten-tail-rule #'+sis-remove-tail-space-before-cc-punc)

                          ;; Context mode
                          (add-hook 'meow-insert-exit-hook #'sis-set-english)
                          (add-to-list 'sis-context-hooks 'meow-insert-enter-hook)

                          ;; Ignore some mode with context mode
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
                                                    (sis--context-other-p back-detect fore-detect)))))
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
                              (add-hook! post-self-insert-hook :local
                                (defun +sis-inline-remove-redundant-space ()
                                  (when (and (eq +sis-inline-english-last-space-pos (1- (point)))
                                             (looking-back " [，。？！；：（【「“]"))
                                    (save-excursion
                                      (backward-char 2)
                                      (delete-char 1)
                                      (setq-local +sis-inline-english-last-space-pos nil)
                                      ))
                                  ))
                              ))
                          ))


(use-package macim
 :straight (:host github :repo "roife/macim.el"
                  :files ("*.el" "module/*" "module"))
 :when (eq system-type 'darwin)
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
 ;; (add-hook 'buffer-list-update-hook #'macim-context-switch)

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
