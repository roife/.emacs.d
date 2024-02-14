;;; -*- lexical-binding: t -*-

;; [org-fragtog] Preview and edit latex in md/org elegantly
(use-package org-fragtog
  :straight t
  :hook ((org-mode . org-fragtog-mode)))

;; [org]
(use-package org
  :straight (:type built-in)
  :custom-face (org-quote ((t (:inherit org-block-begin-line))))
  :hook ((org-mode . (lambda () (setq-local dabbrev-abbrev-skip-leading-regexp "[=*]"))))  ;; Skipping leading char, so corfu can complete with dabbrev for formatted text
  :config
  (setq
   ;; subscription: Use {} for sub- or super- scripts
   org-use-sub-superscripts '{}
   org-export-with-sub-superscripts '{}

   ;; prettify
   org-startup-indented t
   org-pretty-entities t
   org-ellipsis "…"
   ;; Highlight quote and verse blocks
   org-fontify-quote-and-verse-blocks t
   ;; Highlight the whole line for headings
   org-fontify-whole-heading-line t

   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-insert-heading-respect-content t

   ;; better keybindings
   org-special-ctrl-a/e t
   org-special-ctrl-k t
   org-special-ctrl-o t
   org-support-shift-select t
   org-ctrl-k-protect-subtree 'error
   org-fold-catch-invisible-edits 'show-and-error

   org-imenu-depth 4)

  ;; Better Org Latex Preview
  (setq org-latex-create-formula-image-program 'dvisvgm
        org-startup-with-latex-preview nil
        org-highlight-latex-and-related '(latex))
  (plist-put org-format-latex-options :scale 1.3)

  ;; HACK: inline highlight for CJK
  (setq org-emphasis-regexp-components '("-[:space:]('\"{[:nonascii:][:alpha:]"
                                         "-[:space:].,:!?;'\")}\\[[:nonascii:][:alpha:]"
                                         "[:space:]"
                                         "."
                                         1))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)
  (org-element--set-regexps)

  (defun +org-do-emphasis-faces (limit)
    "Run through the buffer and emphasize strings."
    (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\).*?[~=*/_+]"
                            (car org-emphasis-regexp-components))))
      (catch :exit
        (while (re-search-forward quick-re limit t)
          (let* ((marker (match-string 2))
                 (verbatim? (member marker '("~" "="))))
            (when (save-excursion
                    (goto-char (match-beginning 0))
                    (and
                     ;; HACK: Do not match latex fragments.
                     (not (texmathp))
                     ;; Do not match table hlines.
                     (not (and (equal marker "+")
                               (org-match-line
                                "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
                     ;; Do not match headline stars.  Do not consider
                     ;; stars of a headline as closing marker for bold
                     ;; markup either.
                     (not (and (equal marker "*")
                               (save-excursion
                                 (forward-char)
                                 (skip-chars-backward "*")
                                 (looking-at-p org-outline-regexp-bol))))
                     ;; Match full emphasis markup regexp.
                     (looking-at (if verbatim? org-verbatim-re org-emph-re))
                     ;; Do not span over paragraph boundaries.
                     (not (string-match-p org-element-paragraph-separate
                                          (match-string 2)))
                     ;; Do not span over cells in table rows.
                     (not (and (save-match-data (org-match-line "[ \t]*|"))
                               (string-match-p "|" (match-string 4))))))
              (pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist))
                          (m (if org-hide-emphasis-markers 4 2)))
                (font-lock-prepend-text-property
                 (match-beginning m) (match-end m) 'face face)
                (when verbatim?
                  (org-remove-flyspell-overlays-in
                   (match-beginning 0) (match-end 0))
                  (remove-text-properties (match-beginning 2) (match-end 2)
                                          '(display t invisible t intangible t)))
                (add-text-properties (match-beginning 2) (match-end 2)
                                     '(font-lock-multiline t org-emphasis t))
                (when (and org-hide-emphasis-markers
                           (not (org-at-comment-p)))
                  (add-text-properties (match-end 4) (match-beginning 5)
                                       '(invisible t))
                  (add-text-properties (match-beginning 3) (match-end 3)
                                       '(invisible t)))
                (throw :exit t))))))))
  (advice-add #'org-do-emphasis-faces :override #'+org-do-emphasis-faces)
  (defun +org-element--parse-generic-emphasis (mark type)
    "Parse emphasis object at point, if any.

MARK is the delimiter string used.  TYPE is a symbol among
`bold', `code', `italic', `strike-through', `underline', and
`verbatim'.

Assume point is at first MARK."
    (save-excursion
      (let ((origin (point)))
        (unless (bolp) (forward-char -1))
        (let ((opening-re
               (rx-to-string
                `(seq (or line-start (any space ?- ?\( ?' ?\" ?\{ nonascii))
                      ,mark
                      (not space)))))
          (when (looking-at opening-re)
            (goto-char (1+ origin))
            (let ((closing-re
                   (rx-to-string
                    `(seq
                      (not space)
                      (group ,mark)
                      (or (any space ?- ?. ?, ?\; ?: ?! ?? ?' ?\" ?\) ?\} ?\\ ?\[
                               nonascii)
                          line-end)))))
              (when (re-search-forward closing-re nil t)
                (let ((closing (match-end 1)))
                  (goto-char closing)
                  (let* ((post-blank (skip-chars-forward " \t"))
                         (contents-begin (1+ origin))
                         (contents-end (1- closing)))
                    (list type
                          (append
                           (list :begin origin
                                 :end (point)
                                 :post-blank post-blank)
                           (if (memq type '(code verbatim))
                               (list :value
                                     (and (memq type '(code verbatim))
                                          (buffer-substring
                                           contents-begin contents-end)))
                             (list :contents-begin contents-begin
                                   :contents-end contents-end)))))))))))))
  (advice-add #'org-element--parse-generic-emphasis :override #'+org-element--parse-generic-emphasis)
  )


;; [org-entities]
(use-package org-entities
  :config
  (setq org-entities-user
        '(("vdash" "\\vdash" t "⊢" "⊢" "⊢" "⊢")
          ("vDash" "\\vDash" t "⊨" "⊨" "⊨" "⊨")
          ("Vdash" "\\Vdash" t "⊩" "⊩" "⊩" "⊩")
          ("Vvdash" "\\Vvdash" t "⊪" "⊪" "⊪" "⊪")
          ("nvdash" "\\nvdash" t "⊬" "⊬" "⊬" "⊬")
          ("nvDash" "\\nvDash" t "⊭" "⊭" "⊭" "⊭")
          ("nVdash" "\\nVdash" t "⊮" "⊮" "⊮" "⊮")
          ("nVDash" "\\nVDash" t "⊯" "⊯" "⊯" "⊯")
          ("subseteq" "\\subseteq" t "⊆" "⊆" "⊆" "⊆")
          ("supseteq" "\\supseteq" t "⊇" "⊇" "⊇" "⊇")
          ("subsetneq" "\\subsetneq" t "⊊" "⊊" "⊊" "⊊")
          ("supsetneq" "\\supsetneq" t "⊋" "⊋" "⊋" "⊋")
          ("nsubseteq" "\\nsubseteq" t "⊈" "⊈" "⊈" "⊈")
          ("nsupseteq" "\\nsupseteq" t "⊉" "⊉" "⊉" "⊉")
          ("nsubseteqq" "\\nsubseteqq" t "⊈" "⊈" "⊈" "⊈")
          ("nsupseteqq" "\\nsupseteqq" t "⊉" "⊉" "⊉" "⊉")
          ("subsetneqq" "\\subsetneqq" t "⊊" "⊊" "⊊" "⊊")
          ("supsetneqq" "\\supsetneqq" t "⊋" "⊋" "⊋" "⊋")
          ("nsubset" "\\nsubset" t "⊄" "⊄" "⊄" "⊄")
          ("nsupset" "\\nsupset" t "⊅" "⊅" "⊅" "⊅")
          ("nsubseteq" "\\nsubseteq" t "⊈" "⊈" "⊈" "⊈")
          ("nsupseteq" "\\nsupseteq" t "⊉" "⊉" "⊉" "⊉"))))


;; [org-appear] Make invisible parts of Org elements appear visible.
(use-package org-appear
  :straight t
  :hook ((org-mode . org-appear-mode))
  :config
  (setq
   org-hide-emphasis-markers t

   org-appear-autosubmarkers t
   org-appear-autoentities t
   org-appear-autokeywords t
   org-appear-inside-latex t

   org-appear-delay 0.1

   org-appear-trigger 'manual)

  (add-hook! org-mode-hook :call-immediately
    (defun +org-add-appear-hook ()
      (add-hook 'meow-insert-enter-hook #'org-appear-manual-start nil t)
      (add-hook 'meow-insert-exit-hook #'org-appear-manual-stop nil t))))


;; [org-visual-outline] Add guide lines for org outline
(use-package org-visual-outline
  :after org
  :no-require t
  :straight (:host github :repo "legalnonsense/org-visual-outline" :files ("*.el"))
  :init
  (use-package org-visual-indent
    :after org
    :hook ((org-mode . org-visual-indent-mode))
    :config
    ;; HACK: `org-visual-indent' calculates its faces from the current theme,
    ;; but it could be wrong after switching theme.
    (add-hook! +theme-changed-hook :call-immediately
      (defun +org-visual-outline-indent-color-update ()
        (let (bufs)
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (when org-visual-indent-mode
                (push buf bufs)
                (org-visual-indent-mode -1))))
          (setq org-visual-indent-color-indent
                (cl-loop for x from 1 to 8
                         with color = nil
                         do (setq color (or (face-foreground (intern (concat "org-level-" (number-to-string x))) nil t)
                                            (face-foreground 'org-level-1)))
                         collect `(,x ,(list :background color :foreground color :height .1))))
          (set-face-attribute 'org-visual-indent-pipe-face nil
                              :foreground (face-attribute 'default :foreground)
                              :background (face-attribute 'default :foreground))
          (set-face-attribute 'org-visual-indent-blank-pipe-face nil
                              :foreground (face-attribute 'default :background)
                              :background (face-attribute 'default :background))
          (dolist (buf bufs)
            (with-current-buffer buf
              (org-visual-indent-mode t))))
        ))
    ))


;; [org-pomodoro] Pomodoro timer for org-mode
(use-package org-pomodoro
  :straight t
  :after org
  :config
  (setq org-pomodoro-play-sounds nil))


;; [org-modern] A modern org-mode
(use-package org-modern
  :straight t
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda-mode)))


;; [ox]
(use-package ox
  :config
  (setq org-export-with-smart-quotes t
        org-html-validation-link nil
        org-latex-prefer-user-labels t
        org-export-with-latex t))


;; [ox-hugo]
(use-package ox-hugo
  :straight t
  :after ox
  :init (require 'ox-hugo)
  :config
  (setq org-hugo-section "posts"
        org-hugo-base-dir +blog-dir
        org-hugo-external-file-extensions-allowed-for-copying nil)

  (defun +ox-hugo/export-all (&optional org-files-root-dir dont-recurse)
    "Export all Org files (including nested) under ORG-FILES-ROOT-DIR.

All valid post subtrees in all Org files are exported using
`org-hugo-export-wim-to-md'.

If optional arg ORG-FILES-ROOT-DIR is nil, all Org files in
current buffer's directory are exported.

If optional arg DONT-RECURSE is nil, all Org files in
ORG-FILES-ROOT-DIR in all subdirectories are exported. Else, only
the Org files directly present in the current directory are
exported.  If this function is called interactively with
\\[universal-argument] prefix, DONT-RECURSE is set to non-nil.

Example usage in Emacs Lisp: (ox-hugo/export-all \"~/org\")."
    (interactive)
    (let* ((org-files-root-dir (or org-files-root-dir default-directory))
           (dont-recurse (or dont-recurse (and current-prefix-arg t)))
           (search-path (file-name-as-directory (expand-file-name org-files-root-dir)))
           (org-files (if dont-recurse
                          (directory-files search-path :full "\.org$")
                        (directory-files-recursively search-path "\.org$"))))
      (if (null org-files)
          (message "No Org files found in %s" search-path)
        (let ((cnt 1)
              (num-files (length org-files)))
          (dolist (org-file org-files)
            (with-current-buffer (find-file-noselect org-file)
              (message "[ox-hugo/export-all file %d/%d] Exporting %s" cnt num-files org-file)
              (org-hugo-export-wim-to-md :all-subtrees)
              (cl-incf cnt)))))))
  )


;; [org-tree-slide] Presentation with org-mode!
(use-package org-tree-slide
  :straight t
  :after org
  :commands (+org-slide-start +org-slides-stop)
  :config
  (defun +org-slide-start ()
    (interactive)

    (when (eq major-mode 'org-mode)
      ;; hide emephasis marks
      (setq org-hide-emphasis-markers t)
      ;; restart emacs to apply new settings above
      (org-mode-restart)

      ;; set faces for better presentation
      (set-face-attribute 'org-meta-line nil :foreground (face-attribute 'default :background))

      ;; the following settings must be set after restarting org-mode
      ;; render biiiiiig latex fomulars
      (setq-local org-format-latex-options (copy-sequence org-format-latex-options))
      (plist-put org-format-latex-options :scale 3.0)
      (org-latex-preview '(16))

      (setq-local header-line-format nil
                  mode-line-format nil
                  line-spacing 10
                  cursor-type 'hollow
                  meow-cursor-type-normal 'hollow)
      (meow-normal-define-key
       '("n" . org-tree-slide-move-next-tree)
       '("p" . org-tree-slide-move-previous-tree))
      (+hide-tab-bar)
      (text-scale-increase 3)
      (visual-line-mode)
      (show-paren-local-mode -1)
      (hl-line-mode -1)

      (org-tree-slide-mode)
      ;; the following settings must be set after enabling org-tree-slide-mode
      (setq-local org-tree-slide-date " "
                  org-tree-slide-title " "
                  org-tree-slide-author nil
                  org-tree-slide-email nil)

      (org-visual-indent-mode -1)
      (org-indent-mode)

      (add-hook! meow-insert-enter-hook
        (defun +org-tree-slide-make-readonly (&rest _)
          (read-only-mode -1)) nil t)
      (add-hook 'meow-insert-exit-hook #'read-only-mode nil t)
      ))

  (defun +org-slide-stop ()
    (interactive)

    (when (eq major-mode 'org-mode)
      ;; reset
      (setq org-hide-emphasis-markers nil)
      (set-face-attribute 'org-meta-line nil :foreground nil)

      (+show-tab-bar)

      ;; remove local settings
      (kill-local-variable 'org-format-latex-options)

      (org-tree-slide-mode -1)

      (read-only-mode -1)

      ;; `org-mode-restart' will clear all local variables,
      ;; so there is no need to reset them manually
      (org-mode-restart)

      ;; remove biiiiiig latex formulars
      (org-latex-preview '(64))
      )
    )
  (setq org-tree-slide-heading-emphasis t
        org-tree-slide-content-margin-top 1
        org-tree-slide-slide-in-effect nil)
  )
