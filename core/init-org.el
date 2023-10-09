;;; -*- lexical-binding: t -*-

;; [org-fragtog] Preview and edit latex in md/org elegantly
(use-package org-fragtog
  :straight t
  :hook ((org-mode . org-fragtog-mode)))

;; [org]
(use-package org
  :custom-face (org-quote ((t (:inherit org-block-begin-line))))
  :hook ((org-mode . (lambda () (setq-local dabbrev-abbrev-skip-leading-regexp "[=*]"))))  ;; Skipping leading char, so corfu can complete with dabbrev for formatted text
  :config
  ;; HACK: inline highlight for CJK
  (setq org-emphasis-regexp-components '("-[:space:]('\"{[:nonascii:][:alpha:]"
                                         "-[:space:].,:!?;'\")}\\[[:nonascii:][:alpha:]"
                                         "[:space:]"
                                         "."
                                         1))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)
  (org-element--set-regexps)
  (defun org-do-emphasis-faces (limit)
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

  (setq
   ;; Use {} for sub- or super- scripts
   org-use-sub-superscripts "{}"
   ;; Highlight quote and verse blocks
   org-fontify-quote-and-verse-blocks t)

  (defun +org-custom-link-img-follow (path)
    (org-open-file
     (format "%s/static/%s" (project-root (project-current)) path)))
  (defun +org-custom-link-img-export (path desc format)
    (cond
     ((eq format 'html)
      (format "<img src=\"/images/%s\" alt=\"%s\"/>" path desc))))
  (org-link-set-parameters "img" :follow '+org-custom-link-img-follow :export '+org-custom-link-img-export)

  ;; Org Latex Preview
  (setq org-latex-create-formula-image-program 'dvisvgm
        org-startup-with-latex-preview nil)
  (plist-put org-format-latex-options :scale 1.5)

  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   org-support-shift-select t

   org-pretty-entities t
   org-pretty-entities-include-sub-superscripts nil
   org-ellipsis "…"

   org-imenu-depth 4

   org-startup-indented t)
  )

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
  (setq org-hugo-default-section-directory "posts"
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
      (set-face-attribute 'org-block-begin-line nil :background (face-attribute 'default :background))
      (set-face-attribute 'org-block-end-line nil :background (face-attribute 'default :background))
      (set-face-attribute 'org-block-begin-line nil :foreground (face-attribute 'default :background))
      (set-face-attribute 'org-block-end-line nil :foreground (face-attribute 'default :background))
      (set-face-attribute 'org-quote nil :foreground (face-attribute 'default :foreground))

      ;; render biiiiiig latex fomulars
      (setq-local org-format-latex-options (copy-sequence org-format-latex-options))
      (plist-put org-format-latex-options :scale 3.0)
      (org-latex-preview '(16))

      ;; the following settings must be set after restarting org-mode
      (setq-local header-line-format nil
                  mode-line-format nil
                  line-spacing 10)
      (+hide-tab-bar)
      (text-scale-increase 3)
      (visual-line-mode)
      (show-paren-local-mode -1)
      (hl-line-mode -1)
      (org-tree-slide-mode)
      (setq-local org-tree-slide-date " "
                  org-tree-slide-title " "
                  org-tree-slide-author nil
                  org-tree-slide-email nil)
      ))

  (defun +org-slide-stop ()
    (interactive)

    (when (eq major-mode 'org-mode)
      ;; reset
      (setq org-hide-emphasis-markers nil)
      (set-face-attribute 'org-meta-line nil :foreground nil)
      (set-face-attribute 'org-block-begin-line nil :background nil)
      (set-face-attribute 'org-block-begin-line nil :foreground nil)
      (set-face-attribute 'org-block-end-line nil :background nil)
      (set-face-attribute 'org-block-end-line nil :foreground nil)
      (set-face-attribute 'org-quote nil :foreground nil)
      (+show-tab-bar)

      ;; remove local settings
      (kill-local-variable 'org-format-latex-options)

      (org-tree-slide-mode -1)

      ;; `org-mode-restart' will clear all local variables,
      ;; so there is no need to reset them manually
      (org-mode-restart)

      ;; remove biiiiiig latex formulars
      (org-latex-preview '(64))
      )
    )
  (setq org-tree-slide-heading-emphasis t
        org-tree-slide-content-margin-top 1
        org-tree-slide-slide-in-effect t)
  )
