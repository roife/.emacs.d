;;; -*- lexical-binding: t -*-

;; [org-fragtog] Preview and edit latex in md/org
(use-package org-fragtog
  :straight t
  :hook ((org-mode . org-fragtog-mode))
  )

;; [org]
(use-package org
  :custom-face
  (org-quote ((t (:inherit org-block-begin-line))))

  :config
  ;; 让中文也可以不加空格就使用行内格式
  (setq org-emphasis-regexp-components '("-[:space:]('\"{[:nonascii:]"
                                         "-[:space:].,:!?;'\")}\\[[:nonascii:]"
                                         "[:space:]"
                                         "."
                                         1))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)

  ;; 规定上下标必须加 {}，否则中文使用下划线时它会以为是两个连着的下标
  (setq org-use-sub-superscripts "{}")

  ;; 高亮 quote 和 verse block
  (setq org-fontify-quote-and-verse-blocks t)

  (defun org-custom-link-img-follow (path)
    (org-open-file
     (format "%s/%s" (project-root (project-current)) path)))

  (defun org-custom-link-img-export (path desc format)
    (cond
     ((eq format 'html)
      (format "<img src=\"/images/%s\" alt=\"%s\"/>" path desc))))

  (org-link-set-parameters "img" :follow 'org-custom-link-img-follow :export 'org-custom-link-img-export)

  ;; Org Latex Preview
  (setq org-latex-create-formula-image-program 'dvisvgm
        org-startup-with-latex-preview nil)
  (plist-put org-format-latex-options :scale 1.5)

  (setq org-support-shift-select t)
  )


;; ;; [org-indent]
(use-package org-indent
  :after org
  :hook (org-mode . org-indent-mode))


;; [org-modern]
(use-package org-modern
  :after org
  :straight t
  :hook (org-mode . org-modern-mode)
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   ;; org-hide-emphasis-markers t
   ;; org-pretty-entities t
   org-ellipsis "…")
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
