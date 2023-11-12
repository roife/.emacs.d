;;; -*- lexical-binding: t -*-

;; [ebib] Managing BibTeX and biblatex databases
;; (use-package ebib
;;   :straight t
;;   :config
;;   (setq ebib-bibtex-dialect 'biblatex
;;         ebib-keywords-field-keep-sorted t
;;         ebib-preload-bib-files `(,(file-name-concat +ebib-bib-dir "ebib.bib"))
;;         ebib-file-search-dirs `(,(file-name-concat +ebib-bib-dir "storage/pdf/"))
;;         ebib-notes-directory (file-name-concat +blog-dir "orgs/paper-notes")
;;         ebib-reading-list-file (file-name-concat +ebib-bib-dir "ebib_reading_list.org")
;;         ebib-keywords-file (file-name-concat +ebib-bib-dir "ebib_keywords.txt")
;;         ebib-keywords-file-save-on-exit 'always
;;         ebib-layout 'index-only
;;         ebib-use-timestamp t
;;         ebib-file-associations '(("pdf" . "open"))
;;         ebib-index-columns '(("Entry Key" 15 t)
;;                              ("Title" 70 t)
;;                              ("Author" 30 t)
;;                              ("Tag" 30 t))
;;         ebib-index-window-size 25)
;;   )

(use-package citar
  :straight t
  :config
  (setq citar-bibliography '("~/Documents/papers/ebib.bib")
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        citar-notes-paths (list (file-name-concat +blog-dir "orgs/paper-notes"))
        citar-at-point-function 'embark-act))


(use-package citar-embark
  :straight t
  :after citar embark
  :init
  (citar-embark-mode))


(use-package biblio
  :straight t)
