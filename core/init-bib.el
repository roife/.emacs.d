;;; -*- lexical-binding: t -*-

(use-package ebib
  :straight t
  :config
  (setq ebib-bibtex-dialect 'biblatex
        ebib-keywords-field-keep-sorted t
        ebib-preload-bib-files '("~/Documents/papers/ebib.bib")
        ebib-file-search-dirs '("~/Documents/papers/storage/pdf/")
        ebib-notes-directory "~/Documents/papers/storage/notes/"
        ebib-reading-list-file "~/Documents/papers/ebib_reading_list.org"
        ebib-keywords-file "~/Documents/papers/ebib_keywords.txt"
        ebib-keywords-file-save-on-exit 'always
        ebib-layout 'index-only
        ebib-use-timestamp t
        ebib-file-associations '(("pdf" . "open"))
        ebib-index-columns '(("Entry Key" 15 t)
                             ("Title" 70 t)
                             ("Author" 30 t)
                             ("Tag" 30 t))
        ebib-index-window-size 25)
  )

(use-package biblio
  :straight t)
