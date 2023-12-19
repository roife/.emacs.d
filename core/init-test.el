;;; -*- lexical-binding: t -*-
(with-eval-after-load "eglot"
  ;; (add-to-list 'eglot-stay-out-of 'flymake)

  (defun eglot-current-server ()
   "Return logical Eglot server for current buffer, nil if none."
   (setq eglot--cached-server
         (or eglot--cached-server
             (and (not (eq major-mode 'fundamental-mode)) ; gh#1330
                  (or
                   (cl-find-if #'eglot--languageId
                               (gethash (eglot--current-project)
                                        eglot--servers-by-project))
                   (and eglot-extend-to-xref
                        buffer-file-name
                        (gethash (expand-file-name buffer-file-name)
                                 eglot--servers-by-xrefed-file)))))))
  )
(setq eglot-x-enable-snippet-text-edit nil)
