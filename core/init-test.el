;;; -*- lexical-binding: t -*-
(defun jdtls-command-contact (&optional interactive)
  (let* ((jdtls-cache-dir (file-name-concat user-emacs-directory "cache" "lsp-cache"))
         (project-dir (file-name-nondirectory (directory-file-name (project-root (project-current)))))
         (data-dir (expand-file-name (file-name-concat jdtls-cache-dir (md5 project-dir))))
         (jvm-args `("-Xmx8G"
                      ;; "-XX:+UseG1GC"
                      "-XX:+UseZGC"
                      "-XX:+UseStringDeduplication"
                      ;; "-XX:FreqInlineSize=325"
                      ;; "-XX:MaxInlineLevel=9"
                      "-XX:+UseCompressedOops"))
         (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
         ;; tell jdtls the data directory and jvm args
         (contact (append '("jdtls") jvm-args `("-data" ,data-dir))))
    contact))

(with-eval-after-load "eglot"
  (add-to-list 'eglot-stay-out-of 'flymake)

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

  (push '(verilog-mode . ("vizsla")) eglot-server-programs)
  (push '(java-mode . jdtls-command-contact) eglot-server-programs)
  (setq eglot-workspace-configuration '(:rust-analyzer (:completion (:limit 10))))
  )

(setq eglot-x-enable-snippet-text-edit nil)

;; keycast
(use-package keycast
  :straight t
  :config
  (dolist (input
           '(self-insert-command
             org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event
           '(next-line previous-line forward-char backward-char left-char right-char))
    (add-to-list 'keycast-substitute-alist `(,event "." "Moving…")))

  (dolist (event
           '(mwheel-scroll
             mouse-drag-region
             mouse-set-point
             mouse-set-region
             pixel-scroll-precision))
    (add-to-list 'keycast-substitute-alist `(,event "." "Mouse…"))))
