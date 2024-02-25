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

  (push '(verilog-mode . ("vizsla")) eglot-server-programs)
  (push '(java-mode . jdtls-command-contact) eglot-server-programs)
  (setq eglot-workspace-configuration '(:rust-analyzer (:completion (:limit 10))))
  )

(setq eglot-x-enable-snippet-text-edit nil)
