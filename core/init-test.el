;;; -*- lexical-binding: t -*-
(defun jdtls-command-contact (&optional interactive)
  (let* ((jdtls-cache-dir (file-name-concat user-emacs-directory "cache" "lsp-cache"))
         (project-dir (file-name-nondirectory (directory-file-name (project-root (project-current)))))
         (data-dir (expand-file-name (file-name-concat jdtls-cache-dir (md5 project-dir))))
         (jvm-args `("-Xmx8G"
                     "-XX:+UseZGC"
                     "-XX:+UseStringDeduplication"
                     "-XX:+UseCompressedOops"))
         (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
         ;; tell jdtls the data directory and jvm args
         (contact (append '("jdtls") jvm-args `("-data" ,data-dir))))
    contact))

(with-eval-after-load "eglot"
  ;; I don't like flymake
  ;; (add-to-list 'eglot-stay-out-of 'flymake)

  (push '(verilog-mode . ("vizsla")) eglot-server-programs)
  (push '(java-mode . jdtls-command-contact) eglot-server-programs)

  ;; (add-to-list 'eglot-server-programs
  ;;              `((typst-mode typst-ts-mode) . ("typst-lsp")))
  )

(setq eglot-x-enable-snippet-text-edit nil)

(defun cargo-xtask-install-server ()
  (interactive)
  (let ((default-directory (locate-dominating-file default-directory "crates")))
    (if (and default-directory
             (string= (file-name-nondirectory (directory-file-name default-directory)) "rust-analyzer"))
        (progn
          (message "Running cargo xtask install --server")
          (compile "cargo xtask install --server --mimalloc")
          (message "Running cargo xtask install --server done"))
      (message "Not in rust-analyzer project")))
  )
;; set C-c C-x C-i in rust-mode to run cargo xtask install --server
(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "C-c C-x C-i") 'cargo-xtask-install-server))

(defun restart-eglot-and-switch-logs ()
  "Restart Eglot in the current buffer and switch to the Eglot log buffer."
  (interactive)
  (when-let ((project-name (when (project-current)
                             (file-name-nondirectory
                              (directory-file-name
                               (project-root (project-current))))))
             (log-buffer-name (format "*EGLOT (%s/(rust-ts-mode rust-mode)) events*"
                                      project-name)))
    (let ((eglot-log-window (catch 'found
                              (dolist (win (window-list))
                                (when (string-equal (buffer-name (window-buffer win))
                                                    log-buffer-name)
                                  (throw 'found win))))))
      (unless eglot-log-window
        (setq eglot-log-window (split-window-right)))
      (call-interactively #'eglot)
      (let ((current-window (selected-window)))
        (select-window eglot-log-window)
        (when (get-buffer log-buffer-name)
          (kill-buffer log-buffer-name))
        (switch-to-buffer log-buffer-name)
        (select-window current-window)))))

;; (use-package buffer-name-relative
;;   :straight (:host codeberg :repo "ideasman42/emacs-buffer-name-relative")
;;   :hook (after-init . buffer-name-relative-mode)
;;   :config
;;   (setq buffer-name-relative-prefix '("" . "/")))
