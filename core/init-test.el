;;; -*- lexical-binding: t -*-

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
  (when-let* ((project-name (when (project-current)
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

(defun ezf-1 (candidates &optional field separator)
  "Extract FIELD from CANDIDATES and join them with SEPARATOR."
  (let ((field (and field
                    (not (string-empty-p field))
                    (split-string field "-"))))
    (mapconcat
     (lambda (candidate)
       (let ((words (split-string candidate " " t " ")))
         (pcase field
           ('nil candidate)
           (`(,column)
            (nth (string-to-number column) words))
           (`(,begin ,end)
            (let* ((begin (string-to-number begin))
                   (words (nthcdr begin words)))
              (string-join
               (if (string-empty-p end)
                   words
                 (take (1+ (- (string-to-number end) begin)) words))
               " "))))))
     candidates
     (or separator " "))))

(defun ezf--read (filename)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents filename))
    (buffer-string)))

(defun ezf--write (filename string)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region string nil filename nil 'silent)))

(defun ezf (filename &optional field initial-input input-separator output-separator)
  "Select candidates from FILENAME with `completing-read-multiple'.

If FIELD is non-nil, return only that zero-based column or inclusive
column range from each selected line.  FIELD may be \"1\", \"1-6\",
or \"1-\".  INITIAL-INPUT seeds the minibuffer so callers can filter
with text already present on the command line.  INPUT-SEPARATOR is
either \"line\" or \"nul\"; OUTPUT-SEPARATOR joins multiple selections."
  (when-let* ((candidates (completing-read-multiple
                           "Pick a Candidate: "
                           (split-string
                            (ezf--read filename)
                            (if (string= input-separator "nul") "\0" "\n")
                            t)
                           nil t initial-input)))
    (ezf-1 candidates field output-separator)))

(defun ezf--fd (directory initial-input &optional directories-only)
  (require 'consult)
  (let ((default-directory (file-name-as-directory directory))
        (consult-fd-args
         (append '("fd" "--full-path" "--color=never" "--hidden"
                   "--follow" "--exclude=.git")
                 (when directories-only '("--type=directory")))))
    (consult--find
     (if directories-only "Directory: " "File: ")
     +consult-fd-dwim
     initial-input)))

(defun ezf--history (filename initial-input)
  (require 'consult)
  (let ((history (split-string (ezf--read filename) "\0" t)))
    (ezf--write
     filename
     (mapconcat (lambda (command)
                  (string-replace "\n" " ↳ " command))
                history "\n"))
    (let* ((builder (consult--ripgrep-make-builder (list filename)))
           (candidate
            (consult--read
             (consult--process-collection
                 builder :transform (consult--grep-format builder))
             :prompt "History: "
             :lookup #'consult--lookup-member
             :initial initial-input
             :require-match t
             :category 'consult-grep
             :sort nil)))
      (when (string-match "^[^:]+:\\([0-9]+\\):"
                          (substring-no-properties candidate))
        (nth (1- (string-to-number (match-string 1 candidate))) history)))))

(defun ezf-client (request-file result-file)
  "Handle an ezf terminal REQUEST-FILE, writing to RESULT-FILE."
  (let ((frame (selected-frame))
        (vertico-buffer-display-action '(display-buffer-same-window)))
    (unwind-protect
        (when-let* ((selection
                     (condition-case nil
                         (minibuffer-with-setup-hook
                             (lambda ()
                               (run-with-idle-timer
                                0.1 nil
                                (lambda ()
                                  (when-let* ((window (active-minibuffer-window)))
                                    (with-selected-window window
                                      (vertico--exhibit)
                                      (redisplay t))))))
                           (pcase (butlast
                                   (split-string (ezf--read request-file) "\0"))
                             (`("generic" ,filename ,field ,query ,separator)
                              (ezf filename field query separator "\0"))
                             (`("file" ,directory ,query)
                              (ezf--fd directory query))
                             (`("directory" ,directory ,query)
                              (ezf--fd directory query t))
                             (`("history" ,filename ,query)
                              (ezf--history filename query))))
                       (quit nil))))
          (ezf--write result-file selection))
      (run-at-time 0 nil #'delete-frame frame t))))
