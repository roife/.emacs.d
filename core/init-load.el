;;; init-load.el --- Resolve library paths -*- lexical-binding: t -*-

(defvar +load-path--cache nil
  "Cached (PATH FILES PASSTHROUGH) used by `+load-path-filter'.")

(defun +load-path-refresh (&optional path)
  "Rebuild the library index for PATH, defaulting to `load-path'.
Use this after changing package files outside straight."
  (interactive)
  (let ((files (make-hash-table :test #'equal))
        (path (or path load-path))
        passthrough)
    (dolist (directory path)
      (if (or (not (stringp directory))
              (not (file-name-absolute-p directory))
              (find-file-name-handler directory 'directory-files))
          (push directory passthrough)
        (condition-case nil
            (dolist (name (directory-files directory nil nil t))
              ;; Keep case-only matches eligible on macOS; the native loader
              ;; makes the final choice on case-sensitive filesystems too.
              (push directory (gethash (downcase name) files)))
          ;; Missing or unreadable directories may become usable later.
          (file-error (push directory passthrough)))))
    (setq +load-path--cache (list (copy-sequence path) files passthrough))
    (when (called-interactively-p 'interactive)
      (message "Indexed %d library file names" (hash-table-count files)))))

(defun +load-path-filter (path file suffixes)
  "Pre-resolve PATH to directories containing FILE with one of SUFFIXES.
Preserve path order and let Emacs select source, bytecode or native code.
Explicit paths bypass the index; unknown libraries use the full PATH."
  (if (or (null path) (file-name-directory file))
      path
    (unless (and +load-path--cache (equal path (car +load-path--cache)))
      (+load-path-refresh path))
    (let ((files (nth 1 +load-path--cache))
          candidates)
      (dolist (suffix (or suffixes '("")))
        (setq candidates
              (append (gethash (downcase (concat file suffix)) files) candidates)))
      (if (not candidates)
          ;; No negative cache: newly added library names remain loadable.
          path
        (let ((eligible (append candidates (nth 2 +load-path--cache))))
          (seq-filter (lambda (directory) (member directory eligible)) path))))))

(defun +load-path--during-build (original &rest args)
  "Bypass the index while ORIGINAL builds a package with ARGS.
Invalidate it even if the build fails or is interrupted."
  (let ((load-path-filter-function
         (unless (eq load-path-filter-function #'+load-path-filter)
           load-path-filter-function)))
    (unwind-protect
        (apply original args)
      (setq +load-path--cache nil))))

(add-hook! after-init-hook :depth 90
  (defun +load-path-initialize ()
    "Enable pre-resolved library search after package paths have been registered."
    (unless (memq load-path-filter-function '(nil +load-path-filter))
      (error "Conflicting load-path-filter-function: %S" load-path-filter-function))
    (+load-path-refresh)
    (setq load-path-filter-function #'+load-path-filter)))

(with-eval-after-load 'straight
  (advice-add 'straight--build-package :around #'+load-path--during-build))
