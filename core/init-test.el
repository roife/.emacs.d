;;; -*- lexical-binding: t -*-
(with-eval-after-load "eglot"
  (add-to-list 'eglot-stay-out-of 'flymake)

  (cl-defun eglot--languageId (&optional (server (eglot--current-server-or-lose)))
    "Compute LSP \\='languageId\\=' string for current buffer.
Doubles as an predicate telling if SERVER can manage current
buffer."
    (or (cl-loop for (mode . languageid) in
                        (eglot--languages server)
                        when (provided-mode-derived-p major-mode mode)
                        return languageid)
        ""))
  )
