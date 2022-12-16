;;; -*- lexical-binding: t -*-

(use-package citre
  :straight t
  :bind (:map prog-mode-map
              ("C-c c j" . citre-jump+)
              ("C-c c k" . citre-jump-back+)
              ("C-c c p" . citre-peek)
              ("C-c c a" . citre-ace-peek)
              ("C-c c u" . citre-update-this-tags-file))
  :init
  (require 'citre-config)
  (setq citre-auto-enable-citre-mode-modes '(prog-mode)
        citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t)

  (with-eval-after-load 'projectile
    (setq ))

  (defun citre-jump+ ()
    "Jump to the definition of the symbol at point. Fallback to `xref-find-definitions'."
    (interactive)
    (condition-case _
        (citre-jump)
      (error (call-interactively #'xref-find-definitions))))

  (defun citre-jump-back+ ()
    "Go back to the position before last `citre-jump'. Fallback to `xref-go-back'."
    (interactive)
    (condition-case _
        (citre-jump-back)
      (error (if (fboundp #'xref-go-back)
                 (call-interactively #'xref-go-back)
               (call-interactively #'xref-pop-marker-stack)))))
  :config
  (define-advice xref--create-fetcher (:around (fn &rest args) fallback)
    (let ((fetcher (apply fn args))
          (citre-fetcher
           (let ((xref-backend-functions '(citre-xref-backend t)))
             (ignore xref-backend-functions)
             (apply fn args))))
      (lambda ()
        (or (with-demoted-errors "%s, fallback to citre"
              (funcall fetcher))
            (funcall citre-fetcher)))))

  (defalias #'my-eglot-citre-capf
    (cape-super-capf #'eglot-completion-at-point #'citre-completion-at-point))

  (add-hook 'eglot-managed-mode-hook
            (defun my-toggle-citre-eglot-capf ()
              (if (eglot-managed-p)
                  (add-to-list 'completion-at-point-functions #'my-eglot-citre-capf))))
  )


(provide 'init-test)
