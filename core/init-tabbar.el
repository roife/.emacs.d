;; -*- lexical-binding: t; -*-

;; Show persp list when only one persp exists
;; (defvar +tab-bar-shows-single-empty-persp nil)

;; [tab-bar] Tab bar
(use-package tab-bar
  ;; Turn on tab-bar-mode in early-init to speed-up
  ;; :hook (window-setup . tab-bar-mode)
  :config
  (setq tab-bar-separator ""
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width nil
        tab-bar-close-button-show nil
        tab-bar-tab-hints t)

  (customize-set-variable 'tab-bar-select-tab-modifiers '(super))

  ;; truncate for [tab name] and add count
  (setq tab-bar-tab-name-format-functions
        '(tab-bar-tab-name-format-hints
          tab-bar-tab-name-format-truncated
          (lambda (name &rest _) (concat " " name " "))
          tab-bar-tab-name-format-face))

  (defun +hide-tab-bar ()
    (interactive)
    (setq tab-bar-format nil))

  (defun +tab-bar-format-space ()
      (propertize " " 'face 'tab-bar))

  (defun-call! +show-tab-bar ()
    (interactive)
    (setq tab-bar-format '(meow-indicator
                           tab-bar-format-tabs))
    (tab-bar--update-tab-bar-lines))

  ;; WORKAROUND: fresh tab-bar for daemon
  (when (daemonp)
    (add-hook 'after-make-frame-functions
              #'(lambda (&rest _) (force-mode-line-update))))
  )
