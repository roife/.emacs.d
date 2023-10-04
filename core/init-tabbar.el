;; -*- lexical-binding: t; -*-

;; show encodings for UTF-8:LF
(defvar +tab-bar-shows-single-empty-persp nil)

;; [tab-bar] Tab bar
(use-package tab-bar
  ;; Turn on tab-bar-mode in early-init to speed-up
  ;; :hook (window-setup . tab-bar-mode)
  :config
  (setq tab-bar-separator " "
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width nil
        tab-bar-close-button-show nil
        tab-bar-tab-hints t)

  (customize-set-variable 'tab-bar-select-tab-modifiers '(super))

  ;; truncate for [tab name] and add count
  (setq tab-bar-tab-name-function
        (lambda () (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
                     (count (length (window-list-1 nil 'nomini)))
                     (truncated-tab-name (if (< (length raw-tab-name)
                                                tab-bar-tab-name-truncated-max)
                                             raw-tab-name
                                           (truncate-string-to-width raw-tab-name
                                                                     tab-bar-tab-name-truncated-max
                                                                     nil nil tab-bar-tab-name-ellipsis))))
                (if (> count 1)
                    (format "%s (%d)" truncated-tab-name count)
                  truncated-tab-name))))

  ;; Add spaces for tab-name
  (setq tab-bar-tab-name-format-function
        (lambda (tab i) (propertize
                    (format " %d %s " i (alist-get 'name tab))
                    'face (funcall tab-bar-tab-face-function tab))))

  ;; cache for persp indicator
  ;; add [persp-name] and [meow-indicator] on tab-bar
  (defvar +tab-bar-persp-indicator-cache nil)
  (defun +tab-bar-update-persp-indicator (&rest _)
    (setq +tab-bar-persp-indicator-cache
          (when-let* ((persp-list (and (bound-and-true-p persp-mode)
                                       (persp-names-current-frame-fast-ordered)))
                      (cur-persp (and (or +tab-bar-shows-single-empty-persp
                                          (> (length persp-list) 1))
                                      (safe-persp-name (get-current-persp))))
                      (subst-persp-list (cl-substitute (concat "[" cur-persp "]") cur-persp persp-list :count 1))
                      (persp-list-text (concat " " (string-join subst-persp-list " "))))
            (propertize persp-list-text 'face '(:inherit font-lock-variable-name-face)))))

  (defun +tab-bar-persp-indicator ()
    (or +tab-bar-persp-indicator-cache (+tab-bar-update-persp-indicator)))

  ;; TODO: KILL
  (dolist (hook '(persp-created-functions
                  persp-renamed-functions
                  persp-activated-functions
                  persp-after-load-state-functions
                  persp-before-kill-functions))
    (add-hook hook #'(lambda (&rest _)
                       (+tab-bar-update-persp-indicator)
                       (force-mode-line-update t))))


  (setq tab-bar-format '(meow-indicator +tab-bar-persp-indicator tab-bar-format-tabs tab-bar-separator))

  ;; WORKAROUND: fresh tab-bar for daemon
  (when (daemonp)
    (add-hook 'after-make-frame-functions
              #'(lambda (&rest _) (force-mode-line-update))))

  (force-mode-line-update)
  )
