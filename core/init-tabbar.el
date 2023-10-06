;; -*- lexical-binding: t; -*-

;; Show persp list when only one persp exists
(defvar +tab-bar-shows-single-empty-persp nil)

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
                    (concat truncated-tab-name "(" (number-to-string count) ")")
                  truncated-tab-name))))

  ;; Add spaces for tab-name
  (setq tab-bar-tab-name-format-function
        (lambda (tab i)
          (let ((face (funcall tab-bar-tab-face-function tab)))
            (concat
             (propertize " " 'face face)
             (propertize (number-to-string i) 'face `(:inherit ,face :weight ultra-bold :underline t))
             (propertize (concat " " (alist-get 'name tab) " ") 'face face)))))

  ;; cache for persp indicator
  ;; add [persp-name] and [meow-indicator] on tab-bar
  (defvar +tab-bar-persp-indicator-cache nil)
  (defun +tab-bar-update-persp-indicator (&rest _)
    (setq +tab-bar-persp-indicator-cache
          (when-let* ((persp-list (and (bound-and-true-p persp-mode)
                                       (persp-names-current-frame-fast-ordered)))
                      (len (length persp-list))
                      (check-len (or (> len 1) +tab-bar-shows-single-empty-persp))
                      (cur-persp-name (and (safe-persp-name (get-current-persp))))
                      (triple (car (cl-loop for (prev cur next) on (cons nil persp-list)
                                            when (eq cur cur-persp-name)
                                            collect (list
                                                     (if (and (null prev) persp-switch-wrap (> len 2)) (car (last persp-list)) prev)
                                                     cur
                                                     (if (and (null next) persp-switch-wrap (> len 2)) (car persp-list) next))))))
            (cl-destructuring-bind (prev cur next) triple
              (concat
               " "
               (when prev (concat (propertize prev 'face 'mode-line-inactive) "◂"))
               cur
               (when next (concat "▸" (propertize next 'face 'mode-line-inactive)))
               " ")))
          ))

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
