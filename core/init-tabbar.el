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
  (add-hook! (persp-activated-functions persp-update-names-cache)
    (defun +tab-bar-update-persp-indicator (&rest _)
      (setq +tab-bar-persp-indicator-cache
            (when-let* ((persp-list (and (bound-and-true-p persp-mode)
                                         (persp-names-current-frame-fast-ordered)))
                        (len (length persp-list))
                        (check-len (or (> len 1) +tab-bar-shows-single-empty-persp)))
              (let* ((cur-persp-name (safe-persp-name (get-current-persp)))
                     (cur-pos (cl-position cur-persp-name persp-list))
                     (before (seq-subseq persp-list 0 cur-pos))
                     (before-joined (concat (string-join before " ") " "))
                     (after (seq-subseq persp-list (1+ cur-pos)))
                     (after-joined (concat " " (string-join after " ")))
                     (face '(:inherit font-lock-type-face :inverse-video t)))
                (concat (propertize (concat " " (when before before-joined) "·") 'face face)
                        (propertize (concat cur-persp-name) 'face (append face '(:weight ultra-bold :underline t)))
                        (propertize (concat (when after after-joined) " ") 'face face))))
            )
      ))

  (defun +tab-bar-persp-indicator ()
    (or +tab-bar-persp-indicator-cache
        (+tab-bar-update-persp-indicator)))

  ;; [telega]
  (defvar +tab-bar-telega-indicator-cache nil)
  (add-hook! (telega-connection-state-hook telega-kill-hook)
    (defun +tab-bar-telega-icon-update (&rest rest)
      (setq +tab-bar-telega-indicator-cache
            (when (and (fboundp 'telega-server-live-p)
                       (telega-server-live-p)
                       (buffer-live-p telega-server--buffer))
              (let* ((me-user (telega-user-me 'locally))
                     (online-p (and me-user (telega-user-online-p me-user)))
                     (unread-count (and (boundp 'telega--unread-chat-count)
                                        (plist-get telega--unread-chat-count :unread_unmuted_count))))
                (propertize (concat " "
                                    (if online-p "▶" "▷")
                                    (when (and unread-count (not (zerop unread-count)))
                                      (concat " " (number-to-string unread-count)))
                                    " ")
                            'face `(:inherit ,(if online-p 'success 'warning) :inverse-video t)))))))
  (defun +tab-bar-telega-icon ()
    (or +tab-bar-telega-indicator-cache
        (+tab-bar-telega-icon-update)))
  (advice-add 'telega--on-updateUnreadChatCount :after #'+tab-bar-telega-icon-update)

  (defun +hide-tab-bar ()
    (interactive)
    (setq tab-bar-format nil))

  (defun-call! +show-tab-bar ()
    (interactive)
    (setq tab-bar-format '(+tab-bar-telega-icon
                           meow-indicator
                           +tab-bar-persp-indicator
                           tab-bar-format-tabs tab-bar-separator))
    (tab-bar--update-tab-bar-lines))

  ;; WORKAROUND: fresh tab-bar for daemon
  (when (daemonp)
    (add-hook 'after-make-frame-functions
              #'(lambda (&rest _) (force-mode-line-update))))

  ;; (force-mode-line-update)
  )
