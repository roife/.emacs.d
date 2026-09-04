;;; init-ibuffer.el --- Project-aware buffer menu -*- lexical-binding: t; -*-

(use-package ibuffer
  :straight (:type built-in)
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("RET" . +ibuffer-visit-buffer))
  :hook (ibuffer-mode . +ibuffer-mode-setup-h)
  :init
  (setq ibuffer-human-readable-size t
        ibuffer-show-empty-filter-groups nil
        ibuffer-use-other-window t)
  :config
  (defun +ibuffer-mode-setup-h ()
    "Set up automatic refresh and compact lines in Ibuffer."
    (ibuffer-auto-mode 1)
    (setq-local truncate-lines t))

  (defun +ibuffer-visit-buffer ()
    "Visit the buffer at point, closing a popup Ibuffer window first."
    (interactive)
    (if (window-parameter nil 'window-side)
        (let ((buffer (ibuffer-current-buffer t))
              (window (selected-window)))
          (delete-window window)
          (pop-to-buffer-same-window buffer))
      (ibuffer-visit-buffer))))

;; [ibuffer-project] Group buffers by project root or directory.
(use-package ibuffer-project
  :straight t
  :hook (ibuffer . +ibuffer-project-setup-h)
  :preface
  (defconst +ibuffer-special-filter-groups
    '(("Agenda"
       (derived-mode . org-agenda-mode))
      ("Telega"
       (or (mode . telega-chat-mode)
           (mode . telega-root-mode)
           (mode . telega-image-mode)
           (mode . telega-webpage-mode)
           (name . "^\\*Telegram")))
      ("Gnus"
       (or (derived-mode . gnus-mode)
           (predicate . (and (boundp 'gnus-buffers)
                             (memq (current-buffer) gnus-buffers)))))
      ("Elfeed"
       (or (mode . elfeed-search-mode)
           (mode . elfeed-show-mode)))
      ("Eww"
       (or (mode . eww-mode)
           (mode . eww-bookmark-mode)
           (mode . eww-history-mode)
           (mode . eww-buffers-mode)
           (mode . eww-search-annotations-mode)))
      ("Xwidget"
       (mode . xwidget-webkit-mode))
      ("EMMS"
       (predicate . (string-prefix-p "emms-" (symbol-name major-mode))))
      ("Help"
       (or (derived-mode . help-mode)
           (mode . apropos-mode)
           (mode . Info-mode)))
      ("Scratch"
       (name . "\\`\\*scratch\\*")))
    "Filter groups shown before project and temporary buffers.")

  (defun +ibuffer-project-filter-groups ()
    "Return special, project, and temporary Ibuffer filter groups."
    (let ((temporary-filter '(starred-name . "")))
      (append
       +ibuffer-special-filter-groups
       (mapcar
        (lambda (group)
          (list (car group)
                `(and ,(cadr group) (not ,temporary-filter))))
        (ibuffer-project-generate-filter-groups))
       `(("Temporary buffers" ,temporary-filter)))))

  (defun +ibuffer-project-setup-h ()
    "Refresh project groups and sort buffers by project-relative name."
    (setq ibuffer-filter-groups (+ibuffer-project-filter-groups)
          ibuffer-sorting-mode 'project-file-relative
          ibuffer-sorting-reversep nil
          ibuffer-last-sorting-mode 'project-file-relative)
    ;; `ibuffer-hook' runs after Ibuffer's initial update, so redisplay once
    ;; after rebuilding the dynamic project groups.
    (ibuffer-update nil t))

  :config
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 25 25 :left :elide) " "
                (size 9 -1 :right) " "
                (mode 16 16 :left :elide) " "
                project-file-relative)
          (mark " "
                (name 25 -1) " "
                project-file-relative))))

;;; init-ibuffer.el ends here
