;;; -*- lexical-binding: t -*-

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("RET" . +ibuffer-visit-buffer-in-popper))
  :config
  (defun +ibuffer-visit-buffer-in-popper ()
    (interactive)
    (if (window-parameter nil 'window-side)
        (let ((win (selected-window)))
          (ibuffer-visit-buffer-other-window)
          (delete-window win))
      (ibuffer-visit-buffer)))

  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-use-other-window t
        ibuffer-human-readable-size t)
  )

;; [ibuffer-project] Group ibuffer's list by project root
(use-package ibuffer-project
  :straight t
  :hook (ibuffer . +ibuffer-project-activete)
  :config
  ;; HACK: Push temperary buffers in a standalone group
  (defun +ibuffer-project-activete ()
    "Activate ibuffer-project"
    (interactive)
    (let ((starred-name-filter '(starred-name . ""))
          (scratch-filter '(name . "^\\*scratch\\(.*\\)\\*$"))
          (eww-filter '(or (mode . eww-mode)
                           (mode . eww-bookmark-mode)
                           (mode . eww-history-mode)
                           (mode . eww-buffers-mode)
                           (mode . eww-search-annotations-mode)))
          (telega-filter '(or (mode . telega-chat-mode)
                              (mode . telega-root-mode)
                              (mode . telega-image-mode)
                              (mode . telega-webpage-mode)))
          (xwidget-filter '(mode . xwidget-webkit-mode)))
      (setq ibuffer-filter-groups
            (mapcar (lambda (p) (cons (car p) `((and ,(car (cdr p)) (not ,starred-name-filter)))))
                    (ibuffer-project-generate-filter-groups)))
      ;; ChatGPT buffer should be added first to avoid being grouped into projects
      (add-to-list 'ibuffer-filter-groups (list "Telega" telega-filter))
      (add-to-list 'ibuffer-filter-groups (list "Scratch" scratch-filter))
      (add-to-list 'ibuffer-filter-groups (list "Eww" eww-filter))
      (add-to-list 'ibuffer-filter-groups (list "Xwidget" xwidget-filter))
      (add-to-list 'ibuffer-filter-groups (list "Temporary buffers" starred-name-filter) :append)
      )

    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative))
    )

  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 25 25 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                project-file-relative)))
  )
