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
        ibuffer-use-other-window t)
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
          (ebib-filter '(or (mode . ebib-entry-mode)
                            (mode . ebib-index-mode)
                            (mode . ebib-log-mode)
                            (mode . ebib-multiline-mode)
                            (mode . ebib-strings-mode)))
          (elfeed-filter '(or (mode . elfeed-search-mode)
                              (mode . elfeed-show-mode)))
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
      (add-to-list 'ibuffer-filter-groups (list "Ebib" ebib-filter))
      (add-to-list 'ibuffer-filter-groups (list "Elfeed" elfeed-filter))
      (add-to-list 'ibuffer-filter-groups (list "Temporary buffers" starred-name-filter) :append)
      )

    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative))
    )

  (define-ibuffer-column +size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))


  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 25 25 :left :elide)
                " "
                (+size-h 8 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                project-file-relative)))
  )
