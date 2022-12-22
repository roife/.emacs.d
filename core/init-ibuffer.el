;;; -*- lexical-binding: t -*-

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-use-other-window t)
  )

;; Group ibuffer's list by project root
(use-package ibuffer-project
  :straight t
  :hook (ibuffer . +ibuffer-project-activete)
  :defines ibuffer-project-groups
  :config
  ;; HACK: Push temperary buffers in a standalone group
  (defun +ibuffer-project-activete ()
    "Activate ibuffer-project"
    (let ((starred-name-filter '(starred-name . "")))
      (setq ibuffer-filter-groups
            (mapcar (lambda (p) (cons (car p)
                                 `((and ,(car (cdr p)) (not ,starred-name-filter)))))
                    (ibuffer-project-generate-filter-groups)))
      (add-to-list 'ibuffer-filter-groups (list "Temporary buffers" starred-name-filter) :append))

    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative))
    )

  (custom-set-variables
   '(ibuffer-formats
     '((mark modified read-only locked " "
             (name 18 18 :left :elide)
             " "
             (size 9 -1 :right)
             " "
             (mode 16 16 :left :elide)
             " " project-file-relative))))
  )


