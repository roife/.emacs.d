;;; -*- lexical-binding: t -*-

;; [browse-url] Pass a URL to browser
(use-package browse-url
  :defines dired-mode-map
  :bind (("C-, o" . browse-url-at-point)
         ("C-, e" . browse-url-emacs))
  :config
  (setq browse-url-browser-function 'xwidget-webkit-browse-url)
  )


;; [isearch] Use builtin isearch to replace `anzu'
(use-package isearch
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char))
  :config
  (setq
   ;; Record isearch in minibuffer history, so C-x ESC ESC can repeat it.
   isearch-resume-in-command-history t
   ;; One space can represent a sequence of whitespaces
   isearch-lax-whitespace t
   ;; direction change
   isearch-repeat-on-direction-change t
   ;; M-< and M-> move to the first/last occurrence of the current search string.
   isearch-allow-motion t
   isearch-motion-changes-direction t
   ;; lazy-count
   isearch-lazy-count t
   lazy-highlight-cleanup nil
   lazy-highlight-buffer t
   ;; search-ring
   search-ring-max 200
   regexp-search-ring-max 200))


;; [goto-addr] Click to open URL
(use-package goto-addr
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))


;; [avy] Jump with several key strock
(use-package avy
  :straight t
  :bind (("C-, ." . avy-goto-char)
         ("C-, ," . avy-goto-char-2)
         ("C-, l" . avy-goto-line))
  :hook (after-init . avy-setup-default)
  :config
  ;; overlay is used during isearch, `pre' style makes avy keys evident.
  (setq avy-styles-alist '((avy-isearch . pre)))
  )


;; [avy-pinyin] Avy support for pinyin
(use-package ace-pinyin
  :straight t
  :after avy
  :init (ace-pinyin-global-mode t))


;; [avy-link] Avy support for links
(use-package ace-link
  :straight t
  :after avy
  :bind (("C-, j" . ace-link))
  :init
  (ace-link-setup-default (kbd "C-, j"))
  )


;; [expand-region] Select a region quickly
(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))


;; [ialign] Interactive align
(use-package ialign
  :straight t)


;; [hideshow] Code folding
(use-package hideshow
  :hook ((prog-mode conf-mode yaml-mode) . hs-minor-mode)
  :bind (("C-c h TAB" . hs-toggle-hiding)
         ("C-c h `" . hs-toggle-all))
  :config
  ;; More functions
  ;; @see https://karthinks.com/software/simple-folding-with-hideshow/
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-toggle-all ()
    "Toggle hide/show all."
    (interactive)
    (pcase last-command
      ('hs-toggle-all
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; Display line counts
  (defun +hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (let ((lines (number-to-string (count-lines (overlay-start ov) (overlay-end ov)))))
                     (concat " "
                             (propertize (concat " ... L" lines " ") 'face '(:inherit shadow :height 0.8 :box t))
                             " "))
                   )))
  (setq hs-set-up-overlay #'+hs-display-code-line-counts)

  ;; hide-show by indentation
  (defun +fold--hideshow-empty-line-p (_)
    (string= "" (string-trim (thing-at-point 'line 'no-props))))

  (defun +fold--hideshow-geq-or-empty-p (base-indent)
    (or (+fold--hideshow-empty-line-p base-indent)
        (>= (current-indentation) base-indent)))

  (defun +fold--hideshow-g-or-empty-p (base-indent)
    (or (+fold--hideshow-empty-line-p base-indent)
        (> (current-indentation) base-indent)))

  (defun +fold--hideshow-seek (start direction before skip predicate base-indent)
    "Seeks forward (if direction is 1) or backward (if direction is -1) from start, until predicate
fails. If before is nil, it will return the first line where predicate fails, otherwise it returns
the last line where predicate holds."
    (save-excursion
      (goto-char start)
      (goto-char (point-at-bol))
      (let ((bnd (if (> 0 direction)
                     (point-min)
                   (point-max)))
            (pt (point)))
        (when skip (forward-line direction))
        (cl-loop while (and (/= (point) bnd) (funcall predicate base-indent))
                 do (progn
                      (when before (setq pt (point-at-bol)))
                      (forward-line direction)
                      (unless before (setq pt (point-at-bol)))))
        pt)))

  (defun +fold-hideshow-indent-range (&optional point)
    "Return the point at the begin and end of the text block with the same (or
greater) indentation. If `point' is supplied and non-nil it will return the
begin and end of the block surrounding point."
    (save-excursion
      (when point
        (goto-char point))
      (let ((base-indent (current-indentation))
            (begin (point))
            (end (point)))
        (setq begin (+fold--hideshow-seek begin -1 t nil #'+fold--hideshow-geq-or-empty-p base-indent)
              begin (+fold--hideshow-seek begin 1 nil nil #'+fold--hideshow-g-or-empty-p base-indent)
              end   (+fold--hideshow-seek end 1 t nil #'+fold--hideshow-geq-or-empty-p base-indent)
              end   (+fold--hideshow-seek end -1 nil nil #'+fold--hideshow-empty-line-p base-indent))
        (list begin end base-indent))))

  (defun +fold-hideshow-forward-block-by-indent-fn (_arg)
    (let ((start (current-indentation)))
      (forward-line)
      (unless (= start (current-indentation))
        (let ((range (+fold-hideshow-indent-range)))
          (goto-char (cadr range))
          (end-of-line)))))

  ;; support for special modes
  (setq hs-special-modes-alist
        (append
         '((yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
                      ""
                      "#"
                      +fold-hideshow-forward-block-by-indent-fn nil)
           (ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
                      "end\\|[]}]"
                      "#\\|=begin"
                      ruby-forward-sexp)
           (matlab-mode "if\\|switch\\|case\\|otherwise\\|while\\|for\\|try\\|catch"
                        "end"
                        nil (lambda (_arg) (matlab-forward-sexp)))
           (nxml-mode "<!--\\|<[^/>]*[^/]>"
                      "-->\\|</[^/>]*[^/]>"
                      "<!--" sgml-skip-tag-forward nil)
           (latex-mode
            ;; LaTeX-find-matching-end needs to be inside the env
            ("\\\\begin{[a-zA-Z*]+}\\(\\)" 1)
            "\\\\end{[a-zA-Z*]+}"
            "%"
            (lambda (_arg)
              ;; Don't fold whole document, that's useless
              (unless (save-excursion
                        (search-backward "\\begin{document}"
                                         (line-beginning-position) t))
                (LaTeX-find-matching-end)))
            nil))
         hs-special-modes-alist
         '((t))))
  )


;; [project] Project manager
(use-package project
  :bind (:map project-prefix-map
              ("m" . magit-status)
              ("s" . shell))
  :config
  (setq project-switch-commands '((project-find-file "Find file")
                                  (project-find-regexp "Find regexp")
                                  (project-dired "Dired")
                                  (project-eshell "Eshell")
                                  (project-search "Search")
                                  (shell "Shell")
                                  (magit-status "Magit")))

  (defun +project-previous-buffer (arg)
    "Toggle to the previous buffer that belongs to current project."
    (interactive "P")
    (unless arg
      (if-let ((pr (project-current)))
          (switch-to-buffer
           (->> (project--buffer-list pr)
                (--remove (or (minibufferp it)
                              (get-buffer-window-list it)))
                (car))))))

  ;; Use [fd] to find file in project
  (defun +search-project-files-with-fd (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -H -t f -0 . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))
  (cl-defmethod project-files ((project (head local)) &optional dirs)
    "Override `project-files' to use `fd' in local projects."
    (mapcan #'+search-project-files-with-fd
            (or dirs (list (project-root project)))))
  )



;; [wgrep] Edit a grep buffer and apply changes to the file buffer
(use-package wgrep
  :straight t
  :config
  (setq wgrep-auto-save-buffer t)
  )


;; [rg] support for ripgrep
(use-package rg
  :straight t)


;; [vundo] Undo tree
(use-package vundo
  :straight t
  :config
  (setq vundo-compact-display t)
  )


;; [imenu] Jump to function definitions
(use-package imenu
  :hook ((prog-mode conf-mode yaml-mode markdown-mode org-mode) . imenu-add-menubar-index)
  )


;; [re-builder]
(use-package re-builder
  :ensure nil
  :commands re-builder
  :bind (:map reb-mode-map
              ("C-c C-k" . reb-quit)
              ("C-c C-p" . reb-prev-match)
              ("C-c C-n" . reb-next-match))
  :config
  (setq reb-re-syntax 'string))


;; [separedit]
(use-package separedit
  :straight t
  :bind (:map prog-mode-map
              ("C-c '" . separedit))
  :config
  (setq separedit-default-mode 'markdown-mode))
