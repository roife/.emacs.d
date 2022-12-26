;;; -*- lexical-binding: t -*-

;; [smartparens] Better handle for parenthesis
(use-package smartparens
  :straight t
  :hook ((prog-mode conf-mode yaml-mode eshell-mode) . smartparens-strict-mode)
  :config
  (require 'smartparens-config)

  (setq
   ;; Show-parens does this for us already (and is faster)
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil
   ;; The default is 100, but smartparen's scans are expensive, so reduce it
   sp-max-prefix-length 25
   ;; No pair has any business being longer than 4 characters, or set them buffer locally
   sp-max-pair-length 4
   )

  ;; Silence some harmless but annoying [echo-area] spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  ;; [minibuffer]
  ;; Enable `smartparens-mode' in the minibuffer for `eval-expression'.
  (add-hook 'eval-expression-minibuffer-setup-hook
            (lambda () (smartparens-mode +1)))

  ;; Likely writing lisp in the minibuffer, so disable these quote pair
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil)

  ;; Expand {|} => { | }
  ;; Expand {|} => {
  ;;   |
  ;; }
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))


  ;; In lisps ( should open a new form if before another parenthesis
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

  ;; Major-mode specific fixes
  (sp-local-pair 'ruby-mode "{" "}"
                 :pre-handlers '(:rem sp-ruby-pre-handler)
                 :post-handlers '(:rem sp-ruby-post-handler))

  ;; Don't eagerly escape Swift style string interpolation
  (sp-local-pair 'swift-mode "\\(" ")" :when '(sp-in-string-p))

  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))

  ;; Reasonable default pairs for HTML-style comments
  (sp-local-pair (append sp--html-modes '(markdown-mode gfm-mode))
                 "<!--" "-->"
                 :unless '(sp-point-before-word-p sp-point-before-same-p)
                 :actions '(insert) :post-handlers '(("| " "SPC")))

  ;; Disable electric keys in C modes because it interferes with smartparens and custom bindings.
  (with-eval-after-load 'cc-mode
    (setq-default c-electric-flag nil)
    (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")" "\177"))
      (define-key c-mode-base-map key nil))

    ;; Smartparens and cc-mode both try to autoclose angle-brackets
    ;; intelligently. The result isn't very intelligent (causes redundant
    ;; characters), so just do it ourselves.
    (bind-keys :map
               c++-mode-map
               ("<" nil)
               (">" nil))

    (defun +default-cc-sp-point-is-template-p (id action context)
      "Return t if point is in the right place for C++ angle-brackets."
      (and (sp-in-code-p id action context)
           (cond ((eq action 'insert)
                  (sp-point-after-word-p id action context))
                 ((eq action 'autoskip)
                  (/= (char-before) 32)))))

    (defun +default-cc-sp-point-after-include-p (id action context)
      "Return t if point is in an #include."
      (and (sp-in-code-p id action context)
           (save-excursion
             (goto-char (line-beginning-position))
             (looking-at-p "[ 	]*#include[^<]+"))))

    ;; ...and leave it to smartparens
    (sp-local-pair '(c++-mode objc-mode)
                   "<" ">"
                   :when '(+default-cc-sp-point-is-template-p
                           +default-cc-sp-point-after-include-p)
                   :post-handlers '(("| " "SPC")))

    (sp-local-pair '(c-mode c++-mode objc-mode java-mode)
                   "/*!" "*/"
                   :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))

  ;; Expand C-style comment blocks.
  (defun +default-open-doc-comments-block (&rest _ignored)
    (save-excursion
      (newline)
      (indent-according-to-mode)))
  (sp-local-pair
   '(js2-mode typescript-mode rjsx-mode rust-mode c-mode c++-mode objc-mode
              csharp-mode java-mode php-mode css-mode scss-mode less-css-mode
              stylus-mode scala-mode)
   "/*" "*/"
   :actions '(insert)
   :post-handlers '(("| " "SPC")
                    (" | " "*")
                    ("|[i]\n[i]" "RET")))

  (with-eval-after-load 'smartparens-ml
    (sp-with-modes '(tuareg-mode fsharp-mode)
      (sp-local-pair "(*" "*)" :actions nil)
      (sp-local-pair "(*" "*"
                     :actions '(insert)
                     :post-handlers '(("| " "SPC") ("|[i]*)[d-2]" "RET")))))

  (with-eval-after-load 'smartparens-markdown
    (sp-with-modes '(markdown-mode gfm-mode)
      (sp-local-pair "```" "```" :post-handlers '(:add ("||\n[i]" "RET")))

      ;; The original rules for smartparens had an odd quirk: inserting two
      ;; asterixex would replace nearby quotes with asterixes. These two rules
      ;; set out to fix this.
      (sp-local-pair "**" nil :actions :rem)
      (sp-local-pair "*" "*"
                     :actions '(insert skip)
                     :unless '(:rem sp-point-at-bol-p)
                     ;; * then SPC will delete the second asterix and assume
                     ;; you wanted a bullet point. * followed by another *
                     ;; will produce an extra, assuming you wanted **|**.
                     :post-handlers '(("[d1]" "SPC") ("|*" "*"))))

    ;; This keybind allows * to skip over **.
    (with-eval-after-load 'markdown-mode
      (bind-key "*"
                (lambda (&optional _)
                  (cond
                   ((looking-at-p "\\*\\* *")
                    (lambda (&rest _) (interactive) (forward-char 2)))
                   (t nil)))
                markdown-mode-map))
    )

  ;; Removes haskell-mode trailing braces
  (with-eval-after-load 'smartparens-haskell
    (sp-with-modes '(haskell-mode haskell-interactive-mode)
      (sp-local-pair "{-" "-}" :actions :rem)
      (sp-local-pair "{-#" "#-}" :actions :rem)
      (sp-local-pair "{-@" "@-}" :actions :rem)
      (sp-local-pair "{-" "-")
      (sp-local-pair "{-#" "#-")
      (sp-local-pair "{-@" "@-")))

  (with-eval-after-load 'smartparens-python
    (sp-with-modes 'python-mode
      ;; Automatically close f-strings
      (sp-local-pair "f\"" "\"")
      (sp-local-pair "f\"\"\"" "\"\"\"")
      (sp-local-pair "f'''" "'''")
      (sp-local-pair "f'" "'"))
    ;; Original keybind interferes with smartparens rules
    (define-key python-mode-map (kbd "DEL") nil))

  (with-eval-after-load 'web-mode
    (defun +web-is-auto-close-style-3 (_id action _context)
      (and (eq action 'insert)
           (eq web-mode-auto-close-style 3)))
    (sp-local-pair 'web-mode "<" ">" :unless '(:add +web-is-auto-close-style-3))

    ;; let smartparens handle these
    (setq web-mode-enable-auto-quoting nil
          web-mode-enable-auto-pairing t)

    ;; 1. Remove web-mode auto pairs whose end pair starts with a latter
    ;;    (truncated autopairs like <?p and hp ?>). Smartparens handles these
    ;;    better.
    ;; 2. Strips out extra closing pairs to prevent redundant characters
    ;;    inserted by smartparens.
    (dolist (alist web-mode-engines-auto-pairs)
      (setcdr alist
              (cl-loop for pair in (cdr alist)
                       unless (string-match-p "^[a-z-]" (cdr pair))
                       collect (cons (car pair)
                                     (string-trim-right (cdr pair)
                                                        "\\(?:>\\|]\\|}\\)+\\'")))))
    (setq web-mode-auto-pairs (delq nil web-mode-auto-pairs)))
  )
