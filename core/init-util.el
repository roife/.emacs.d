;;; -*- lexical-binding: t -*-

;; custom directory
(defvar +blog-dir (expand-file-name "~/source/roife.github.io/"))
(defvar +ebib-bib-dir (expand-file-name "~/Documents/papers/"))
(defvar +elfeed-enclosure-dir (expand-file-name "~/Downloads/"))

;; Thanks to DOOM Emacs
(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The hook(s) to add to.
  2. Optional properties :local, :append, and/or :depth [N].
  3. The function(s) to be added: this can be a quoted function, a quoted list
     thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
     implicitly be wrapped in a lambda).

\(fn HOOKS [:append :local [:depth N] :remove :call-immediately] FUNCTIONS-OR-FORMS...)"
  (declare (indent defun))
  (let* ((hook-forms hooks)
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p call-immediately-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))
        (:call-immediately (setq call-immediately-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil)) next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (func (list ,@func-forms))
         (dolist (hook (nreverse ',hook-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))
         ,(if call-immediately-p `(funcall func))))))

(defmacro defadvice! (symbol arglist &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &rest [WHERE PLACES...] BODY\)"
  (declare (indent defun))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro +advice-pp-to-prin1! (&rest body)
  "Define an advice called SYMBOL that map `pp' to `prin1' when called.
PLACE is the function to which to add the advice, like in `advice-add'.

\(fn SYMBOL &rest [PLACES...]\)"
  `(progn
     (dolist (target (list ,@body))
       (advice-add target :around #'+call-fn-with-pp-to-prin1))))

(defmacro defun-call! (symbol args &rest body)
  "Define a function and optionally apply it with specified arguments.

\(fn SYMBOL ARGS &optional [DOCSTRING] &optional [:call-with APPLY_ARGS] BODY\)"
  (declare (indent defun))
  (let* ((docstring (if (stringp (car body)) (pop body)))
         (apply-body (if (eq :call-with (car body))
                         (progn
                           (cl-assert (eq (pop body) :call-with))
                           (pop body))
                       nil)))
    `(progn
       (defun ,symbol ,args
         ,@(if docstring
               (cons docstring body)
             body))
       (apply ',symbol
              ,(if (listp apply-body)
                   `(list ,@apply-body)
                 `(list ,apply-body))))))

(defun +call-fn-with-pp-to-prin1 (fn &rest args)
  "Call FN with ARGS, map `pp' to `prin1' when called."
  (cl-letf (((symbol-function #'pp) #'prin1)
            ((symbol-function #'pp-to-string) #'prin1-to-string))
    (apply fn args)))

(defun +unfill-region (start end)
  "Replace newline chars in region from START to END by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

(defun +temp-buffer-p (buffer)
  "Return t if BUFFER is temporary."
  (string-match-p "^ " (buffer-name buffer)))

;; theme changed hook
(defvar +theme-changed-hook nil
  "Hook run after the theme is changed.")
(defadvice! +run-theme-changed-hook-a (&rest _)
  :after #'enable-theme
  (run-hooks '+theme-changed-hook))
