;;; -*- lexical-binding: t -*-

(defun +reopen-file-with-sudo ()
  (interactive)
  (find-alternate-file (format "/sudo::%s" (buffer-file-name))))

(provide 'init-util)
