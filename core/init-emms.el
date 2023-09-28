;;; -*- lexical-binding: t -*-


;; [emms] Enjoy music inside Emacs
(use-package emms
  :straight t
  :config
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory "~/Music/")
  )

(use-package consult-emms
  :straight (:host github :repo "Hugo-Heagren/consult-emms" :files ("dist" "*.el"))
  )
