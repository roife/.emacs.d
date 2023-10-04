;;; -*- lexical-binding: t -*-

;; [eww] Builtin browser
(use-package eww
  :config
  (setq eww-retrieve-command '("readable")
        shr-max-image-proportion 0.5))
