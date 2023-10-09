;;; -*- lexical-binding: t -*-

(use-package chatgpt-shell
  :straight t
  :config
  (require 'auth-source)
  (let ((auth-info (car (auth-source-search :user "roife-openai"))))
    (setq chatgpt-shell-api-url-base (plist-get auth-info :host)
          chatgpt-shell-openai-key (plist-get auth-info :secret))))
