(use-package telega
  :straight t
  :custom-face
  (telega-msg-heading ((t (:inherit hl-line :background nil))))
  (telega-msg-inline-reply ((t (:inherit (hl-line font-lock-function-name-face)))))
  (telega-msg-inline-forward ((t (:inherit (hl-line font-lock-type-face)))))
  :hook ((telega-chat-mode . visual-line-mode))
  :bind (:map telega-chat-button-map
              ("h" . nil))
  :config
  (setq telega-chat-show-avatars nil
        telega-user-show-avatars nil
        telega-root-show-avatars nil
        telega-chat-fill-column 58
        telega-translate-to-language-by-default "zh"
        telega-chat-input-markups '(nil "org")
        telega-chat-prompt-format "▶ "
        telega-completing-read-function completing-read-function
        ;; avoid emoji display problem
        telega-emoji-use-images nil)

  (if (eq system-type 'darwin)
      (setq telega-proxies '((:server "127.0.0.1" :port 7890 :enable t :type (:@type "proxyTypeSocks5"))))
    (setq telega-server-libs-prefix "/usr"
          telega-proxies '((:server "127.0.0.1" :port 7891 :enable t :type (:@type "proxyTypeSocks5")))))
  )
