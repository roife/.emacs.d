(use-package telega
  :straight t
  :config
  (setq telega-server-libs-prefix "/usr"
        telega-proxies '((:server "127.0.0.1" :port 7891 :enable t :type (:@type "proxyTypeSocks5")))
        telega-chat-show-avatars nil
        telega-user-show-avatars nil
        telega-root-show-avatars nil
        telega-chat-fill-column 50
        telega-translate-to-language-by-default "zh"))
