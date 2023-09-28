;;; -*- lexical-binding: t -*-


;; [elfeed] Read rss within Emacs
(use-package elfeed
  :straight t
  :bind (:map elfeed-search-mode-map
         ("g" . elfeed-update)
         ("G" . elfeed-search-update--force)
         :map elfeed-show-mode-map
         ("M-v" . scroll-down-command)
         ("j" . scroll-up-line)
         ("k" . scroll-down-line))
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-feeds '(("https://sspai.com/feed" sspai)
                       ("https://nikonrumors.com/feed/" nikon-rumors)
                       ("https://rss.utgd.net/feed" utgd)
                       ;; bilibili
                       ("https://rsshub.app/bilibili/user/video/3816626" bili-links)
                       ("https://rsshub.app/bilibili/user/video/110683415" bili-taro)
                       ("https://rsshub.app/bilibili/user/video/389245745" bili-leya)
                       ("https://rsshub.app/bilibili/user/video/63231" bili-fansuki))
        elfeed-enclosure-default-dir +elfeed-enclosure-dir
        elfeed-search-filter "@4-months-ago +"
        elfeed-search-clipboard-type 'CLIPBOARD
        elfeed-search-title-max-width 100
        elfeed-search-title-min-width 30
        elfeed-search-trailing-width 25
        elfeed-show-truncate-long-urls t
        elfeed-show-unique-buffers t
        ;; elfeed-search-date-format '("%F %R" 16 :left)
        )
  )
