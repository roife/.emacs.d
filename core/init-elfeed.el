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
                       ("https://rss.utgd.net/feed" untaged)
                       ;; tech
                       ("https://karthinks.com/index.xml" karthinks-emacs)
                       ("https://matklad.github.io/feed.xml" matklad)
                       ("https://rust-analyzer.github.io/feed.xml" rust-analyzer)
                       ;; ytb
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCz0ONCn6eRcDJGsUzupc3TA" ytb-links)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVTifvD7WFz1Z-AnEzUoUUA" ytb-fansuki)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCiQo406SKypmtAQXIHdZ6mA" ytb-birchpunk)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCiQo406SKypmtAQXIHdZ6mA" ytb-leya)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCue63vweCtx5j6jylVZsd7w" ytb-hummingbird)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCRewJ9oGONpRm_kaYU6UPGQ" ytb-kolar)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMZZNUTkXjuWlYyB_RwxNKA" ytb-wsf-xmm)
                       ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMZZNUTkXjuWlYyB_RwxNKA" ytb-foodie-gao))
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
