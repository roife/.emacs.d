;;; -*- lexical-binding: t -*-

(use-package elfeed
  :straight t
  ;; :bind (:map elfeed-search-mode-map
  ;;        ("g" . elfeed-update)
  ;;        ("G" . elfeed-search-update--force)
  ;;        :map elfeed-show-mode-map
  ;;        ("M-v" . scroll-down-command)
  ;;        ("j" . scroll-up-line)
  ;;        ("k" . scroll-down-line))
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-feeds '(("https://rsshub.app/v2ex/topics/hot" v2ex)
                       ("http://www.v2ex.com/feed/apple.xml" v2ex apple)
                       ("https://www.solidot.org/index.rss" tech)
                       ("https://news.ycombinator.com/rss" hacknews))
        elfeed-enclosure-default-dir "~/Downloads/"
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
