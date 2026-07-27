;;; -*- lexical-binding: t -*-

(defvar +elfeed-local-dir (expand-file-name "rss/" user-emacs-directory))

;; [elfeed] Read rss within Emacs
(use-package elfeed
  :straight t
  :init
  (require 'auth-source)

  (defadvice! +elfeed-update-after-local-feeds-a (fn &rest args)
    :around '(elfeed-update elfeed-update-background)
    "Refresh local feeds asynchronously, then call FN with ARGS."
    (if (process-live-p (get-process "elfeed-local-feeds"))
        (message "Local feed update is already running")
      (let ((process-environment (copy-sequence process-environment))
            (command (expand-file-name "scripts/update-elfeed-feeds" user-emacs-directory)))
        (setenv "REDDIT_PRIVATE_RSS_TOKEN"
                (auth-source-pick-first-password :host "reddit-private-rss"
                                                 :port "rss"))
        (make-process :name "elfeed-local-feeds"
                      :buffer (get-buffer-create "*elfeed-local-feeds*")
                      :command (list command)
                      :noquery t
                      :sentinel
                      (lambda (process _event)
                        (unless (process-live-p process)
                          (unless (zerop (process-exit-status process))
                            (message "Local feed update failed; see *elfeed-local-feeds*"))
                          (apply fn args)))))))

  (run-at-time "1 min" (* 60 60 2) #'elfeed-update-background)
  :bind (:map elfeed-search-mode-map
              ("g" . elfeed-update)
              ("G" . elfeed-search-update--force)
              :map elfeed-show-mode-map
              ("M-v" . scroll-down-command)
              ("j" . scroll-up-line)
              ("k" . scroll-down-line))
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-feeds `((,(concat "file://" (expand-file-name "feed.atom" +elfeed-local-dir)) hackernews)
                       ;; emacs
                       ("https://karthinks.com/index.xml" karthinks)
                       ("https://emacsredux.com/atom.xml" redux)
                       ("https://egh0bww1.com/rss.xml" includeyy)
                       ("https://www.rahuljuliato.com/rss.xml" rahul)
                       ("https://emacs-china.org/latest.rss" emacs-china)
                       (,(concat "file://" (expand-file-name "reddit-emacs.atom" +elfeed-local-dir)) r/emacs)
                       ;; programming
                       ("https://matklad.github.io/feed.xml" matklad)
                       ("https://rust-analyzer.github.io/feed.xml" rust-analyzer)
                       ("https://blog.rust-lang.org/feed.xml" rust))
        elfeed-enclosure-default-dir (expand-file-name "elfeed/" user-emacs-directory)
        elfeed-show-entry-switch #'switch-to-buffer
        elfeed-show-entry-delete #'delete-window
        elfeed-search-clipboard-type 'CLIPBOARD
        elfeed-search-title-max-width 100
        elfeed-search-title-min-width 30
        elfeed-search-trailing-width 25
        elfeed-show-truncate-long-urls t
        elfeed-show-unique-buffers t)

  (add-hook 'elfeed-db-update-hook #'+refresh-tab-bar)
  (add-hook 'elfeed-tag-hook #'+refresh-tab-bar)
  (add-hook 'elfeed-untag-hook #'+refresh-tab-bar)

  ;; Ignore db directory in recentf
  (push elfeed-db-directory recentf-exclude)
  )
