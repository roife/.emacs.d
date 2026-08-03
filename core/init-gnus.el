;;; -*- lexical-binding: t -*-

(use-package nnnrss
  :straight (:host github :repo "jjbarr/nnnrss")
  :config
  ;; nnnrss 0.4.1 references an unbound `id' when an item has no GUID.
  (defun nnnrss--read-id (article)
    (or (when-let* ((id (dom-child-by-tag article 'guid)))
          (string-trim (dom-text id)))
        (dom-attr article 'about)
        (when-let* ((link (dom-child-by-tag article 'link)))
          (string-trim (dom-text link)))
        (when-let* ((title (dom-child-by-tag article 'title)))
          (string-trim (dom-text title)))))
  (puthash "include-yy's blog" "Include YY" nnfeed-group-names))

;; [gnus] a newsreader, mail reader, and news server client
(use-package gnus
  :commands gnus
  :config
  (setq
   gnus-use-cache t
   gnus-use-header-prefetch t
   gnus-asynchronous t

   ;; mark duplicate copies
   gnus-suppress-duplicates t
   ;; be quiet
   gnus-interactive-exit 'quiet
   gnus-inhibit-startup-message t
   ;; Close network connections before macOS goes to sleep.
   gnus-close-on-sleep t
   ;; Do not persist killed groups or use the legacy .newsrc file.
   gnus-save-killed-list nil
   gnus-save-newsrc-file nil
   gnus-read-newsrc-file nil
   ;; Automatically restore the local Gnus state journal after an unclean
   ;; exit instead of prompting about .newsrc-dribble.
   gnus-always-read-dribble-file t
   ;; Discover local Maildir folders at startup and subscribe to them without
   ;; prompting one by one.
   gnus-check-new-newsgroups 'ask-server
   gnus-subscribe-newsgroup-method 'gnus-subscribe-alphabetically
   ;; A unified query lang
   gnus-search-use-parsed-queries t

   ;; article mode
   gnus-article-sort-functions '((not gnus-article-sort-by-number)
                                 (not gnus-article-sort-by-date))
   gnus-article-browse-delete-temp t

   ;; Display more MIME stuff
   gnus-mime-display-multipart-related-as-mixed t

   ;; Use the experienced-user Gnus UI.  Sending still asks for confirmation
   ;; through `message-confirm-send' below.
   gnus-novice-user nil
   gnus-expert-user t)

  (setq gnus-select-method '(nnnil "")
        gnus-secondary-select-methods
        `((nnmaildir "GMail"
                     (directory "~/.local/share/mail/gmail/"))
          (nnnrss "karthinks.com/index.xml")
          (nnnrss "egh0bww1.com/rss.xml")
          (nnnrss "www.rahuljuliato.com/rss.xml")
          (nnnrss "rust-analyzer.github.io/feed.xml")
          (nnatom "emacsredux.com/atom.xml")
          (nnatom "matklad.github.io/feed.xml")
          (nnatom "blog.rust-lang.org/feed.xml")
          (nnatom ,(no-littering-expand-var-file-name "rss/hackernews.atom"))
          (nndiscourse "emacs-china"
                       (nndiscourse-base-url "https://emacs-china.org")
                       (nndiscourse-auth-type user-api-key)))

        ;; Gmail already keeps a server-side Sent folder, so do not create an
        ;; additional local monthly archive.
        gnus-message-archive-group nil))

;; Read and participate in Emacs China through its Discourse API.
(use-package nndiscourse
  :straight (:host github :repo "roife/nnext"))

;; [gnus-group] group mode
(use-package gnus-group
  :config
  ;; Keep the useful state on the left and give the group name a stable
  ;; starting column:
  ;;
  ;;   process  open  unread/total  group
  ;;      #      *       12/340     INBOX
  ;;
  ;; For Discourse groups display the category description supplied by the
  ;; backend (for example, "Emacs-general") instead of its stable category ID.
  ;; Other backends retain the compact native `%c' name.
  (setq gnus-group-line-format
        (concat "%M%p%B %6,6y/%-6,6t  %P%("
                "%~(form (if (and (string-prefix-p \"nndiscourse+\""
                " gnus-tmp-group)"
                " (not (string-empty-p gnus-tmp-newsgroup-description)))"
                " (car (split-string gnus-tmp-newsgroup-description \" — \"))"
                " (gnus-short-group-name gnus-tmp-group)))@%)%0,0D\n")
        gnus-group-uncollapsed-levels 2
        gnus-group-sort-function '(gnus-group-sort-by-level gnus-group-sort-by-alphabet)
        gnus-permanently-visible-groups ".*")

  (defvar +gnus--refresh-process nil
    "The process updating external sources before a Gnus refresh.")
  (defadvice! +gnus--sync-before-refresh (refresh &rest args)
    :around #'gnus-group-get-new-news
    "Update external sources before calling the Gnus REFRESH with ARGS.
Interactive refreshes update the local Hacker News Atom feed in addition to
synchronizing mail.  Automatic Gnus demon scans only synchronize mail, so they
do not repeatedly invoke the comparatively expensive Hacker News generator."
    (if (and +gnus--refresh-process
             (process-live-p +gnus--refresh-process))
        (message "A Gnus source update is already running")
      (let ((group-buffer (current-buffer))
            (interactive-refresh (called-interactively-p 'interactive))
            (output-buffer (get-buffer-create "*gnus-source-update*"))
            (command
             (list
              (expand-file-name "scripts/update-gnus-sources"
                                user-emacs-directory))))
        (when interactive-refresh
          (setq command (append command '("--hacker-news"))))
        (with-current-buffer output-buffer
          (erase-buffer))
        (setq +gnus--refresh-process
              (make-process
               :name "gnus-source-update"
               :buffer output-buffer
               :command command
               :noquery t
               :sentinel
               (lambda (process _event)
                 (when (memq (process-status process) '(exit signal))
                   (setq +gnus--refresh-process nil)
                   (if (zerop (process-exit-status process))
                       (message "Gnus sources updated")
                     (message
                      "Some Gnus sources failed to update; see %s"
                      (buffer-name (process-buffer process))))
                   ;; Refresh even after an update failure: mbsync or the HN
                   ;; generator may have completed partially, and the existing
                   ;; local data remains usable.
                   (when (buffer-live-p group-buffer)
                     (with-current-buffer group-buffer
                       (apply refresh args)))))))
        (message (if interactive-refresh
                     "Updating Hacker News and synchronizing mail..."
                   "Synchronizing mail..."))))))

(use-package gnus-topic
  :after gnus-group
  :hook (gnus-group-mode . gnus-topic-mode)
  :bind (:map gnus-topic-mode-map
              ("TAB" . gnus-topic-fold)
              ("<tab>" . gnus-topic-fold))
  :config
  ;; Compact topic headings with an explicit collapsed marker and aggregate
  ;; unread count.  Empty topics remain visible so the hierarchy is stable.
  (setq gnus-topic-line-format "%i%(%n%)  %4A %v\n"
        gnus-topic-display-empty-topics t
        gnus-topic-indent-level 2))

(use-package gnus-demon
  :after gnus
  :config
  (gnus-demon-add-handler #'gnus-demon-scan-news 30 nil)
  (add-hook 'gnus-started-hook #'gnus-demon-init))


;; [gnus-sum] summary mode
(use-package gnus-sum
  :after gnus
  :bind (:map gnus-summary-mode-map
              ("RET" . gnus-summary-select-article-buffer)
              ("<return>" . gnus-summary-select-article-buffer))
  :config
  (defalias 'gnus-user-format-function-H #'nndiscourse-summary-liked-mark)
  (setq
   ;; Keep the original symbols, but match Gnus's native component widths:
   ;; roots are 2 columns, ancestor segments 2, and leaves 4.
   gnus-sum-thread-tree-root            "┌ "
   gnus-sum-thread-tree-false-root      "◌ "
   gnus-sum-thread-tree-single-indent   "◎ "
   gnus-sum-thread-tree-vertical        "│ "
   gnus-sum-thread-tree-indent          "  "
   gnus-sum-thread-tree-leaf-with-other "├─► "
   gnus-sum-thread-tree-single-leaf     "╰─► "
   ;; Use Gnus's native summary renderer.  Keep all status columns before
   ;; variable-width fields so its built-in mark-position tracking stays valid.
   gnus-summary-line-format "%U%R%uH %3d %-23,23f %B%s\n"
   ;; Loose threads
   gnus-simplify-subject-functions
   '(gnus-simplify-subject-re gnus-simplify-whitespace)
   gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
   ;; Filling in threads
   ;; Keep a little old context available for incomplete conversations.
   gnus-fetch-old-headers 2
   gnus-fetch-old-ephemeral-headers 2
   gnus-build-sparse-threads 'some
   ;; More threading
   gnus-show-threads t
   gnus-thread-indent-level 2
   ;; Sorting
   gnus-thread-sort-functions 'gnus-thread-sort-by-most-recent-date
   gnus-subthread-sort-functions 'gnus-thread-sort-by-date
   ;; Viewing
   gnus-view-pseudos 'automatic
   gnus-view-pseudo-asynchronously t
   ;; No auto select
   gnus-auto-select-first nil
   gnus-auto-select-next nil
   gnus-paging-select-next nil))

;; [message] Composing mail and news messages
(use-package message
  :after gnus
  :hook (message-mode . auto-fill-mode)
  :config
  (setq user-full-name "roifewu"
        user-mail-address "roifewu@gmail.com"
        message-kill-buffer-on-exit t
        message-confirm-send t
        message-signature nil
        message-mail-alias-type 'ecomplete

        message-send-mail-function #'message-use-send-mail-function

        ;; Use smtpmail to send mail through Gmail.
        send-mail-function #'smtpmail-send-it
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-user user-mail-address
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls
        smtpmail-servers-requiring-authorization "\\`smtp\\.gmail\\.com\\'"))


;; Attach marked files from Dired with `C-c RET C-a'.
(use-package gnus-dired
  :after dired
  :hook (dired-mode . turn-on-gnus-dired-mode))
