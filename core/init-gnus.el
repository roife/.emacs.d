;;; -*- lexical-binding: t -*-

;; [gnus] a newsreader, mail reader, and news server client
(use-package gnus
  :config
  (setq
   ;; speed up with async and caches
   gnus-use-cache t
   gnus-use-header-prefetch t
   gnus-asynchronous t

   ;; cache
   gnus-cache-enter-articles '(ticked dormant unread)
   gnus-cache-remove-articles '(read)
   gnus-cacheable-groups "^\\(nntp\\|nnimap\\)"

   ;; mark duplicate copies
   gnus-suppress-duplicates t
   ;; be quiet
   gnus-interactive-exit 'quiet
   gnus-inhibit-startup-message t
   ;; startup
   gnus-save-killed-list nil
   gnus-check-new-newsgroups nil
   ;; No other newsreader is used
   gnus-save-newsrc-file nil
   gnus-read-newsrc-file nil
   gnus-subscribe-newsgroup-method 'gnus-subscribe-interactively
   ;; A unified query lang
   gnus-search-use-parsed-queries t

   ;; article mode
   gnus-article-sort-functions '((not gnus-article-sort-by-number)
                                 (not gnus-article-sort-by-date))
   gnus-article-browse-delete-temp t

   ;; Display more MINE stuff
   gnus-mime-display-multipart-related-as-mixed t

   ;; no more confirmations
   gnus-novice-user nil
   gnus-expert-user t
   )

  (setq gnus-select-method '(nnnil "")
        gnus-secondary-select-methods '((nnimap "GMail"
                                                (nnimap-inbox "INBOX")
                                                (nnimap-address "imap.gmail.com")
                                                (nnimap-server-port "imaps")
                                                (nnimap-stream ssl)
                                                (nnimap-expunge 'never))
                                        (nnimap "Outlook"
                                                (nnimap-inbox "INBOX")
                                                (nnimap-address "outlook.office365.com")
                                                (nnimap-server-port 993)
                                                (nnimap-stream ssl)
                                                (nnimap-expunge 'never))
                                        (nntp "gwene.io.github.matklad")
                                        ))
  )


;; [gnus-group] group mode
(use-package gnus-group
  :hook (gnus-group-mode . gnus-topic-mode)
  :after gnus
  :config
  ;;               indentation ------------.
  ;;       #      process mark ----------. |
  ;;                     level --------. | |
  ;;                subscribed ------. | | |
  ;;       %          new mail ----. | | | |
  ;;       *   marked articles --. | | | | |
  ;;                             | | | | | |  Ticked    New     Unread  open-status Group
  (setq gnus-group-line-format "%M%m%S%L%p%P %1(%7i%) %3(%7U%) %3(%7y%) %4(%B%-45G%) %d\n"
        gnus-group-sort-function '(gnus-group-sort-by-level gnus-group-sort-by-alphabet))
  )


;; [gnus-sum] summary mode
(use-package gnus-sum
  :hook (gnus-select-group . gnus-group-set-timestamp)
  :after gnus
  :config
  (setq
   ;; Pretty marks
   gnus-sum-thread-tree-root            "┌ "
   gnus-sum-thread-tree-false-root      "◌ "
   gnus-sum-thread-tree-single-indent   "◎ "
   gnus-sum-thread-tree-vertical        "│"
   gnus-sum-thread-tree-indent          "  "
   gnus-sum-thread-tree-leaf-with-other "├─►"
   gnus-sum-thread-tree-single-leaf     "╰─►"
   gnus-summary-line-format "%U%R %3d %[%-23,23f%] %B %s\n"
   ;; Loose threads
   gnus-summary-make-false-root 'adopt
   gnus-simplify-subject-functions '(gnus-simplify-subject-re gnus-simplify-whitespace)
   gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
   ;; Filling in threads
   ;; 2 old articles are enough for memory
   gnus-fetch-old-headers 2
   gnus-fetch-old-ephemeral-headers 2
   gnus-build-sparse-threads 'some
   ;; More threading
   gnus-show-threads t
   gnus-thread-indent-level 2
   gnus-thread-hide-subtree nil
   ;; Sorting
   gnus-thread-sort-functions 'gnus-thread-sort-by-most-recent-date
   gnus-subthread-sort-functions 'gnus-thread-sort-by-date
   ;; Viewing
   gnus-view-pseudos 'automatic
   gnus-view-pseudos-separately t
   gnus-view-pseudo-asynchronously t
   ;; No auto select
   gnus-auto-select-first nil
   gnus-auto-select-next nil
   gnus-paging-select-next nil)
  )


;; [message] Composing mail and news messages
(use-package message
  :after gnus
  :hook (message-mode . auto-fill-mode)
  :config
  (setq user-full-name "roife"
        user-mail-address "roifewu@gmail.com"
        message-kill-buffer-on-exit t
        message-signature user-full-name
        message-mail-alias-type 'ecomplete

        message-send-mail-function #'message-use-send-mail-function

        ;; Use sendmail package to send mails.
        send-mail-function #'smtpmail-send-it

        ;; And sendmail relies on `smtpmail'.
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-user user-mail-address
        smtpmail-smtp-service 587)
  )
