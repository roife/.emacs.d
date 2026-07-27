;;; -*- lexical-binding: t -*-

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
        '((nnmaildir "GMail"
                     (directory "~/.local/share/mail/gmail/")))

        ;; Gmail already keeps a server-side Sent folder, so do not create an
        ;; additional local monthly archive.
        gnus-message-archive-group nil))


;; [gnus-group] group mode
(use-package gnus-group
  :config
  ;;               indentation ------------.
  ;;       #      process mark ----------. |
  ;;                     level --------. | |
  ;;                subscribed ------. | | |
  ;;       %          new mail ----. | | | |
  ;;       *   marked articles --. | | | | |
  ;;                             | | | | | |  Ticked    New     Unread  open-status Group
  (setq gnus-group-line-format "%M%m%S%L%p%P %1(%7i%) %3(%7U%) %3(%7y%) %4(%B%-45G%)\n"
        gnus-group-sort-function
        '(gnus-group-sort-by-level gnus-group-sort-by-alphabet))

  (defvar +gnus--mbsync-process nil
    "The active mbsync process.")
  (defadvice! +gnus--sync-before-refresh (refresh &rest args)
    :around #'gnus-group-get-new-news
    "Synchronize mail before calling the Gnus REFRESH function with ARGS."
    (if (and +gnus--mbsync-process
             (process-live-p +gnus--mbsync-process))
        (message "Mail sync is already running")
      (let ((group-buffer (current-buffer))
            (output-buffer (get-buffer-create "*mbsync*")))
        (with-current-buffer output-buffer
          (erase-buffer))
        (setq +gnus--mbsync-process
              (make-process
               :name "mbsync"
               :buffer output-buffer
               :command '("mbsync" "--all")
               :noquery t
               :sentinel
               (lambda (process _event)
                 (when (memq (process-status process) '(exit signal))
                   (setq +gnus--mbsync-process nil)
                   (if (zerop (process-exit-status process))
                       (when (buffer-live-p group-buffer)
                         (with-current-buffer group-buffer
                           (apply refresh args)))
                     (message "Mail sync failed; see *mbsync*"))))))
        (message "Synchronizing mail...")))))

(use-package gnus-topic
  :after gnus-group
  :hook (gnus-group-mode . gnus-topic-mode))

(use-package gnus-demon
  :after gnus
  :config
  (gnus-demon-add-handler #'gnus-demon-scan-news 120 nil)
  (add-hook 'gnus-started-hook #'gnus-demon-init))


;; [gnus-sum] summary mode
(use-package gnus-sum
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
   gnus-simplify-subject-functions '(gnus-simplify-subject-re gnus-simplify-whitespace)
   gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
   ;; Filling in threads
   ;; 2 old articles are enough for memory
   gnus-fetch-old-headers 2
   gnus-fetch-old-ephemeral-headers 2
   gnus-build-sparse-threads 'some
   ;; More threading
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
        message-signature user-full-name
        message-mail-alias-type 'ecomplete

        message-send-mail-function #'message-use-send-mail-function

        ;; Use smtpmail to send mail through Gmail.
        send-mail-function #'smtpmail-send-it
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-user user-mail-address
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls
        smtpmail-servers-requiring-authorization
        "\\`smtp\\.gmail\\.com\\'"))


;; Attach marked files from Dired with `C-c RET C-a'.
(use-package gnus-dired
  :after dired
  :hook (dired-mode . turn-on-gnus-dired-mode))
