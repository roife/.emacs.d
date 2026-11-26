;; -*- lexical-binding: t; -*-
(use-package telega
  :straight (:host github :repo "zevlg/telega.el"
                   :files (:defaults "contrib/*.el" "etc"))
  :commands +telega-toggle-work-mode
  :require-incrementally
  (password-cache cl-lib find-func emacsbug
                  telega-customize telega-server telega-root telega-ins
                  telega-match telega-filter telega-chat telega-folders
                  telega-user telega-info telega-media telega-sticker
                  telega-util telega-vvnote telega-webpage telega-notifications
                  telega-modes telega-i18n telega-story telega-completions
                  telega-tdlib telega-tdlib-events t)
  :preface
  (defun +telega-install-tdlib ()
    "Fetch and install telega's expected TDLib commit under ~/.local."
    (interactive)
    (unless (eq system-type 'darwin)
      (user-error "+telega-install-tdlib currently only supports macOS"))
    (require 'telega)
    (require 'compile)
    (let ((script (expand-file-name "scripts/install-telega-tdlib" user-emacs-directory))
          (version (telega-dockrefile-tdlib-version))
          (prefix (expand-file-name "~/.local")))
      (unless (file-executable-p script)
        (user-error "TDLib install script is not executable: %s" script))
      (compilation-start
       (mapconcat #'shell-quote-argument (list script version prefix) " ")
       'compilation-mode
       (lambda (_) "*telega-install-tdlib*"))))

  (defun +telega-toggle-archive ()
    "Toggle telega root buffer between the main and archive filters."
    (interactive)
    (let* ((archive-p (equal (telega-filter-active) '(archive)))
           (filter (if archive-p (list telega-filter-default) '(archive))))
      (telega-filters-push filter)
      (message "telega filter: %s" (if archive-p telega-filter-default 'archive))))

  :custom-face
  (telega-msg-heading ((t (:inherit hl-line :background unspecified))))
  (telega-msg-inline-reply ((t (:inherit (hl-line font-lock-function-name-face)))))
  (telega-msg-inline-forward ((t (:inherit (hl-line font-lock-type-face)))))
  (telega-msg-self-title ((t (:bold t :italic t))))
  :bind (:map telega-chat-button-map
              ("h" . nil)
              :map telega-chat-mode-map
              ("C-c C-c" . +telega-toggle-work-mode)
              :map telega-root-mode-map
              ("C-c C-c" . +telega-toggle-work-mode)
              ("A" . +telega-toggle-archive))
  :hook ((telega-chat-mode . corfu-mode)
         (telega-chat-mode . +telega-completions-setup-capf))
  :config
  (defvar +telega-work-mode nil
    "Whether Telega uses the work display settings.")

  (defun +telega-toggle-work-mode ()
    "Toggle Telega's work display settings across chats."
    (interactive)
    (setq +telega-work-mode (not +telega-work-mode))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'telega-chat-mode) telega-chatbuf--ewoc)
          (with-telega-buffer-modify
            (telega-save-cursor
              (telega-chatbuf--footer-update)
              (ewoc-refresh telega-chatbuf--ewoc))))))
    (message "Telega work mode %s" (if +telega-work-mode "enabled" "disabled")))

  (defadvice! +telega-work-sender-title-faces-a (orig &rest args)
    :around #'telega-msg-sender-title-faces
    (if +telega-work-mode
        '(default)
      (apply orig args)))

  (defadvice! +telega-work-message-trail-a (orig msg)
    :around #'telega-ins--message-date-and-status
    (if +telega-work-mode
        (cl-letf* ((original-symbol (symbol-function 'telega-symbol))
                   ((symbol-function 'telega-ins--date) #'ignore)
                   ((symbol-function 'telega-symbol)
                    (lambda (name &optional image)
                      (if (memq name '(checkmark heavy-checkmark no-checkmark))
                          ""
                        (funcall original-symbol name image)))))
          (funcall orig msg))
      (funcall orig msg)))

  (defadvice! +telega-work-message-reactions-a (orig &rest args)
    :around #'telega-ins--message0
    (let ((telega-msg-temex-show-reactions
           (if +telega-work-mode
               `(and ,telega-msg-temex-show-reactions (chat saved-messages))
             telega-msg-temex-show-reactions)))
      (apply orig args)))

  (defadvice! +telega-work-reaction-list-a (orig msg)
    :around #'telega-ins--msg-reaction-list
    (unless +telega-work-mode
      (funcall orig msg)))

  (defadvice! +telega-work-inline-message-face-a (orig &rest args)
    :around '(telega-ins--msg-reply-to-message-inline
              telega-ins--msg-reply-to-story-inline
              telega-ins--msg-fwd-info-inline)
    (if +telega-work-mode
        (let ((inhibit-read-only t))
          (telega-ins--with-props '(face default)
            (apply orig args)))
      (apply orig args)))

  (defadvice! +telega-work-message-palette-face-a (orig face &rest args)
    :around #'telega-face-with-palette
    (if (and +telega-work-mode
             (memq face '(telega-msg-heading telega-msg-inline-reply
                          telega-msg-inline-forward)))
        'default
      (apply orig face args)))

  (defadvice! +telega-work-unindent-chat-a (orig &rest args)
    :around '(telega-chatbuf-msg--pp telega-chatbuf-footer-ins-pending-message)
    (if +telega-work-mode
        (let ((start (point)))
          (prog1 (apply orig args)
            (let ((end (point))
                  (inhibit-read-only t))
              (dolist (prop '(line-prefix wrap-prefix))
                ;; Trim avatar padding, including NBSPs, while keeping markers.
                (cl-loop for pos = start then next
                         while (< pos end)
                         for next = (next-single-property-change pos prop nil end)
                         for prefix = (get-text-property pos prop)
                         when (stringp prefix)
                         do (put-text-property pos next prop
                                               (string-trim-left prefix "[[:blank:]\u00a0]+")))))))
      (apply orig args)))

  (setq telega-root-fill-column 79
        telega-chat-fill-column 79)

  (add-hook! telega-chat-mode-hook
    (defun +telega-disable-visual-fill-column-h ()
      "Disable visual fill column in Telega chat buffers."
      (visual-fill-column-mode -1)))

  (telega-root-auto-fill-mode -1)
  (remove-hook 'telega-chat-mode-hook #'telega-chat-auto-fill-mode)

  (setq telega-chat-show-avatars nil
        telega-user-show-avatars nil
        telega-root-show-avatars nil
        telega-completions-username-show-avatars nil
        telega-active-locations-show-avatars nil
        telega-avatar-text-function (lambda (&rest _) "")

        telega-translate-to-language-by-default "zh"
        telega-chat-input-markups '(nil "org")
        telega-server-libs-prefix (expand-file-name "~/.local")

        ;; root page
        telega-chat-button-width '(0.2 10 25)
        telega-brackets '(((chat (return t)) "" "")
                          ((user (return t)) "" ""))
        telega-chat-button-format-plist (list :with-title 'full-name
                                              :with-username-p nil
                                              :with-title-faces-p nil
                                              :with-unread-trail-p t
                                              :with-members-trail-p nil
                                              :with-bot-verification-p nil
                                              :with-status-icons-trail-p t)

        ;; emoji
        telega-symbol-pin "%"
        telega-symbol-folder ""
        telega-symbol-photo ""
        ;; Avoid Telega's explicit `compose-chars' strings.
        telega-topic-brackets '("⟦" "⟧")
        telega-symbol-forward "→"
        telega-symbol-story-reply "↩"

        ;; filters
        telega-filters-custom nil
        telega-filter-custom-show-folders nil

        ;; images
        telega-use-images nil
        telega-emoji-use-images nil
        telega-symbols-emojify '()

        telega-date-format-alist '((today . "%H:%M") (this-week . "%m/%d") (old . "%m/%d") (date . "%y/%m/%d")
                                   (time . "%H:%M") (date-time . "%y/%m/%d. %H:%M") (date-long . "%y/%m/%d")
                                   (date-break-bar . "%m/%d"))
        telega-chat-group-messages-timespan 600
        telega-completions-capf-functions '(telega-capf-username
                                            telega-capf-hashtag
                                            telega-capf-markdown-precode
                                            telega-capf-botcmd))

  (defun +telega-capf-in-input (capf)
    "Call CAPF only while point is in Telega's writable input area."
    (when (and (markerp telega-chatbuf--input-marker)
               (>= (point) telega-chatbuf--input-marker)
               (not (get-text-property (point) 'read-only)))
      (funcall capf)))

  (defun +telega-completions-setup-capf ()
    "Set up Telega CAPFs without completing read-only message text."
    (telega-completions-setup-capf)
    (setq-local completion-at-point-functions
                (mapcar (lambda (capf)
                          (apply-partially #'+telega-capf-in-input capf))
                        telega-completions-capf-functions)))

  (when (eq system-type 'gnu/linux)
    (add-hook! telega-before-auth-hook
      (defun +telega-enable-linux-proxy-h ()
        "Add and enable the local SOCKS5 proxy before Telega authorizes."
        (telega--addProxy '(:server "127.0.0.1" :port 7891 :type (:@type "proxyTypeSocks5"))
                          :enable-p t
                          :comment "Local SOCKS5 proxy"))))

  ;; HACK: Work around upstream bot command completion returning nested lists.
  ;; Each mapped candidate list is freshly allocated, so `mapcan' is safe here.
  (defun telega-completions--bot-commands (chat)
    "Return bot command completion candidates for CHAT."
    (let* ((info (telega-chat--info chat))
           (telega-full-info-offline-p nil)
           (full-info (telega--full-info info)))
      (if (telega-chatbuf-match-p '(type bot))
          (telega-completions--bot-commands-list
           (telega--tl-get full-info :bot_info :commands))
        (mapcan (lambda (bot-commands)
                  (telega-completions--bot-commands-list
                   (plist-get bot-commands :commands)
                   (telega-msg-sender-username
                    (telega-user-get
                     (plist-get bot-commands :bot_user_id))
                    'with-@)))
                (plist-get full-info :bot_commands)))))

  ;; HACK: Show full name only in chatbuf
  (defadvice! +telega-message-header-username-only-a
    (orig msg &optional msg-chat msg-sender addon-inserter)
    :around #'telega-ins--message-header
    (let* ((msg (copy-sequence msg))
           (orig-ins (symbol-function 'telega-ins--msg-sender)))
      (setq msg (plist-put msg :author_signature nil))
      (setq msg (plist-put msg :sender_tag nil))
      (setq msg (plist-put msg :sender_boost_count 0))
      (cl-letf (((symbol-function 'telega-ins--msg-sender)
                 (lambda (sender &rest _args)
                   (funcall orig-ins sender
                            :with-title 'full-name
                            :with-username-p nil
                            :with-badges-p nil)))
                ((symbol-function 'telega-chat-admin-get)
                 (lambda (&rest _) nil)))
        (funcall orig msg msg-chat msg-sender addon-inserter))))

  ;; HACK: show stickers
  (defadvice! +telega-enable-image-for-stickers (orig-fn &rest args)
    :around '(telega-sticker--create-image
              telega-describe-stickerset
              telega-ins--sticker-list
              telega-ins--sticker-image
              telega-ins--inline-sticker
              telega-chatbuf-sticker-insert)
    (let ((telega-use-images (not +telega-work-mode)))
      (apply orig-fn args)))

  (defadvice! +telega-work-sticker-set-cover-a (orig &rest args)
    :around #'telega-stickerset-title
    (let ((telega-sticker-set-show-cover
           (and (not +telega-work-mode) telega-sticker-set-show-cover)))
      (apply orig args)))

  ;; HACK: disable sponsored msg
  (defadvice! +telega-hide-sponsored-messages-a (&rest _)
    :override #'telega-chatbuf-footer-ins-sponsored-messages
    nil)

  (add-hook! telega-ready-hook
    (defun +telega-disable-sponsored-messages-h ()
      (telega--toggleHasSponsoredMessagesEnabled nil)))

  ;; HACK: remove filter line in main filter
  (defadvice! +telega-hide-default-filter-footer-a (orig-fn &rest args)
    :around #'telega-filters--footer
    (if (and (telega-filter-default-p)
             (not telega--sort-criteria)
             (not telega--sort-inverted))
        ""
      (apply orig-fn args)))

  ;; HACK: preview stickers with vertico
  (defadvice! +telega-stickerset-preview-with-vertico-a (fn &rest args)
    :around #'telega-stickerset--minibuf-post-command
    (cl-letf (((symbol-function #'completion-all-sorted-completions)
               (lambda (&rest _)
                 (list (vertico--candidate)))))
      (apply fn args)))
  )


(use-package telega-dired-dwim
  :after telega
  :demand t)

;; [chirp] twitter client
(use-package chirp
  :straight (:host github :repo "LuciusChen/chirp")
  :commands (chirp-home
             chirp-following
             chirp-bookmarks
             chirp-likes
             chirp-me
             chirp-list
             chirp-search
             chirp-thread
             chirp-profile
             chirp-profile-followers
             chirp-profile-following-users)
  :init
  (setq chirp-cache-directory (no-littering-expand-var-file-name "chirp/")
        chirp-compose-temporary-directory
        (no-littering-expand-var-file-name "chirp/compose/"))
  (make-directory chirp-cache-directory t)
  (make-directory chirp-compose-temporary-directory t)
  :config
  (setq chirp-show-avatars nil
        chirp-show-tweet-media nil
        chirp-tweet-separator ""
        chirp-tweet-separator-indent 0))
