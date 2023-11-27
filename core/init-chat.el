;; -*- lexical-binding: t; -*-
;; (use-package language-detection
;;   :straight t)

(use-package telega
  :straight t
  :custom-face
  (telega-msg-heading ((t (:inherit hl-line :background unspecified))))
  (telega-msg-inline-reply ((t (:inherit (hl-line font-lock-function-name-face)))))
  (telega-msg-inline-forward ((t (:inherit (hl-line font-lock-type-face)))))
  (telega-msg-user-title ((t (:bold t))))
  :bind (:map telega-chat-button-map
              ("h" . nil))
  :init
  (setq telega-chat-folder-format nil)
  :config
  (setq telega-chat-show-avatars nil
        telega-user-show-avatars nil
        telega-root-show-avatars nil
        telega-chat-fill-column 68
        telega-idle-delay 5
        telega-translate-to-language-by-default "zh"
        telega-chat-input-markups '(nil "org")
        telega-chat-prompt-format "▶ "
        telega-completing-read-function completing-read-function
        ;; avoid emoji display problem
        telega-emoji-use-images nil
        ;; add double lines before reply box
        telega-symbol-underline-bar (propertize " " 'face 'telega-webpage-strike-through)
        ;; root page
        ;; telega-root-default-view-function 'telega-view-folders
        telega-symbol-folder ""
        telega-root-fill-column 70
        telega-root-auto-fill-mode nil
        ;; filters
        telega-filters-custom nil
        telega-filter-custom-show-folders nil
        ;; images
        telega-sticker--use-thumbnail t
        telega-use-images nil
        telega-emoji-use-images nil

        telega-symbols-emojify nil)

  (if (eq system-type 'darwin)
      (setq telega-proxies '((:server "127.0.0.1" :port 7890 :enable t :type (:@type "proxyTypeSocks5"))))
    (setq telega-proxies '((:server "127.0.0.1" :port 7891 :enable t :type (:@type "proxyTypeSocks5")))))

  ;; completion
  (setq telega-emoji-company-backend #'telega-company-emoji)

  (add-hook! telega-chat-mode-hook
    (defun +telega-completion-setup ()
      (make-variable-buffer-local 'completion-at-point-functions)
      (setq completion-at-point-functions
            (append (mapcar #'cape-company-to-capf
                            telega-company-backends)
                    completion-at-point-functions))
      (require 'company)
      (corfu-mode 1))
    )

  ;; better hl-line settings in telega
  (add-hook! (telega-root-mode-hook telega-chat-mode-hook)
    (defun +telega-disable-special-hl-line-fn ()
      (setq-local hl-line-range-function nil)))

  ;; disable some images
  (advice-add #'telega-ins--user-emoji-status :override #'ignore)

  (defadvice! +telega-enable-image-for-stickers (orig-fn &rest args)
    :around '(telega-sticker--create-image
              ;; telega-describe-stickerset
              telega-ins--sticker-list
              telega-ins--sticker-image
              telega-ins--inline-sticker
              telega-chatbuf-sticker-insert)
    (let ((telega-use-images t))
      (apply orig-fn args)))

  ;; turn on visual-fill-column-mode
  (add-hook! telega-chat-mode-hook
    (defun +telega-enable-visual-fill-column-mode ()
      (visual-line-mode 1)
      (visual-fill-column-mode 1)
      (setq-local visual-fill-column-extra-text-width '(0 . 3))))
  )


;; (use-package telega-mnz
;;   :straight nil
;;   :hook ((telega-load . global-telega-mnz-mode))
;;   :config
;;   (setq telega-mnz-use-language-detection t)
;;   )


(use-package telega-dired-dwim
  :straight nil
  )


(use-package telega-url-shorten
  :straight nil
  :hook ((telega-load . global-telega-url-shorten-mode))
  :init
  ;; HACK: we don't use all-the-icons but telega-url-shorten needs it
  ;; so just make it happy
  (provide 'all-the-icons)
  (setq telega-url-shorten-use-images nil)
  (setq telega-url-shorten-regexps
        (list `(too-long-link
                :regexp "^\\(https?://\\)\\(.\\{50\\}\\).*?$"
                :symbol ""
                :replace ,(concat "\\1\\2" truncate-string-ellipsis))))
  )
