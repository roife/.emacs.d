;; [pdf-view] View PDF
(use-package pdf-tools
  :straight t
  :defines pdf-annot-activate-created-annotations
  :hook ((pdf-tools-enabled . pdf-view-auto-slice-minor-mode)
         (pdf-tools-enabled . pdf-isearch-minor-mode)
         (pdf-tools-enabled . pdf-occur-global-minor-mode)
         (pdf-tools-enabled . pdf-outline-minor-mode))
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
         ([remap scroll-up-command] . pdf-view-scroll-up-or-next-page)
         ([remap scroll-down-command] . pdf-view-scroll-down-or-previous-page))
  :init
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil
        pdf-annot-activate-created-annotations t)
  :config
  (pdf-tools-install t nil t nil)
  )

;; (use-package pdf-annot
;;   :after pdf-tools
;;   :init
;;   (add-hook! kill-buffer-hook :local
;;     (defun +pdf-cleanup-annot-windows ()
;;       "Kill left-over annotation buffers when the document is killed."
;;       (when (buffer-live-p pdf-annot-list-document-buffer)
;;         (pdf-info-close pdf-annot-list-document-buffer))
;;       (when (buffer-live-p pdf-annot-list-buffer)
;;         (kill-buffer pdf-annot-list-buffer))
;;       (let ((contents-buffer (get-buffer "*Contents*")))
;;         (when (and contents-buffer (buffer-live-p contents-buffer))
;;           (kill-buffer contents-buffer)))))
;;   )

;; [saveplace-pdf-view] Recover last viewed position
(use-package saveplace-pdf-view
  :straight t
  :when (ignore-errors (pdf-info-check-epdfinfo) t)
  :autoload (saveplace-pdf-view-find-file-advice saveplace-pdf-view-to-alist-advice)
  :init
  (advice-add 'save-place-find-file-hook :around #'saveplace-pdf-view-find-file-advice)
  (advice-add 'save-place-to-alist :around #'saveplace-pdf-view-to-alist-advice))
