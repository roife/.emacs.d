;;; -*- lexical-binding: t -*-

(use-package gptel
  :straight t
  :init
  (setq gptel-model 'deepseek-v4-flash
        gptel-default-mode 'org-mode
        gptel-confirm-tool-calls nil)
  :config
  (setq-default gptel-backend
                (gptel-make-deepseek "DeepSeek-thinking"
                  :stream t
                  :request-params '(:thinking (:type "enabled"))
                  :key #'gptel-api-key-from-auth-source))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  )

(use-package gptel-rewrite
  :straight nil
  :bind (("C-c r t" . +gptel-rewrite-translate-to-chinese)
         ("C-c r s" . +gptel-rewrite-summarize)
         :map gptel-rewrite-actions-map
         ("C-c C-x" . +gptel-rewrite-export))
  :preface
  (defun +gptel-rewrite-export (&optional overlays)
    "Export OVERLAYS to a new buffer without changing their source.
When OVERLAYS is nil, export all pending rewrites in the current buffer."
    (interactive)
    (require 'gptel-rewrite)
    (setq overlays (or overlays gptel--rewrite-overlays))
    (unless overlays
      (user-error "No pending rewrites to export"))
    (let* ((source-buffer (current-buffer))
           (source-name (buffer-name source-buffer))
           (prepared-buffer
            (gptel--rewrite-prepare-buffer overlays))
           (prepared-point
            (with-current-buffer prepared-buffer
              (- (point) (point-min))))
           (contents
            (with-current-buffer prepared-buffer
              (buffer-substring-no-properties (point-min) (point-max))))
           (export-buffer
            (generate-new-buffer
             (format "*gptel rewrite export: %s*" source-name))))
      (with-current-buffer export-buffer
        (markdown-ts-mode)
        (insert contents)
        (goto-char (+ (point-min)
                      (min prepared-point (- (point-max) (point-min)))))
        (setq-local header-line-format
                    (format " Exported rewrite from %s" source-name))
        (visual-line-mode 1)
        (set-buffer-modified-p nil))
      (pop-to-buffer export-buffer)))

  (defun +gptel-rewrite-region-or-buffer (prompt)
    "Rewrite the active region, or the whole buffer, according to PROMPT."
    (require 'gptel-rewrite)
    (if (use-region-p)
        (gptel--suffix-rewrite prompt)
      (when (= (point-min) (point-max))
        (user-error "Buffer is empty"))
      (save-mark-and-excursion
        (set-mark (point-max))
        (goto-char (point-min))
        (activate-mark)
        (gptel--suffix-rewrite prompt))))

  (defun +gptel-rewrite-translate-to-chinese ()
    "Translate the active region, or the whole buffer, to Chinese."
    (interactive)
    (+gptel-rewrite-region-or-buffer
     "Translate into fluent Chinese."))

  (defun +gptel-rewrite-summarize ()
    "Summarize the active region, or the whole buffer when no region is active."
    (interactive)
    (+gptel-rewrite-region-or-buffer
     "Summarize in Chinese while preserving details and key information."))

  (with-eval-after-load 'embark
    (keymap-set embark-region-map "T" #'+gptel-rewrite-translate-to-chinese)
    (keymap-set embark-region-map "S" #'+gptel-rewrite-summarize)))

(use-package gptel-agent
  :straight t
  :after gptel
  :config (gptel-agent-update))


(use-package gptel-magit
  :straight (gptel-magit :type git :host github :repo "roife/gptel-magit")
  :hook ((magit-mode . gptel-magit-install))
  :config
  (setq gptel-magit-body-length 72
        gptel-magit-commit-prompt (cdr (assoc "Conventional Commits" gptel-magit-commit-styles-alist))))

(use-package gptel-quick
  :straight (gptel-quick :type git :host github :repo "karthink/gptel-quick")
  :after (gptel embark)
  :config
  (setq gptel-quick-backend (gptel-make-deepseek "DeepSeek-quick"
                              :stream t
                              :request-params '(:thinking (:type "disabled"))
                              :key #'gptel-api-key-from-auth-source)
        gptel-quick-model 'deepseek-v4-flash
        gptel-quick-word-count 500
        gptel-quick-system-message (lambda (&rest _) "一句话解释："))
  (keymap-set embark-general-map "?" #'gptel-quick)
  )

(use-package codex-ide
  :straight (:type git :host github :repo "dgillis/emacs-codex-ide")
  :custom-face
  (codex-ide-item-summary-face ((t (:inherit font-lock-function-name-face :height 0.9))))
  (codex-ide-item-detail-face ((t (:inherit shadow :height 0.8))))
  :init
  (setq codex-ide-diff-inline-fold-threshold 20
        codex-ide-image-detail "auto"
        codex-ide-prompt-placeholder-text ""
        codex-ide-placeholder-ellipsis-animation-interval nil
        codex-ide-status-mode-auto-refresh-delay 0.3
        codex-ide-want-mcp-bridge nil
        codex-ide-emacs-context-policy nil
        codex-ide-session-transcript-default-detail-level 'compact
        codex-ide-buffer-name-function (lambda (dir)
                                         (format "%s: %s"
                                                 codex-ide-buffer-name-prefix
                                                 (file-name-nondirectory (directory-file-name dir)))))
  )

(use-package codex-ide-session
  :straight nil
  :preface
  (defun +codex-ide-submit-or-newline ()
    "Submit one-line Codex prompts, otherwise insert a newline."
    (interactive)
    (let* ((session (codex-ide--get-default-session-for-current-buffer))
           (start (and session
                       (codex-ide-session-input-start-marker session)))
           (end (and session
                     (codex-ide--input-end-position session))))
      (if (and (markerp start)
               end
               (not (save-excursion
                      (goto-char (marker-position start))
                      (search-forward "\n" end t))))
          (codex-ide-submit)
        (newline))))
  :bind (:map codex-ide-session-prompt-minor-mode-map
              ("RET" . +codex-ide-submit-or-newline)
              ("<return>" . +codex-ide-submit-or-newline)
              ("S-<return>" . newline)
              :map codex-ide-session-mode-map
              ("C-c C-;" . codex-ide-agent-config-menu)
              ("C-c C-r" . codex-ide-status))
  :config
  (require 'codex-ide))
