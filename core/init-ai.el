;;; -*- lexical-binding: t -*-

(use-package gptel
  :straight t
  :init
  (setq gptel-model 'gpt-5.3-codex-spark
        gptel-default-mode 'org-mode
        gptel-confirm-tool-calls nil
        ;; Codex models ignore temperature; keep it nil to avoid warnings.
        gptel-temperature nil)
  :config
  (setq-default gptel-backend (gptel-make-openai-oauth "OpenAI-sub"))

  (gptel-make-deepseek "DeepSeek-thinking"
    :stream t
    :request-params '(:thinking (:type "enabled"))
    :key #'gptel-api-key-from-auth-source)

  (gptel-make-deepseek "DeepSeek"
    :stream t
    :request-params '(:thinking (:type "disabled"))
    :key #'gptel-api-key-from-auth-source)
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  )

(use-package gptel-magit
  :straight (gptel-magit :type git :host github :repo "roife/gptel-magit")
  :hook ((magit-mode . gptel-magit-install))
  :config
  (setq gptel-magit-body-length 72
        gptel-magit-commit-prompt (cdr (assoc "Conventional Commits" gptel-magit-commit-styles-alist)))
  )

;; (use-package agent-shell
;;   :straight (:type git :host github :repo "xenodium/agent-shell")
;;   :commands (agent-shell +agent-shell-openai-start-codex)
;;   :bind (("C-c a a" . agent-shell)
;;          ("C-c a c" . +agent-shell-openai-start-codex))
;;   :config
;;   (setq agent-shell-show-welcome-message nil
;;       agent-shell-prefer-viewport-interaction nil
;;       agent-shell-show-busy-indicator nil
;;       agent-shell-session-strategy 'new
;;       agent-shell-openai-codex-acp-command '("codex-acp")
;;       agent-shell-show-config-icons nil
;;       agent-shell-status-kind-label-function #'agent-shell--plain-colored-status-kind-label
;;       agent-shell-markdown-table-use-unicode-borders nil
;;       agent-shell-preferred-agent-config
;;       (+agent-shell-openai-make-codex-config)
;;       agent-shell-permission-icon "!"
;;       agent-shell-thought-process-icon "")
;;
;;   (require 'agent-shell-openai)
;;   (defun +agent-shell-openai-make-codex-config ()
;;     "Create a Codex config that reuses the local Codex login."
;;     (let ((config (agent-shell-openai-make-codex-config)))
;;       (setf (alist-get :needs-authentication config) nil
;;             (alist-get :authenticate-request-maker config) nil)
;;       config))
;;   (defun +agent-shell-openai-start-codex ()
;;     "Start an interactive Codex agent shell."
;;     (interactive)
;;     (agent-shell--dwim :config (+agent-shell-openai-make-codex-config)
;;                        :new-shell t)))

(use-package codex-ide
  :straight (:type git :host github :repo "dgillis/emacs-codex-ide")
  :custom-face
  ;; smaller font
  (codex-ide-item-summary-face ((t (:inherit font-lock-function-name-face :height 0.9))))
  (codex-ide-item-detail-face ((t (:inherit shadow :height 0.8))))
  :init
  (setq codex-ide-diff-inline-fold-threshold 20
        codex-ide-image-detail "auto"
        codex-ide-prompt-placeholder-text ""
        codex-ide-placeholder-ellipsis-animation-interval nil
        codex-ide-status-mode-auto-refresh-delay 0.3
        codex-ide-buffer-name-function (lambda (dir) (format "%s: %s"
                                                        codex-ide-buffer-name-prefix
                                                        (file-name-nondirectory (directory-file-name dir)))))
  )
