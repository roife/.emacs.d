;;; -*- lexical-binding: t -*-

(use-package gptel
  :straight t
  :init
  (setq gptel-model "gpt-5.3-codex-spark"
        gptel-default-mode 'org-mode
        gptel-confirm-tool-calls nil)

  (setq-default gptel-backend (gptel-make-openai-oauth "OpenAI-sub"))

  (gptel-make-deepseek "DeepSeek-thinking"
    :stream t
    :request-params '(:thinking (:type "enabled"))
    :key #'gptel-api-key-from-auth-source)

  (gptel-make-deepseek "DeepSeek"
    :stream t
    :request-params '(:thinking (:type "disabled"))
    :key #'gptel-api-key-from-auth-source)

  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (gptel-agent-update)
  )

(use-package gptel-agent
  :straight t)

(use-package gptel-magit
  :straight (gptel-magit :type git :host github :repo "roife/gptel-magit")
  :hook ((magit-mode . gptel-magit-install))
  :config
  (setq gptel-magit-body-length 72
        gptel-magit-commit-prompt (cdr (assoc "Conventional Commits" gptel-magit-commit-styles-alist))
        gptel-magit-include-reasoning 'ignore)
  )

(use-package codex-ide
  :straight (:type git :host github :repo "dgillis/emacs-codex-ide")
  :bind (("C-c C-;" . codex-ide-menu))
  :config
  (setq codex-ide-diff-inline-fold-threshold 20
        codex-ide-renderer-render-markdown-during-streaming nil
        codex-ide-prompt-placeholder-text ""
        codex-ide-placeholder-ellipsis-animation-interval nil
        codex-ide-status-placeholder-text-alist '(("approval" . "Need approval...")
                                                  ("interrupting" . "Interrupting..."))
        codex-ide-status-mode-auto-refresh-delay 0.3)
  )
