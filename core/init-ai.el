;;; -*- lexical-binding: t -*-

(use-package gptel
  :straight t
  :init
  (setq gptel-model "gpt-5.3-codex-spark"
        gptel-default-mode 'org-mode
        gptel-confirm-tool-calls nil
        ;; Codex models ignore temperature; keep it nil to avoid warnings.
        gptel-temperature nil)

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
        gptel-magit-commit-prompt (cdr (assoc "Conventional Commits" gptel-magit-commit-styles-alist)))
  )

(use-package codex-ide
  :straight (:type git :host github :repo "dgillis/emacs-codex-ide")
  :custom-face
  ;; smaller font
  (codex-ide-item-summary-face ((t (:inherit font-lock-function-name-face :height 1.0))))
  (codex-ide-item-detail-face ((t (:inherit shadow :height 0.9))))
  :bind (:map codex-ide-session-mode-map
              ("C-c C-;" . codex-ide-menu)
              ("C-<return>" . codex-ide-submit))
  :init
  (setq codex-ide-diff-inline-fold-threshold 20
        codex-ide-prompt-placeholder-text "Tell Codex what to do..."
        codex-ide-placeholder-ellipsis-animation-interval nil
        codex-ide-status-placeholder-text-alist '(("approval" . "Need approval...")
                                                  ("interrupting" . "Interrupting..."))
        codex-ide-status-mode-auto-refresh-delay 0.3
        codex-ide-session-baseline-prompt "- You are running inside Emacs and can use MCP tools to interact with Emacs.
- Use markdown pipe tables. In table cells, wrap code-like idents, paths, symbols, and exprs in backticks.
- Use markdown links for code references, e.g. [`foo.el`](/tmp/foo.el#L3C2).
- Do not needlessly use Emacs commands."
        codex-ide-buffer-name-function (lambda (dir) (format "%s: %s"
                                                        codex-ide-buffer-name-prefix
                                                        (codex-ide--project-name dir)))
        codex-ide--prompt-context-open-tag "<Emacs-context>"
        codex-ide--prompt-context-close-tag "</Emacs-context>"
        )
  )
