;;; -*- lexical-binding: t -*-

(use-package gptel
  :straight t
  :init
  (setq gptel-model 'gpt-5.3-codex-spark
        gptel-default-mode 'org-mode
        gptel-confirm-tool-calls nil)
  :config
  (setq-default gptel-backend (gptel-make-openai-oauth "OpenAI OAuth"))
  (add-hook! gptel-post-stream-hook #'gptel-auto-scroll)
  (add-hook! gptel-post-response-functions #'gptel-end-of-response))


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
  :bind (("C-c g e" . +gptel-quick-explain)
         ("C-c g t" . +gptel-quick-translate-to-chinese)
         ("C-c g s" . +gptel-quick-summarize))
  :preface
  (defadvice! +gptel-quick-apply-options-a
    (gptel-quick-fn query-text &optional count)
    :around #'gptel-quick
    "Run GPTEL-QUICK-FN with the options stored on QUERY-TEXT."
    (let* ((options
            (and (stringp query-text)
                 (get-text-property 0 '+gptel-quick-options query-text)))
           (system-message (car-safe options))
           (gptel-quick-system-message
            (if system-message
                (lambda (&rest _) system-message)
              gptel-quick-system-message)))
      (funcall gptel-quick-fn query-text count)))

  (defadvice! +gptel-request-apply-quick-limit-a
    (gptel-request-fn prompt &rest args)
    :around #'gptel-request
    "Remove gptel-quick's token limit when PROMPT requests it."
    (let* ((options
            (and (stringp prompt)
                 (get-text-property 0 '+gptel-quick-options prompt)))
           (gptel-max-tokens
            (if (and options (not (cdr options)))
                nil
              gptel-max-tokens)))
      (apply gptel-request-fn prompt args)))

  (defadvice! +set-transient-map-keep-gptel-quick-a
    (set-transient-map-fn map keep-pred &rest args)
    :around #'set-transient-map
    "Keep gptel-quick's map active during unrelated commands."
    (when (equal (buffer-name) " *gptel-quick*")
      (setq keep-pred
            (lambda ()
              (or (null this-command)
                  (not (where-is-internal this-command (list map) t))))))
    (apply set-transient-map-fn map keep-pred args))

  (defadvice! +posframe-show-focus-gptel-quick-a
    (posframe-show-fn buffer-or-name &rest args)
    :around #'posframe-show
    "Show the gptel-quick posframe with input focus."
    (let ((gptel-quick-p (equal buffer-or-name " *gptel-quick*")))
      (when gptel-quick-p
        (setq args (plist-put args :accept-focus t)))
      (let ((frame (apply posframe-show-fn buffer-or-name args)))
        (when (and gptel-quick-p (frame-live-p frame))
          (set-window-point (frame-selected-window frame) 1)
          (select-frame-set-input-focus frame))
        frame)))

  (defun +gptel-quick-region-or-buffer (system-message &optional limit-response)
    "Run `gptel-quick' on the active region or buffer using SYSTEM-MESSAGE.
Preserve SYSTEM-MESSAGE when requesting another response with `+'.  When
LIMIT-RESPONSE is non-nil, apply gptel-quick's count-derived token limit."
    (require 'gptel-quick)
    (let ((query-text
           (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (buffer-substring-no-properties (point-min) (point-max)))))
      (when (string-empty-p query-text)
        (user-error "Buffer is empty"))
      (setq query-text
            (propertize query-text '+gptel-quick-options
                        (cons system-message limit-response)))
      (gptel-quick query-text)))

  (defun +gptel-quick-explain ()
    "Explain the active region, or the whole buffer, in Chinese."
    (interactive)
    (+gptel-quick-region-or-buffer
     "Explain in clear Chinese, preserving necessary context and details."
     t))

  (defun +gptel-quick-translate-to-chinese ()
    "Translate the active region, or the whole buffer, to Chinese."
    (interactive)
    (+gptel-quick-region-or-buffer "Translate into fluent Chinese."))

  (defun +gptel-quick-summarize ()
    "Summarize the active region, or the whole buffer, in Chinese."
    (interactive)
    (+gptel-quick-region-or-buffer
     "Summarize in Chinese while preserving details and key information."
     t))

  (with-eval-after-load 'embark
    (keymap-set embark-general-map "?" #'gptel-quick)
    (keymap-set embark-region-map "E" #'+gptel-quick-explain)
    (keymap-set embark-region-map "T" #'+gptel-quick-translate-to-chinese)
    (keymap-set embark-region-map "S" #'+gptel-quick-summarize))

  :config
  (setq gptel-quick-word-count 50
        gptel-quick-timeout nil))

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


;; [gptel-copilot] gptel-powered inline code completion
(use-package gptel-copilot
  :straight (:type git :host github :repo "roife/gptel-copilot")
  :commands gptel-copilot-mode
  :preface
  (defun +gptel-copilot-complete ()
    "Accept the completion, or move to the end of code or line."
    (interactive)
    (or (gptel-copilot-accept-completion)
        (mwim-end-of-code-or-line)))

  (defun +gptel-copilot-complete-word ()
    "Accept one completion word, or move forward one word."
    (interactive)
    (or (gptel-copilot-accept-completion-by-word 1)
        (forward-word)))

  :hook (prog-mode . gptel-copilot-mode)
  :bind (:map gptel-copilot-mode-map
              ("C-e" . +gptel-copilot-complete)
              ("M-f" . +gptel-copilot-complete-word))
  :config
  (require 'gptel-openai-oauth)

  (setq gptel-copilot-model 'gpt-5.4-mini
        gptel-copilot-backend
        (gptel-make-openai-oauth "OpenAI OAuth Inline"
          :request-params '(:reasoning (:effort "low")))))
