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
        gptel-magit-commit-prompt (cdr (assoc "Conventional Commits" gptel-magit-commit-styles-alist)))

  (defun +gptel-magit-fish (fifo insertp)
    "Generate a message, optionally insert it, and report through FIFO."
    (let ((commit-buffer (magit-commit-message-buffer))
          record fsm)
      (cond
       ((and insertp (not commit-buffer))
        (user-error "No commit in progress"))
       ((and (not insertp) commit-buffer)
        (user-error "Commit already in progress")))
      (setq fsm
            (gptel-magit--generate
             (lambda (message)
               (setq record (concat "0" message)))
             nil commit-buffer "Generating commit message..."))
      (let ((info (gptel-fsm-info fsm)))
        (plist-put
         info :post
         (append
          (plist-get info :post)
          (list
           (lambda (request-info)
             (with-temp-buffer
               (insert
                (or record
                    (if (eq (gptel-fsm-state fsm) 'DONE)
                        "0"
                      (concat "1" (format "%s"
                                          (or (plist-get request-info :status)
                                              (plist-get request-info :error)
                                              "generation failed"))))))
               (write-region (point-min) (point-max)
                             fifo nil 'silent)))))))
      fsm)))

(use-package gptel-quick
  :straight (gptel-quick :type git :host github :repo "roife/gptel-quick")
  :bind (("C-c g e" . +gptel-quick-explain)
         ("C-c g t" . +gptel-quick-translate-to-chinese)
         ("C-c g s" . +gptel-quick-summarize)
         ("C-c g d" . +gptel-quick-dict))
  :preface
  (defun +gptel-quick-region-or-buffer (system-message &optional limit-response thing)
    "Run `gptel-quick' on THING, the active region, or the buffer.
Preserve SYSTEM-MESSAGE when requesting another response with `+'.  When
LIMIT-RESPONSE is non-nil, apply gptel-quick's count-derived token limit."
    (require 'gptel-quick)
    (let ((query-text
           (if thing
               (or (thing-at-point thing t)
                   (user-error "No %s at point" thing))
             (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (buffer-substring-no-properties (point-min) (point-max))))))
      (when (string-empty-p query-text)
        (user-error "Buffer is empty"))
      (gptel-quick query-text nil
                   (append (list :system system-message)
                           (unless limit-response
                             (list :max-tokens nil))))))

  (defmacro +gptel-quick-define-command (name doc prompt &optional limit thing)
    "Define NAME as a gptel-quick action over the region or buffer."
    `(defun ,name ()
       ,doc (interactive)
       (+gptel-quick-region-or-buffer ,prompt ,limit ,thing)))

  (+gptel-quick-define-command +gptel-quick-explain
                               "Explain the active region, or the whole buffer, in Chinese."
                               "Explain in clear Chinese, preserving necessary context and details." t)

  (+gptel-quick-define-command +gptel-quick-translate-to-chinese
                               "Translate the active region, or the whole buffer, to Chinese."
                               "Translate into fluent Chinese.")

  (+gptel-quick-define-command +gptel-quick-summarize
                               "Summarize the active region, or the whole buffer, in Chinese."
                               "Summarize in Chinese while preserving details and key information." t)

  (+gptel-quick-define-command +gptel-quick-dict
                               "Explain the word at point in dictionary style."
                               "Given a word, explain it in the style of a concise English dictionary entry,
and add accurate Chinese translations for each sense. Preserve the compact dictionary
format in plain text rather than giving a long explanatory article or markdown document.
No need for chinese in sentences.

Use this format:
*word* syllable division | pronunciation | part of speech (inflections)

1. English definition **中文**
     *Example sentence.*
 | Sub-sense or extended meaning **中文**
     *Example sentence.*
• ...
2. English definition **中文**
     *Example sentence.*"
                               nil 'word)

  (with-eval-after-load 'embark
    (keymap-set embark-general-map "?" #'gptel-quick)
    (keymap-set embark-region-map "E" #'+gptel-quick-explain)
    (keymap-set embark-region-map "T" #'+gptel-quick-translate-to-chinese)
    (keymap-set embark-region-map "S" #'+gptel-quick-summarize)
    (keymap-set embark-region-map "D" #'+gptel-quick-dict))

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
  :disabled t
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
