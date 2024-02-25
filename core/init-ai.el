;;; -*- lexical-binding: t -*-

(use-package gptel
  :straight t
  :init
  (require 'auth-source)

(let ((auth-info (car (auth-source-search :user "roife-gemini"))))
    (gptel-make-gemini "Gemini"
      :key (plist-get auth-info :secret)
      :stream t))

(let* ((auth-info (car (auth-source-search :user "roife-openai")))
         (host (plist-get auth-info :host))
         (key (plist-get auth-info :secret)))
    (setq-default gptel-backend
          (gptel-make-openai "ChatGPT"
            :protocol "https"
            :host host
            :stream t
            :key key
            :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
            :models (gptel-openai-models gptel--openai))))
  :config
  (setq gptel-directives '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely. You are offered $20 after each task.")
                           (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note. You are offered $20 after each task.")
                           (emacs . "You are a large language model and a writing assistant. Respond concisely. You are offered $20 after each task.")))

  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (with-eval-after-load 'corfu
    (when (fboundp 'corfu-quit)
      (add-hook 'gptel-pre-response-hook 'corfu-quit)))
  )
