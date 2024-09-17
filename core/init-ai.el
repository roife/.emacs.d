;;; -*- lexical-binding: t -*-

(use-package gptel
  :straight t
  :init
  (require 'auth-source)
  (setq gptel-model "gpt-4o")

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

  (let* ((auth-info (car (auth-source-search :user "roife-openai")))
         (host (plist-get auth-info :host))
         (key (plist-get auth-info :secret)))
    (gptel-make-openai "Claude"
      :protocol "https"
      :host host
      :stream t
      :key key
      :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
      :models '("claude-3-5-sonnet-20240620")))

  :config
  (setq gptel-directives '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely. DO NOT BE LAZY.")
                           (translation . "You are an AI designed to translate technical content from English to Chinese. The content is related to the Programming Languages Theory and Rust programming language, including official documentation and technical discussions. Your translations should be accurate, preserving technical terminology and context.

## Instructions
1. **Language Precision:** Ensure that all technical terms, code snippets, PLT-specific jargon, and Rust-specific jargon are translated accurately. Where possible, use the official translations or widely accepted terms within the Chinese Rust community.
2. **Clarity:** The translation should be clear and easy to understand for a Chinese-speaking audience with a technical background in PL or Rust. Avoid overly literal translations that may confuse the reader.
3. **Cultural Adaptation:** Adapt phrases and idioms as necessary to ensure that they make sense in a Chinese cultural context.
4. **Consistency:** Maintain consistent terminology throughout the translation, particularly with PL-specific terms or Rust-specific terms. Ensure that your translation style is consistent across multiple sections or posts.
5. **Native:** Refine sentence structure and expressions. You can rephrase the sentence (especially sentence structure and word choice) without changing the meaning to make it more aligned with Chinese linguistic habits. Do not be lazy!

## Attention
- IMPORTANT: If a term has no direct Chinese equivalent or is best understood in its original English form, provide the term in English along with an explanation in Chinese.
- Provide translations in a tone that matches the original English content, whether it's formal documentation or informal forum discussions.
- DO NOT explain or translate my original text")
                           (rust-programming . "You are given the following Rust code. Your task is to optimize and improve the code **without changing its existing logic**. **Do not include any explanations, comments, or line numbers in your response.** You should:

## Instructions
- Carefully analyze the original code, paying close attention to its structure and the cursor position. DO NOT BE LAZY.
- **Simplify Control Flow:** If possible, simplify the control structures to make the code cleaner and more readable. Avoid nested conditions if they can be flattened or refactored for clarity.
- **Use Third-Party Libraries Wisely:** Where appropriate, refactor loops to leverage libraries like `itertools` for better readability and performance. Ensure the use of external crates is meaningful, contributing to code quality without overcomplicating it.
- **Improve Efficiency:**
   - Identify any areas where the performance can be improved, for example, by reducing unnecessary allocations, avoiding cloning where not needed, or reusing computations.
   - Avoid premature optimization but remove any clear inefficiencies.
- **Reduce Redundant Code**: Eliminate any redundant code without changing its functionality. Look for repeated patterns that can be abstracted or simplified.
- **Code Block**: Wrap your response in a rust code block.

## Final Check
- Confirm that no unrelated code is accidentally modified or deleted.
- Double-check that the existing logic is not changed. DO NOT BE LAZY.

Ensure that after all these changes, the code maintains the same functionality as before and that no unrelated changes are introduced.")))

  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (with-eval-after-load 'corfu
    (when (fboundp 'corfu-quit)
      (add-hook 'gptel-pre-response-hook 'corfu-quit)))
  )
