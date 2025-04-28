;; -*- lexical-binding: t; -*-
(use-package aider
  :config
  (setq aider-prompt-file-name ".aider.prompt.org")

  ;; (setq aider-args '("--model" "openai/gpt-4o-mini" "--no-auto-commits"))

  ;; (setenv "OPENAI_API_BASE" "https://chatapi.onechats.top/v1/")
  ;; (let ((api-key (funcall (plist-get (car (auth-source-search :host "chatapi.onechats.top")) :secret))))
  ;;   (setenv "OPENAI_API_KEY" (encode-coding-string api-key 'utf-8)))


  ;; (setq aider-args '("--model" "deepseek/deepseek-chat" "--no-auto-commits"))
  ;; (let ((api-key (funcall (plist-get (car (auth-source-search :host "api.deepseek.com")) :secret))))
  ;;   (setenv "DEEPSEEK_API_KEY" (encode-coding-string api-key 'utf-8)))


  (setq aider-args '("--model" "volcengine/deepseek-v3-250324" "--no-auto-commits"))
  (let ((api-key (funcall (plist-get (car (auth-source-search :host "work.console.volcengine.com")) :secret))))
    (setenv "VOLCENGINE_API_KEY" (encode-coding-string api-key 'utf-8)))



  ;; (setq aider-args '("--model" "openrouter/google/gemini-2.5-pro-preview-03-25" "--no-auto-commits"))
  (let ((api-key (funcall (plist-get (car (auth-source-search :host "openrouter.ai")) :secret))))
    (setenv "OPENROUTER_API_KEY" (encode-coding-string api-key 'utf-8)))

  ;; (setq aider-args '("--model" "gemini-2.5-pro" "--no-auto-commits"))
  ;; (let ((api-key (funcall (plist-get (car (auth-source-search :host "google.com")) :secret))))
  ;;   (setenv "GEMINI_API_KEY" (encode-coding-string api-key 'utf-8)))




  (global-set-key (kbd "C-c a") 'aider-transient-menu))
(provide 'aider)
