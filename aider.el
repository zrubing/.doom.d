;; -*- lexical-binding: t; -*-
(use-package aider
  :config
  (setq aider-prompt-file-name ".aider.prompt.org")

  (setq aider-args '("--model" "openai/gpt-4o-mini" "--no-auto-commits"))
  (setenv "OPENAI_API_BASE" "https://chatapi.onechats.top/v1/")
  (let ((api-key (funcall (plist-get (car (auth-source-search :host "chatapi.onechats.top")) :secret))))
    (setenv "OPENAI_API_KEY" (encode-coding-string api-key 'utf-8)))

  (global-set-key (kbd "C-c a") 'aider-transient-menu))
(provide 'aider)
