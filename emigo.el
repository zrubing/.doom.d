;; -*- lexical-binding: t; -*-

(use-package! emigo
  :config

  (let ((api-key (funcall (plist-get (car (auth-source-search :host "api.deepseek.com")) :secret))))
    (setenv "DEEPSEEK_API_KEY" (encode-coding-string api-key 'utf-8)))
  (setq emigo-model "deepseek/deepseek-chat")
  (setq emigo-base-url "")
  (setq emigo-api-key (getenv "DEEPSEEK_API_KEY"))

  (emigo-enable) ;; Starts the background process automatically
  )



(provide 'emigo)
