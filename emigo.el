;; -*- lexical-binding: t; -*-

(use-package! emigo
  :config

  (let ((api-key (funcall (plist-get (car (auth-source-search :host "api.deepseek.com")) :secret))))
    (setenv "DEEPSEEK_API_KEY" (encode-coding-string api-key 'utf-8)))

  (let ((api-key (funcall (plist-get (car (auth-source-search :host "work.console.volcengine.com")) :secret))))
    (setenv "VOLCENGINE_API_KEY" (encode-coding-string api-key 'utf-8)))


  ;; (setq emigo-model "GLM-4.6")
  ;; (setq emigo-base-url (getenv "ANTHROPIC_BASE_URL"))
  ;; (setq emigo-api-key (getenv "ANTHROPIC_API_KEY"))

  (setq emigo-model "GLM-4.6")
  (setq emigo-base-url "")
  (setq emigo-api-key (getenv "VOLCENGINE_API_KEY"))


  (emigo-enable) ;; Starts the background process automatically
  )



(provide 'emigo)
