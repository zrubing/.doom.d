;; -*- lexical-binding: t; -*-

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config

  ;; (let ((api-key (funcall (plist-get (car (auth-source-search :host "google.com")) :secret))))
  ;;   (setenv "GEMINI_API_KEY" (encode-coding-string api-key 'utf-8)))


  (let ((api-key (funcall (plist-get (car (auth-source-search :host "api.deepseek.com")) :secret))))
    (setenv "DEEPSEEK_API_KEY" (encode-coding-string api-key 'utf-8)))
  ;; See the Configuration section below
  ;; (aidermacs-default-model "gemini-2.5-pro"))
  (setq aidermacs-use-architect-mode t)
  (setq aidermacs-default-model "deepseek/deepseek-chat")


  ;; (let ((api-key (funcall (plist-get (car (auth-source-search :host "openrouter.ai")) :secret))))
  ;;   (setenv "OPENROUTER_API_KEY" (encode-coding-string api-key 'utf-8)))
  ;; ;; See the Configuration section below
  ;; ;; (aidermacs-default-model "gemini-2.5-pro"))
  ;; (setq aidermacs-use-architect-mode t)
  ;; (setq aidermacs-default-model "openrouter/meta-llama/llama-4-scout")


  )

(provide 'aidermacs)
