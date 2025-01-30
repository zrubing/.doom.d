;;; -*- lexical-binding: t; -*-

(use-package! gptel
  :defer t
  :config
  ;; OPTIONAL configuration
  (setq! auth-source-debug t)
  (setq! gptel-default-mode 'org-mode)
  (setq! gptel-api-key (lambda()
                         (gptel-api-key-from-auth-source "chatapi.onechats.top") ))



  (defvar gptel--openai-proxy
    (gptel-make-openai
        "ChatGptProxy"
      :key 'gptel-api-key
      :host "chatapi.onechats.top"
      :stream t
      :models '(gpt-4o-mini o1-mini gpt-3.5-turbo gpt-4o))
    )

  (defvar gptel--deepseek-proxy
    (gptel-make-openai "DeepseekProxy"
      :key (lambda() (gptel-api-key-from-auth-source "api.deepseek.com"))
      :host "api.deepseek.com"
      :stream t
      :models '("deepseek-chat" "deepseek-coder"))
    )

  (defvar gptel--openai-proxy-claude
    (gptel-make-openai
        "ClaudeProxy"
      :key 'gptel-api-key
      :host "chatapi.onechats.top"
      :stream t
      :models '(gpt4o-mini claude-3-haiku-20240307 claude-3-5-sonnet-20240620))
    )

  (defvar gptel--openai-proxy-github
    (gptel-make-openai
        "GithubModels"
      :key (lambda() (gptel-api-key-from-auth-source "models.inference.ai.azure.com"))
      :host "models.inference.ai.azure.com"
      :endpoint "/chat/completions"
      :stream t
      :models '(gpt-4o-mini gpt-4o DeepSeek-R1))
    )


  (defvar gptel--ollama
    (gptel-make-ollama "Ollama"
      :host "localhost:11434"
      :stream t
      :models '("qwen2:1.5b" "deepseek-coder-v2:latest" "scomper/minicpm-v2.5:latest" "gemma2:9b" "phi3:medium"))
    )

  (setq! gptel-model 'gpt-4o-mini
         gptel-backend gptel--openai-proxy-github)
  )



(provide 'gptel.el)
