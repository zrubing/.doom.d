;;; -*- lexical-binding: t; -*-

(use-package! gptel
  :defer t
  :config
  ;; OPTIONAL configuration
  (setq! auth-source-debug t)
  (setq! gptel-default-mode 'org-mode)
  (setq! gptel-api-key (lambda()
                         (gptel-api-key-from-auth-source "chatapi.onechats.top") ))


  (gptel-make-openai "OpenAI Proxy" ;Any name you want
    :host "chatapi.onechats.top"
    :endpoint "/chat/completions"
    :stream t
    :key 'gptel-api-key
    :models '(gpt-4.1-mini gpt-4.1-nano gpt-4.1))


  (gptel-make-openai "Claude Proxy" ;Any name you want
    :host "chatapi.onechats.top"
    :endpoint "/chat/completions"
    :stream t
    :key 'gptel-api-key
    :models '(claude-3-haiku-20240307 claude-3-5-sonnet-20240620))



  ;; Github Models offers an OpenAI compatible API
  (gptel-make-openai "Github Models" ;Any name you want
    :host "models.inference.ai.azure.com"
    :endpoint "/chat/completions?api-version=2024-05-01-preview"
    :stream t
    :key (lambda() (gptel-api-key-from-auth-source "models.inference.ai.azure.com"))
    :models '(DeepSeek-R1 gpt-4o-mini gpt-4o gpt-4o))

  ;; OpenRouter offers an OpenAI compatible API
  (gptel-make-openai "OpenRouter"               ;Any name you want
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key (lambda() (gptel-api-key-from-auth-source "openrouter.ai"))                   ;can be a function that returns the key
    :models '(microsoft/mai-ds-r1:free
              qwen/qwen3-8b:free
              google/gemini-2.5-pro-preview-03-25
              deepseek/deepseek-chat-v3-0324:free
              ))

  (setq! gptel-model 'deepseek-reasoner
         gptel-backend
         (gptel-make-deepseek "DeepSeek"       ;Any name you want
           :stream t                           ;for streaming responses
           :models '("deepseek-reasoner" "deepseek-chat" "deepseek-coder")
           :key (lambda() (gptel-api-key-from-auth-source "api.deepseek.com")))               ;can be a function that returns the key
         )
  )



(provide 'gptel.el)
