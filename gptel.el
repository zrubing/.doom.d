;;; -*- lexical-binding: t; -*-

(use-package! gptel
  :defer t
  :config
  ;; OPTIONAL configuration
  (setq! auth-source-debug t)
  (setq! gptel-default-mode 'org-mode)
  (setq! gptel-api-key (lambda()
                         (gptel-api-key-from-auth-source "chatapi.onechats.top") ))

  (setq! gptel-temperature 0)
  (setq! bailian-config
         (gptel-make-openai "bailian-config"
           :host "dashscope.aliyuncs.com"
           :endpoint "/compatible-mode/v1/chat/completions"
           :stream t
           :protocol "https"
           :key (lambda() (gptel-api-key-from-auth-source "bailian.console.aliyun.com"))
           :models '(Moonshot-Kimi-K2-Instruct)
           )
         )

  (setq! volcengine-config
         (gptel-make-openai "volcengine-config"
           :host "ark.cn-beijing.volces.com"
           :endpoint "/api/v3/chat/completions"
           :stream t
           :protocol "https"
           :key (lambda() (gptel-api-key-from-auth-source "work.console.volcengine.com"))
           :models '(kimi-k2-250711)
           ))

  (setq onechats-config
        (gptel-make-openai "OpenAI Proxy" ;Any name you want
          :host "chatapi.onechats.ai"
          :endpoint "/chat/completions"
          :stream t
          :key 'gptel-api-key
          :models '(kimi-k2))
        )

  (setq claude-config
        (gptel-make-openai "Claude Proxy" ;Any name you want
          :host "chatapi.onechats.top"
          :endpoint "/chat/completions"
          :stream t
          :key 'gptel-api-key
          :models '(claude-3-haiku-20240307 claude-3-5-sonnet-20240620))
        )

  (setq! githubmodels-config
         (gptel-make-openai "Github Models" ;Any name you want
           :host "models.inference.ai.azure.com"
           :endpoint "/chat/completions?api-version=2024-05-01-preview"
           :stream t
           :key (lambda() (gptel-api-key-from-auth-source "models.inference.ai.azure.com"))
           :models '(DeepSeek-R1 gpt-4o-mini gpt-4o gpt-4o))
         )

  (setq! deepseek-config
         (gptel-make-deepseek "DeepSeek"       ;Any name you want
           :stream t                           ;for streaming responses
           :models '("deepseek-chat" "deepseek-reasoner")
           :key (lambda() (gptel-api-key-from-auth-source "api.deepseek.com")))
         )
  (setq! openrouter-config
         (gptel-make-openai "OpenRouter"               ;Any name you want
           :host "openrouter.ai"
           :endpoint "/api/v1/chat/completions"
           :stream t
           :key (lambda() (gptel-api-key-from-auth-source "openrouter.ai"))                   ;can be a function that returns the key
           :models '(
                     google/gemini-2.5-pro-preview
                     qwen/qwen3-235b-a22b:free
                     qwen/qwen3-8b:free
                     deepseek/deepseek-chat-v3-0324:free
                     ))
         )

  ;; (setq! gptel-model 'deepseek-chat
  ;;        gptel-backend deepseek-config)

  (setq! gptel-model 'kimi-k2-250711
         gptel-backend volcengine-config)



  (gptel-make-preset 'deepseek-with-fetch                      ;preset name, a symbol
    :description "deepseek chat with fetch tool" ;for your reference
    :backend deepseek-config                     ;gptel backend or backend name
    :model 'deepseek-chat
    :tools '("fetch")) ;gptel tools or tool names
  )


(defun my/gptel-write-buffer ()
  "Save buffer to disk when starting gptel"
  (unless (buffer-file-name (current-buffer))
    (let ((suffix (format-time-string "%Y%m%dT%H%M" (current-time)))
          (chat-dir "~/org-roam-dir/chat"))
      (unless (file-directory-p chat-dir)
        (make-directory chat-dir :parents))
      (write-file (expand-file-name (concat "gptel-" suffix ".org") chat-dir)))))

(add-hook 'gptel-mode-hook #'my/gptel-write-buffer)



(provide 'gptel.el)
