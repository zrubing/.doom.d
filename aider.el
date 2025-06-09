;; -*- lexical-binding: t; -*-
(use-package aider
  :config
  (setq aider-prompt-file-name ".aider.prompt.org")

  ;; (setq aider-args '("--model" "openai/gpt-4o-mini" "--no-auto-commits"))

  ;; (setenv "OPENAI_API_BASE" "https://chatapi.onechats.top/v1/")
  ;; (let ((api-key (funcall (plist-get (car (auth-source-search :host "chatapi.onechats.top")) :secret))))
  ;;  (setenv "OPENAI_API_KEY" (encode-coding-string api-key 'utf-8)))

  ;; (setenv "OPENAI_API_BASE" "https://api.shubiaobiao.com/v1/")
  ;; (let ((api-key (funcall (plist-get (car (auth-source-search :host "zkt.api.shubiaobiao.com")) :secret))))
  ;;  (setenv "OPENAI_API_KEY" (encode-coding-string api-key 'utf-8)))


  ;; (setenv "OPENAI_API_BASE" "https://dashscope.aliyuncs.com/compatible-mode/v1")
  ;; (let ((api-key (funcall (plist-get (car (auth-source-search :host "bailian.console.aliyun.com")) :secret))))
  ;;   (setenv "OPENAI_API_KEY" (encode-coding-string api-key 'utf-8)))


  ;; (let ((api-key (funcall (plist-get (car (auth-source-search :host "api.deepseek.com")) :secret))))
  ;;   (setenv "DEEPSEEK_API_KEY" (encode-coding-string api-key 'utf-8)))


  ;; (let ((api-key (funcall (plist-get (car (auth-source-search :host "work.console.volcengine.com")) :secret))))
  ;;   (setenv "VOLCENGINE_API_KEY" (encode-coding-string api-key 'utf-8)))


  ;;(setq aider-args '("--model" "openrouter/qwen/qwen3-32b" "--no-auto-commits"))
  ;;(setq aider-args '("--model" "openrouter/qwen/qwen3-235b-a22b" "--no-auto-commits"))
  ;; ---------------------- open router -------------------
  (let ((api-key (funcall (plist-get (car (auth-source-search :host "openrouter.ai")) :secret))))
    (setenv "OPENROUTER_API_KEY" (encode-coding-string api-key 'utf-8)))

  ;; (setq aider-args '("--model" "gemini-2.5-pro" "--no-auto-commits"))
  ;; (let ((api-key (funcall (plist-get (car (auth-source-search :host "google.com")) :secret))))
  ;;   (setenv "GEMINI_API_KEY" (encode-coding-string api-key 'utf-8)))



  (let* ((current-dir (file-name-directory load-file-name))
         (model-dir (expand-file-name "aider-model-setting" current-dir))
         (model-settings-file (expand-file-name "model-setting.yml" model-dir)))

    ;; for open api base proxy
    ;;(setq aider-args `("--model" "openai/google/gemini-2.5-flash" "--no-auto-commits" "--edit-format" "diff" "--read" ,(expand-file-name "CONVENTIONS.md" model-dir) "--model-settings-file" ,model-settings-file))

    ;;(setq aider-args `("--model" "openai/gpt-4.1" "--no-auto-commits" "--read" ,(expand-file-name "CONVENTIONS.md" model-dir) "--model-settings-file" ,model-settings-file))

    ;;(setq aider-args `("--model" "volcengine/deepseek-v3-250324" "--no-auto-commits" "--read" ,(expand-file-name "CONVENTIONS.md" model-dir) "--model-settings-file" ,model-settings-file))
    ;;(setq aider-args `("--model" "volcengine/deepseek-r1-250528" "--no-auto-commits" "--read" ,(expand-file-name "CONVENTIONS.md" model-dir) "--model-settings-file" ,model-settings-file))
    ;;(setq aider-args `("--model" "deepseek/deepseek-chat" "--no-auto-commits" "--edit-format" "diff" "--read" ,(expand-file-name "CONVENTIONS.md" model-dir) "--model-settings-file" ,model-settings-file))
    ;;(setq aider-args `("--model" "deepseek/deepseek-reasoner" "--no-auto-commits" "--edit-format" "diff" "--read" ,(expand-file-name "CONVENTIONS.md" model-dir) "--model-settings-file" ,model-settings-file))
    (setq aider-args `("--model" "openrouter/google/gemini-2.5-pro-preview" "--no-auto-commits" "--edit-format" "diff" "--thinking-tokens" "128" "--read" ,(expand-file-name "CONVENTIONS.md" model-dir) "--model-settings-file" ,model-settings-file))
    ;;(setq aider-args `("--model" "openrouter/google/gemini-2.5-flash-preview-05-20" "--edit-format" "diff" "--no-auto-commits" "--read" ,(expand-file-name "CONVENTIONS.md" model-dir) "--model-settings-file" ,model-settings-file))
    ;;(setq aider-args `("--model" "openrouter/openai/gpt-4.1" "--no-auto-commits" "--read" ,(expand-file-name "CONVENTIONS.md" model-dir) "--model-settings-file" ,model-settings-file))
    ;;(setq aider-args `("--model" "openrouter/deepseek/deepseek-chat-v3-0324:free" "--no-auto-commits" "--read" ,(expand-file-name "CONVENTIONS.md" model-dir) "--model-settings-file" ,model-settings-file))
    ;;(setq aider-args `("--model" "openai/qwen-turbo-2025-04-28" "--no-auto-commits" "--model-metadata-file" ,model-settings-file))
    ;;(setq aider-args `("--model" "openai/qwen3-30b-a3b" "--no-auto-commits" "--edit-format" "diff" "--read" ,(expand-file-name "CONVENTIONS.md" model-dir) "--model-settings-file" ,model-settings-file))
    ;;(setq aider-args `("--model" "openai/qwen3-235b-a22b" "--no-auto-commits" "--edit-format" "diff" "--read" ,(expand-file-name "CONVENTIONS.md" model-dir) "--model-settings-file" ,model-settings-file))

    ;;(setq aider-args `("--model" "openrouter/qwen/qwen3-30b-a3b" "--no-auto-commits" "--model-settings-file" ,model-settings-file))
    ;;(setq aider-args '("--model" "openrouter/qwen/qwen3-32b" "--no-auto-commits"))

    )

  ;;(setq aider-args '("--model" "volcengine/deepseek-v3-250324" "--no-auto-commits" ""))
  (global-set-key (kbd "C-c a") 'aider-transient-menu))
(provide 'aider)
