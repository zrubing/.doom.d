;; -*- lexical-binding: t; -*-
(use-package aider
  :config
  (setq aider-prompt-file-name ".aider.prompt.org")
  (setq aider-enable-markdown-highlighting nil)

  ;; 定义模型配置列表，每个元素是 (名称 BASE_URL API_KEY_HOST API_KEY_ENV_VAR)
  ;; default.me.api.master-jsx.top
  (setq +aider-model-configs
        '(("api-master" "openai/gpt-5-codex" "https://api.master-jsx.top/v1" "default.me.api.master-jsx.top" "OPENAI_API_KEY")
          ("api-master" "openai/gpt-5-chat-latest" "https://api.master-jsx.top/v1" "microsoft.me.api.master-jsx.top" "OPENAI_API_KEY")

          ("api-master" "openai/grok-4-fast-non-reasoning" "https://api.master-jsx.top/v1" "microsoft.me.api.master-jsx.top" "OPENAI_API_KEY")
          ("api-master" "openai/grok-4-fast-reasoning" "https://api.master-jsx.top/v1" "microsoft.me.api.master-jsx.top" "OPENAI_API_KEY")

          ("moonshot" "openai/kimi-k2-0905-preview" "https://api.moonshot.cn/v1" "api.moonshot.cn" "OPENAI_API_KEY")
          ("moonshot" "openai/kimi-k2-turbo-preview" "https://api.moonshot.cn/v1" "api.moonshot.cn" "OPENAI_API_KEY")

          ("bailian" "bailian" "https://dashscope.aliyuncs.com/compatible-mode/v1" "bailian.console.aliyun.com" "OPENAI_API_KEY")
          ("deepseek" "deepseek" nil "api.deepseek.com" "DEEPSEEK_API_KEY")
          ("volcengine" "volcengine/kimi-k2-250905" nil "work.console.volcengine.com" "VOLCENGINE_API_KEY")
          ("volcengine" "volcengine/deepseek-v3-250324" nil "work.console.volcengine.com" "VOLCENGINE_API_KEY")
          ("volcengine" "volcengine/doubao-seed-1-6-250615" nil "work.console.volcengine.com" "VOLCENGINE_API_KEY")
          ("volcengine" "volcengine/deepseek-r1-250528" nil "work.console.volcengine.com" "VOLCENGINE_API_KEY")
          ("volcengine" "volcengine/deepseek-v3-1-terminus" nil "work.console.volcengine.com" "VOLCENGINE_API_KEY")))

  ;; 交互式切换模型
  (defun +aider-switch-model ()
    "交互选择模型并设置 BASE_URL、API_KEY 及 aider-args."
    (interactive)
    (let* ((choices (mapcar (lambda (config)
                              (format "[%s] %s" (nth 0 config) (nth 1 config)))
                            +aider-model-configs))
           (choice (completing-read "选择模型: " choices))
           (config (seq-find (lambda (c) 
                               (string= choice (format "[%s] %s" (nth 0 c) (nth 1 c))))
                             +aider-model-configs))
           (provider (nth 0 config))
           (model-name (nth 1 config))
           (base (nth 2 config))
           (host (nth 3 config))
           (env  (nth 4 config)))
      (when base
        (setenv "OPENAI_API_BASE" base))

      (let* ((auth-source-do-cache nil)  ;; 禁用缓存
             (auth-source-debug t)       ;; 如需调试可保留此行
             (auth-info (car (auth-source-search :host host :require '(:secret))))
             (secret (plist-get auth-info :secret))
             (api-key (if (functionp secret)
                          (funcall secret)
                        secret)))
        (when api-key
          (setenv env (encode-coding-string api-key 'utf-8))
          (message "已设置环境变量 %s" env)))

      ;; 根据模型名称动态设置 aider-args
      (setq aider-args
            (list "--model" model-name
                  "--edit-format" "diff"
                  "--chat-language" "chinese"
                  "--no-auto-commits"))
      ;; 检测并关闭已有的 aider 相关进程，防止配置不生效
      (let ((aider-buffers (seq-filter (lambda (buf)
                                         (with-current-buffer buf
                                           (derived-mode-p 'aider-mode 'aider-comint-mode)))
                                       (buffer-list))))
        (dolist (buf aider-buffers)
          (when (get-buffer-process buf)
            (kill-process (get-buffer-process buf)))
          (kill-buffer buf)))
      (message "已切换到模型: [%s] %s 并更新 aider-args" provider model-name)
      (message "当前使用的模型是: [%s] %s" provider (cadr aider-args))))

  ;; 默认加载第一个模型配置
  ;; (let ((default-config (nth 0 +aider-model-configs)))
  ;;   (+aider-switch-model))




  (let* ((current-dir (file-name-directory load-file-name))
         (model-dir (expand-file-name "aider-model-setting" current-dir))
         (model-settings-file (expand-file-name "model-setting.yml" model-dir)))
    
    )

  ;; hook报错，先移除
  ;; (add-hook 'aider-mode-hook
  ;;           (lambda ()
  ;;             (advice-remove #'comint-output-filter #'doom--comint-enable-undo-a)))

  (add-hook! 'aider-mode-hook
    (defun +aider-remove-doom-advice-h ()
      (advice-remove #'comint-output-filter #'doom--comint-enable-undo-a)
      (advice-remove #'comint-output-filter #'doom--comint-protect-output-in-visual-modes-a)
      (remove-hook 'comint-output-filter-functions #'doom--comint-protect-output-in-visual-modes-a)))

  (add-hook! 'aider-comint-mode-hook
             #'+aider-remove-doom-advice-h)

  ;;(setq aider-args '("--model" "volcengine/deepseek-v3-250324" "--no-auto-commits" ""))
  (global-set-key (kbd "C-c a") 'aider-transient-menu))
(provide 'aider)
