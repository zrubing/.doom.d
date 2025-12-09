;; -*- lexical-binding: t; -*-

(use-package! aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config

  ;; 推荐写法：直接设置列表，并将参数名和参数值分开
  (setq aidermacs-extra-args
        '(
          "--edit-format" "diff"
          "--aiderignore" ".gitignore"
          "--chat-language" "chinese"
          "--no-auto-commits"))

  ;; 从 default.el 中的 gptel 配置动态生成 aidermacs 配置
  ;; 前提：需要先加载 gptel 配置（配置已在 default.el 中定义）
  (defun aidermacs-generate-configs-from-gptel ()
    "从 default.el 中的 +gptel-models 配置生成 aidermacs 兼容的配置列表。"
    (when (boundp '+gptel-models)
      (apply #'append
             (mapcar (lambda (cfg)
                       (let* ((provider (symbol-name (car cfg)))
                              (props (cdr cfg))
                              (type (plist-get props :type))
                              (host (plist-get props :host))
                              (key-key (plist-get props :key-key))
                              (models (plist-get props :models))
                              (endpoint (plist-get props :endpoint))
                              (protocol (plist-get props :protocol)))
                         (mapcar (lambda (model)
                                   ;; 生成符合 aidermacs 格式的配置：
                                   ;; (提供者名称 模型标识符 API基础URL 主机名 key-key type)
                                   ;; 注意：model 需要转换为字符串，并根据配置中的type字段添加前缀
                                   (let* ((model-name (symbol-name model))
                                          ;; 根据配置中的type字段作为前缀
                                          (model-with-prefix (concat (symbol-name type) "/" model-name))
                                          ;; 构建完整的API base URL，移除 /chat/completions
                                          (api-base-url (when (and host endpoint protocol)
                                                          (replace-regexp-in-string "/chat/completions$" ""
                                                                                    (concat protocol "://" host endpoint)))))
                                     ;; 返回统一的配置格式，现在包含 type
                                     (list provider model-with-prefix api-base-url host key-key type)))
                                 models)))
                     +gptel-models))))

  ;; 定义模型配置列表，复用 default.el 中 gptel 的配置
  (defvar aidermacs-model-configs nil
    "aidermacs 模型配置列表，从 default.el 中的 +gptel-models 动态生成。
每个配置包含：(提供者名称 模型标识符 API基础URL 主机名 key-key)")

  ;; 保存上次选择的模型索引
  (defcustom aidermacs-last-model-index 0
    "上次选择的模型配置在 aidermacs-model-configs 中的索引。"
    :type 'integer
    :group 'aidermacs)

  ;; 交互式切换模型
  (defun aidermacs-switch-model ()
    "交互选择模型并设置相关环境变量和配置。"
    (interactive)
    ;; 如果配置为空，尝试重新生成
    (when (null aidermacs-model-configs)
      (setq aidermacs-model-configs (aidermacs-generate-configs-from-gptel)))

    (let* ((choices (mapcar (lambda (config)
                              (format "[%s] %s" (nth 0 config) (nth 1 config)))
                            aidermacs-model-configs))
           (choice (completing-read "选择模型: " choices nil t))
           (index (cl-position choice choices :test #'string=))
           (config (nth index aidermacs-model-configs))
           (provider (nth 0 config))
           (model-id (nth 1 config))
           (api-base (nth 2 config))
           (host (nth 3 config))
           (key-key (nth 4 config))
           (type (nth 5 config)))
      ;; 设置 API 基础 URL（如果提供）
      (when api-base
        (setenv "OPENAI_API_BASE" api-base))

      ;; 从 auth-source 获取 API 密钥
      ;; 对于某些服务，aidermacs 可能需要设置特定的环境变量
      (let* ((auth-source-do-cache nil)  ;; 禁用缓存
             (auth-info (car (auth-source-search :host key-key :require '(:secret))))
             (secret (plist-get auth-info :secret))
             (api-key (if (functionp secret)
                          (funcall secret)
                        secret)))
        (when api-key
          ;; 根据 type 设置相应的环境变量
          (cond
           ;; 如果 type 是 openai，设置 OPENAI_API_KEY 和 OPENAI_API_BASE
           ((eq type 'openai)
            (setenv "OPENAI_API_KEY" (encode-coding-string api-key 'utf-8))
            (when api-base
              (setenv "OPENAI_API_BASE" api-base))
            (message "已为 OpenAI 兼容 API 设置密钥和基础 URL"))
           ;; 可以在这里添加其他类型的处理
           (t
            (setenv "OPENAI_API_KEY" (encode-coding-string api-key 'utf-8))
            (message "已为 %s 设置 API 密钥" host)))))

      ;; 设置 aidermacs 默认模型
      (setq aidermacs-default-model model-id)

      ;; 保存选择的模型索引
      (message "[aidermacs-switch-model] 保存索引: %d" index)
      (setq aidermacs-last-model-index index)
      (put 'aidermacs-last-model-index 'saved-value index)
      (customize-save-variable 'aidermacs-last-model-index aidermacs-last-model-index)
      (message "[aidermacs-switch-model] 保存后的值: %d" aidermacs-last-model-index)

      (message "aidermacs 已切换到模型: [%s] %s" provider model-id)))

  ;; 加载上次保存的模型配置
  (defun aidermacs-load-last-model ()
    "加载上次保存的模型配置。"
    (message "[aidermacs-load-last-model] 开始加载上次保存的模型配置...")

    ;; 从 customize 文件加载保存的值
    (message "[aidermacs-load-last-model] 尝试从 custom.el 加载保存的值...")
    (let ((custom-file (expand-file-name "custom.el" doom-user-dir)))
      (message "[aidermacs-load-last-model] custom.el 路径: %s" custom-file)
      (when (file-exists-p custom-file)
        (message "[aidermacs-load-last-model] custom.el 存在，尝试解析...")
        (with-temp-buffer
          (insert-file-contents custom-file)
          (goto-char (point-min))
          (when (re-search-forward "'(aidermacs-last-model-index \\([0-9]+\\))" nil t)
            (let ((saved-index (string-to-number (match-string 1))))
              (message "[aidermacs-load-last-model] 找到保存的索引: %d" saved-index)
              (setq aidermacs-last-model-index saved-index))))))

    ;; 记录当前状态
    (message "[aidermacs-load-last-model] 当前 aidermacs-last-model-index: %s" aidermacs-last-model-index)
    (message "[aidermacs-load-last-model] 当前 aidermacs-model-configs 数量: %d"
             (if aidermacs-model-configs (length aidermacs-model-configs) 0))
    ;; 如果配置为空，尝试重新生成
    (when (null aidermacs-model-configs)
      (message "[aidermacs-load-last-model] 模型配置为空，尝试从 gptel 生成...")
      (setq aidermacs-model-configs (aidermacs-generate-configs-from-gptel))
      (message "[aidermacs-load-last-model] 已生成 %d 个模型配置"
               (if aidermacs-model-configs (length aidermacs-model-configs) 0)))

    (let ((config (nth aidermacs-last-model-index aidermacs-model-configs)))
      (if config
          (progn
            (message "[aidermacs-load-last-model] 找到配置索引 %d" aidermacs-last-model-index)
            (let* ((provider (nth 0 config))
                   (model-id (nth 1 config))
                   (api-base (nth 2 config))
                   (host (nth 3 config))
                   (key-key (nth 4 config))
                   (type (nth 5 config)))
              (message "[aidermacs-load-last-model] 解析配置: provider=%s, model=%s, api-base=%s, host=%s, key-key=%s, type=%s"
                       provider model-id api-base host key-key type)

              ;; 设置 API 基础 URL
              (when api-base
                (message "[aidermacs-load-last-model] 设置 API 基础 URL: %s" api-base)
                (setenv "OPENAI_API_BASE" api-base)
                (message "[aidermacs-load-last-model] OPENAI_API_BASE 已设置为: %s" (getenv "OPENAI_API_BASE"))
                (message "[aidermacs-load-last-model] api-base 环境变量验证结果: %s" api-base))

              ;; 获取 API 密钥并设置相应的环境变量
              (message "[aidermacs-load-last-model] 开始获取 API 密钥，key-key: %s" key-key)
              (let* ((auth-source-do-cache nil)
                     (auth-info (car (auth-source-search :host key-key :require '(:secret))))
                     (secret (plist-get auth-info :secret))
                     (api-key (if (functionp secret)
                                  (funcall secret)
                                secret)))
              (message "[aidermacs-load-last-model] auth-info: %s" (if auth-info "found" "nil"))
              (when api-key
                (message "[aidermacs-load-last-model] 成功获取 API 密钥 (长度: %d)" (length api-key))
                ;; 根据 type 设置相应的环境变量
                (cond
                 ;; 如果 type 是 openai，设置 OPENAI_API_KEY 和 OPENAI_API_BASE
                 ((eq type 'openai)
                  (message "[aidermacs-load-last-model] 检测到 OpenAI 类型，设置环境变量")
                  (setenv "OPENAI_API_KEY" (encode-coding-string api-key 'utf-8))
                  (when api-base
                    (message "[aidermacs-load-last-model] 再次设置 OPENAI_API_BASE: %s" api-base)
                    (setenv "OPENAI_API_BASE" api-base))
                  (message "[aidermacs-load-last-model] 最终 OPENAI_API_KEY 长度: %d" (length (getenv "OPENAI_API_KEY")))
                  (message "[aidermacs-load-last-model] 最终 OPENAI_API_BASE: %s" (getenv "OPENAI_API_BASE"))
                  (message "已为 OpenAI 兼容 API 设置密钥和基础 URL"))
                 ;; 可以在这里添加其他类型的处理
                 (t
                  (message "[aidermacs-load-last-model] 使用默认类型设置环境变量")
                  (setenv "OPENAI_API_KEY" (encode-coding-string api-key 'utf-8))
                  (message "已为 %s 设置 API 密钥" host))))
              (unless api-key
                (message "[aidermacs-load-last-model] 错误: API 密钥为空")))

            ;; 设置默认模型
            (message "[aidermacs-load-last-model] 设置默认模型: %s" model-id)
            (setq aidermacs-default-model model-id)
            (message "aidermacs 已加载上次使用的模型: [%s] %s" provider model-id)))
        (message "[aidermacs-load-last-model] 错误: 未找到索引 %d 的配置" aidermacs-last-model-index)))))
  ;; 定义 transient 菜单 (已注释，使用原生功能)
  ;; (transient-define-prefix aidermacs-transient-menu ()
  ;;   "aidermacs 的 transient 菜单"
  ;;   [["模型管理"
  ;;     ("m" "切换模型" aidermacs-switch-model)]
  ;;    ["aidermacs 操作"
  ;;     ("q" "退出" transient-quit-one)]])

  ;; 在 use-package 块外部进行初始化
(with-eval-after-load 'aidermacs
  ;; 确保配置正确加载
  (when (fboundp 'aidermacs-generate-configs-from-gptel)
    (setq aidermacs-model-configs (aidermacs-generate-configs-from-gptel)))
  ;; 加载上次保存的模型
  (when (fboundp 'aidermacs-load-last-model)
    (aidermacs-load-last-model)))

(provide 'aidermacs)
