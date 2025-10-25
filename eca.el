;;; -*- lexical-binding: t; -*-
;;; ECA - Emacs Code Assistant 统一配置

(require 'json)

;;;### 变量定义
(defcustom eca-last-model-index 0
  "上次选择的模型配置索引。"
  :type 'integer
  :group 'eca)

(defvar eca-config-dir
  (if (and (fboundp 'doom-project-root) (doom-project-root))
      (expand-file-name ".eca" (doom-project-root))
    (expand-file-name ".config/eca" (getenv "HOME")))
  "ECA 配置目录")

(defvar eca-config-path
  (expand-file-name "config.json" eca-config-dir)
  "ECA 配置文件路径")

(defvar eca-current-behavior "agent"
  "当前 ECA 行为模式")

(defun eca-convert-gptel-to-eca (gptel-models)
  "将 gptel 模型配置转换为 ECA providers 配置"
  (let ((providers '()))
    (dolist (cfg gptel-models)
      (let* ((name (car cfg))
             (props (cdr cfg))
             (type (plist-get props :type))
             (host (plist-get props :host))
             (endpoint (plist-get props :endpoint))
             (key-key (plist-get props :key-key))
             (models (plist-get props :models))
             (provider-name (replace-regexp-in-string "-config$" "" (symbol-name name)))
             ;; 处理特殊的 deepseek 配置（没有 host 字段）
             (api-url (cond
                       ((eq type 'deepseek) "https://api.deepseek.com")
                       ((string-prefix-p "https://" host) host)
                       (t (concat "https://" host))))
             (path-adjustment (when endpoint
                               (replace-regexp-in-string "^/v1/chat/completions" "" endpoint))))

        ;; 创建 provider 配置
        (push (cons provider-name
                    (list :api "openai-chat"
                          :url api-url
                          :keyRc (concat "apikey@" key-key)
                          :completionUrlRelativePath path-adjustment
                          :models (mapcar (lambda (model)
                                            (cons (symbol-name model)
                                                  (list :modelName (symbol-name model))))
                                          models)))
              providers)))
    providers))

(defun eca-generate-config (&optional from-gptel)
  "生成完整的 ECA 配置文件

交互式调用时，默认从 gptel 配置转换（推荐），可选择生成默认配置。

编程调用时：
- FROM-GPTEL 为 t 或省略：从 gptel 配置转换
- FROM-GPTEL 为 nil：生成默认配置"
  (interactive)
  ;; 确保 from-gptel 总是有定义
  (if (null from-gptel)
      (if (called-interactively-p 'interactive)
          ;; 交互式调用时询问用户
          (setq from-gptel (y-or-n-p "从 gptel 配置转换模型？(推荐) "))
        ;; 非交互式调用时默认从 gptel 转换
        (setq from-gptel t)))
  ;; 确保配置目录和子目录存在
  (unless (file-exists-p eca-config-dir)
    (make-directory eca-config-dir t))
  (unless (file-exists-p (expand-file-name "rules" eca-config-dir))
    (make-directory (expand-file-name "rules" eca-config-dir) t))
  (unless (file-exists-p (expand-file-name "commands" eca-config-dir))
    (make-directory (expand-file-name "commands" eca-config-dir) t))

  ;; 读取现有配置（如果存在）
  (let ((existing-config (eca-read-current-config))
        (base-config (list :defaultBehavior "agent"
                           :welcomeMessage "欢迎使用 ECA! 🚀\n\n输入 '/' 查看命令\n\n已配置中文支持"
                           :toolCall (list :approval (list :byDefault "allow"))))
        config-data)

    ;; 生成配置数据
    (setq config-data (if from-gptel
                          ;; 从 gptel 配置转换
                          (when (boundp '+gptel-models)
                            (let ((providers (eca-convert-gptel-to-eca +gptel-models)))
                              ;; 设置默认模型
                              (when providers
                                (let* ((first-provider (caar providers))
                                       (first-models (plist-get (cdar providers) :models))
                                       (first-model (caar first-models)))
                                  (plist-put base-config :defaultModel
                                            (concat first-provider "/" first-model))
                                  (plist-put base-config :providers providers)))))
                        ;; 使用默认配置
                        (plist-put base-config :defaultModel "volcengine/kimi-k2-250905")))

    ;; 使用智能合并配置
    (when config-data
      (let ((merged-config (eca-merge-configs config-data existing-config)))
        (let ((config (let ((json-encoding-pretty-print t))
                   (json-encode merged-config))))

          ;; 写入配置文件，确保正确处理编码
          (condition-case err
              (progn
                (with-temp-buffer
                  ;; 设置缓冲区编码为 UTF-8
                  (set-buffer-file-coding-system 'utf-8-unix)
                  (insert config)
                  ;; 使用 write-region 而不是 with-temp-file 来更好地控制编码
                  (let ((coding-system-for-write 'utf-8-unix))
                    (write-region (point-min) (point-max) eca-config-path nil 'silent)))
                ;; 仅在交互调用时显示成功消息
                (when (called-interactively-p 'interactive)
                  (if from-gptel
                      (message "✅ ECA 配置已从 gptel 生成到: %s" eca-config-path)
                    (message "✅ ECA 默认配置已生成到: %s" eca-config-path))))

            (error
             ;; 错误处理 - 仅在交互调用时显示错误
             (when (called-interactively-p 'interactive)
               (message "❌ ECA 配置生成失败: %s" (error-message-string err))))))))))

;;;### 模型切换功能
(defun eca-read-current-config ()
  "读取当前 ECA 配置文件"
  (when (file-exists-p eca-config-path)
    (with-temp-buffer
      (insert-file-contents eca-config-path)
      (goto-char (point-min))
      (json-parse-buffer :object-type 'plist))))

(defun eca-merge-configs (new-config existing-config)
  "智能合并配置，保留重要的现有配置项

NEW-CONFIG: 新的配置数据 (plist)
EXISTING-CONFIG: 现有配置数据 (plist)

返回合并后的配置，保留 mcpServers 和其他重要配置"
  (let ((merged-config (copy-sequence new-config)))
    ;; 保留现有配置中的 mcpServers（如果存在）
    (when (and existing-config (plist-get existing-config :mcpServers))
      (setq merged-config (plist-put merged-config :mcpServers (plist-get existing-config :mcpServers))))
    
    ;; 保留其他可能存在的配置项（除了我们明确要覆盖的）
    (when existing-config
      (dolist (key '(:rules :commands :workspaces :customSettings))
        (when (plist-get existing-config key)
          (setq merged-config (plist-put merged-config key (plist-get existing-config key))))))
    
    merged-config))

(defun eca-write-config (config-data)
  "写入配置数据到 ECA 配置文件，支持配置合并而不是完全覆盖

CONFIG-DATA 应该是一个 plist，函数会将其与现有配置合并，保留 mcpServers 等关键配置"
  ;; 确保配置目录存在
  (unless (file-exists-p eca-config-dir)
    (make-directory eca-config-dir t))

  ;; 读取现有配置并合并
  (let ((existing-config (eca-read-current-config))
        (merged-config (eca-merge-configs config-data existing-config)))

    ;; 写入配置文件，确保正确处理编码
    (condition-case err
        (let ((config (let ((json-encoding-pretty-print t))
                   (json-encode merged-config))))
          (with-temp-buffer
            ;; 设置缓冲区编码为 UTF-8
            (set-buffer-file-coding-system 'utf-8-unix)
            (insert config)
            ;; 使用 write-region 来更好地控制编码
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (point-min) (point-max) eca-config-path nil 'silent)))
          t)
      (error
       (message "❌ 配置文件写入失败: %s" (error-message-string err))
       nil))))

(defun eca-get-available-models ()
  "获取所有可用的模型列表"
  (let* ((config (eca-read-current-config))
         (providers (plist-get config :providers))
         (models '()))
    (when providers
      ;; 遍历所有 provider，providers 是一个 plist
      (let ((provider-list providers))
        (while provider-list
          (let* ((provider-key (car provider-list))
                 (provider-value (cadr provider-list))
                 (provider-models (plist-get provider-value :models)))
            (when provider-models
              ;; 遍历 provider 的所有模型，models 也是一个 plist
              (let ((model-list provider-models))
                (while model-list
                  (let* ((model-key (car model-list))
                         (model-value (cadr model-list)))
                    (push (list (concat (substring (symbol-name provider-key) 1) "/" (substring (symbol-name model-key) 1))
                               (substring (symbol-name provider-key) 1)
                               (substring (symbol-name model-key) 1))
                          models))
                  (setq model-list (cddr model-list)))))
            (setq provider-list (cddr provider-list))))))
    (nreverse models)))

(defun eca-switch-model ()
  "交互式切换 ECA 模型"
  (interactive)
  (let* ((available-models (eca-get-available-models))
         (model-names (mapcar (lambda (model) (car model)) available-models))
         (current-config (eca-read-current-config))
         (current-model (plist-get current-config :defaultModel))
         (current-index (or (and current-model
                                (cl-position current-model model-names :test 'equal))
                           0)))
    (if (null available-models)
        (message "❌ 没有可用的模型配置，请先生成配置")
      (let* ((selected-model-name (completing-read
                                    (format "选择模型 (当前: %s): " current-model)
                                    model-names
                                    nil
                                    t
                                    nil
                                    nil
                                    current-model))
             (selected-model (cl-find selected-model-name available-models
                                      :key (lambda (model) (car model))
                                      :test 'equal))
             (selected-provider (cadr selected-model))
             (selected-model-key (caddr selected-model)))
        (when selected-model
          ;; 更新配置，包括动态更新欢迎消息
          (let* ((welcome-template "欢迎使用 ECA! 🚀\n\n输入 '/' 查看命令\n\n已配置中文支持\n\n当前默认模型: %s")
                 (new-welcome (format welcome-template selected-model-name selected-provider))
                 (new-config (plist-put current-config :defaultModel selected-model-name)))
            (setq new-config (plist-put new-config :welcomeMessage new-welcome))
            (if (eca-write-config new-config)
                (progn
                  ;; 更新最后选择的模型索引
                  (setq eca-last-model-index (cl-position selected-model-name model-names :test 'equal))
                  (message "✅ 已切换到模型: %s" selected-model-name)
                  ;; 询问是否重启 ECA 服务器
                  (when (y-or-n-p "是否重启 ECA 服务器以应用新配置？")
                    (eca-restart-server)))
              (message "❌ 模型切换失败"))))))))

(defun eca-restart-server ()
  "重启 ECA 服务器（调用 Emacs 函数 eca-restart）"
  (interactive)
  (condition-case err
      (progn
        (message "🔄 正在重启 ECA 服务器...")
        (if (fboundp 'eca-restart)
            (progn
              (eca-restart)
              (message "✅ ECA 服务器已通过 Emacs 函数重启"))
          (progn
            ;; 如果 eca-restart 函数不存在，回退到进程重启
            (let ((process (get-process "eca")))
              (if process
                  (progn
                    (delete-process process)
                    (message "🔄 已停止外部 ECA 服务器...")
                    (run-with-timer 2 nil
                                    (lambda ()
                                      (let ((default-directory (getenv "HOME")))
                                        (start-process "eca" "*ECA*" "eca" "server")
                                        (message "🚀 外部 ECA 服务器已重新启动")))))
                (message "⚠️  ECA 服务器未运行，尝试启动...")
                (let ((default-directory (getenv "HOME")))
                  (start-process "eca" "*ECA*" "eca" "server")
                  (message "🚀 外部 ECA 服务器已启动"))))))
        (message "✅ ECA 重启完成"))
    (error
     (message "❌ ECA 重启失败: %s" (error-message-string err)))))

(defun eca-update-welcome-message ()
  "更新配置文件中的欢迎消息以反映当前默认模型"
  (interactive)
  (let* ((config (eca-read-current-config))
         (current-model (plist-get config :defaultModel)))
    (when (and current-model config)
      (let* ((model-parts (split-string current-model "/"))
             (provider (if (>= (length model-parts) 2) (car model-parts) "未知"))
             (welcome-template "欢迎使用 ECA! 🚀\n\n输入 '/' 查看命令\n\n已配置中文支持\n\n当前默认模型: %s (供应商: %s)")
             (new-welcome (format welcome-template current-model provider))
             (new-config (plist-put config :welcomeMessage new-welcome)))
        (if (eca-write-config new-config)
            (message "✅ 欢迎消息已更新: 当前模型 %s (供应商: %s)" current-model provider)
          (message "❌ 欢迎消息更新失败"))))))

(defun eca-test-config-merge ()
  "测试配置合并功能"
  (interactive)
  (let* ((existing-config (eca-read-current-config))
         (new-config (list :defaultBehavior "agent"
                          :welcomeMessage "测试配置"
                          :defaultModel "test/model"
                          :providers '()))
         (merged-config (eca-merge-configs new-config existing-config)))
    (with-output-to-temp-buffer "*ECA Config Merge Test*"
      (princ "🤖 ECA 配置合并测试\n\n")
      (princ "📄 现有配置:\n")
      (princ (format "  mcpServers: %s\n" (if (plist-get existing-config :mcpServers) "存在" "不存在")))
      (princ (format "  rules: %s\n" (if (plist-get existing-config :rules) "存在" "不存在")))
      (princ (format "  commands: %s\n" (if (plist-get existing-config :commands) "存在" "不存在")))
      (princ "\n🔄 合并后配置:\n")
      (princ (format "  mcpServers: %s\n" (if (plist-get merged-config :mcpServers) "已保留" "不存在")))
      (princ (format "  rules: %s\n" (if (plist-get merged-config :rules) "已保留" "不存在")))
      (princ (format "  commands: %s\n" (if (plist-get merged-config :commands) "已保留" "不存在")))
      (princ (format "  defaultModel: %s\n" (plist-get merged-config :defaultModel))))))

(defun eca-status ()
  "显示 ECA 当前状态"
  (interactive)
  (let* ((config (eca-read-current-config))
         (current-model (plist-get config :defaultModel))
         (current-behavior (plist-get config :defaultBehavior))
         (process (get-process "eca")))
    (with-output-to-temp-buffer "*ECA Status*"
      (princ "🤖 ECA 状态信息\n\n")
      (princ (format "📍 当前模型: %s\n" (or current-model "未配置")))
      (princ (format "🎯 行为模式: %s\n" (or current-behavior "agent")))
      (princ (format "📁 配置目录: %s\n" eca-config-dir))
      (princ (format "📄 配置文件: %s\n" eca-config-path))
      (princ (format "🔄 服务器状态: %s\n"
                     (if process
                         (format "运行中 (PID: %d)" (process-id process))
                       "未运行")))
      (princ (format "🔧 mcpServers 配置: %s\n" (if (plist-get config :mcpServers) "已配置" "未配置")))
      (princ "\n🔧 可用操作:\n")
      (princ "  • M-x eca-switch-model - 切换模型\n")
      (princ "  • M-x eca-restart-server - 重启服务器\n")
      (princ "  • M-x eca-generate-config - 重新生成配置\n")
      (princ "  • M-x eca-update-welcome-message - 更新欢迎消息\n")
      (princ "  • M-x eca-test-config-merge - 测试配置合并功能"))))

(defun eca-quick-setup ()
  "快速设置 ECA"
  (interactive)
  (when (y-or-n-p "这将重新生成 ECA 配置文件，是否继续？")
    (eca-generate-config)
    (when (y-or-n-p "是否立即选择模型并启动服务器？")
      (eca-switch-model)
      (unless (get-process "eca")
        (eca-restart-server)))))

;;;### 添加到 autoload
;;;### autoload
(autoload 'eca-switch-model "eca" "切换 ECA 模型" t)
(autoload 'eca-restart-server "eca" "重启 ECA 服务器" t)
(autoload 'eca-status "eca" "显示 ECA 状态" t)
(autoload 'eca-quick-setup "eca" "快速设置 ECA" t)
(autoload 'eca-update-welcome-message "eca" "更新欢迎消息" t)
(autoload 'eca-test-config-merge "eca" "测试配置合并功能" t)

(provide 'eca)
;;; eca.el ends here
