;;; -*- lexical-binding: t; -*-

(use-package gptel
  :defer t
  :config
  ;; OPTIONAL configuration
  (setq auth-source-debug t)
  (setq gptel-default-mode 'org-mode)
  (setq gptel-api-key (lambda()
                         (gptel-api-key-from-auth-source "chatapi.onechats.top") ))

  (setq gptel-temperature 0)
  

  ;; +gptel-models 配置已在 default.el 中定义
  ;; 根据统一变量动态创建 backend 对象
  (mapc (lambda (cfg)
          (let* ((name (car cfg))
                 (props (cdr cfg))
                 (type (plist-get props :type))
                 (key (lambda() (gptel-api-key-from-auth-source (plist-get props :key-key))))
                 (kwargs (list :host (plist-get props :host)
                               :endpoint (plist-get props :endpoint)
                               :stream (plist-get props :stream)
                               :protocol (plist-get props :protocol)
                               :key key
                               :models (plist-get props :models))))
            (set name
                 (cond
                  ((eq type 'openai) (apply #'gptel-make-openai (symbol-name name) kwargs))
                  ((eq type 'deepseek) (apply #'gptel-make-deepseek (symbol-name name) kwargs))
                  (t (error "未知类型: %s" type))))))
        +gptel-models)

  ;; 用统一配置初始化后的默认选择
  (setq gptel-model 'gpt-5-chat-latest
         gptel-backend microsoft-api-master-jsx)

  ;; 定义模型配置列表，每个元素是 (名称 后端 模型)
  ;; 从 default.el 中的统一 +gptel-models 动态生成配置列表
  (setq +gptel-model-configs
        (apply #'append
               (mapcar (lambda (cfg)
                         (let* ((name (car cfg))
                                (backend (symbol-value name))
                                (models (plist-get (cdr cfg) :models))
                                (provider (symbol-name name)))
                           (mapcar (lambda (m)
                                     (list provider backend m))
                                   models)))
                       +gptel-models)))

  ;; 交互式切换模型
  (defun +gptel-switch-model ()
    "交互选择模型并设置 gptel-model 和 gptel-backend."
    (interactive)
    (let* ((choices (mapcar (lambda (config)
                              (format "[%s] %s" (nth 0 config) (nth 2 config)))
                            +gptel-model-configs))
           (choice (completing-read "选择模型: " choices))
           (config (seq-find (lambda (c) 
                               (string= choice (format "[%s] %s" (nth 0 c) (nth 2 c))))
                             +gptel-model-configs))
           (provider (nth 0 config))
           (backend (nth 1 config))
           (model (nth 2 config)))
      (setq gptel-model model)
      (setq gptel-backend backend)
      (message "已切换到模型: [%s] %s" provider model)))




  ;; Define custom presets
  (when (fboundp 'gptel-make-preset)
    (gptel-make-preset 'instruction
                       :system (concat "- 高可用\n"
                                       "- KISS原则\n"
                                       "- 清晰\n"))

    (when (boundp 'deepseek-config)
      (gptel-make-preset 'deepseek-with-fetch
                         :description "deepseek chat with fetch tool"
                         :backend deepseek-config
                         :model 'deepseek-chat
                         :tools '("fetch")))

    (when (boundp 'volcengine-config)
      (gptel-make-preset 'kimi-with-mcp-mysql-hinihao-ai-dev
                         :description "kimi with mysql mcp"
                         :backend volcengine-config
                         :model 'kimi-k2-250905
                         :post (lambda () (when (fboundp 'gptel-mcp-connect)
                                            (gptel-mcp-connect '("mysql-hinihao-ai-dev")))))

      (gptel-make-preset 'kimi-with-mcp-mysql-hinihao-ai-prod
                         :description "kimi with mysql mcp"
                         :backend volcengine-config
                         :model 'kimi-k2-250905
                         :post (lambda () (when (fboundp 'gptel-mcp-connect)
                                            (gptel-mcp-connect '("mysql-hinihao-ai-prod"))))))

    (when (boundp 'openrouter-minimax-config)
      (gptel-make-preset 'openrouter-minimax
                         :description "OpenRouter Minimax Chat"
                         :backend openrouter-minimax-config
                         :model 'minimax-text-chat)))


  (require 'shr)
  (defvar shr-external-rendering-functions)

  
  (defvar url-http-response-status)

  (defun +gptel-url-retrieve (url)
    (message "Retrieving %s..." url)
    (let ((buffer (url-retrieve-synchronously url t t 20)))
      (unless buffer
        "Retrieving %s...failed" url)
      (with-current-buffer buffer
        (message "Retrieving %s...%s" url url-http-response-status)
        (when (>= url-http-response-status 400)
	  (error "HTTP Error %s: %s" url-http-response-status
	         (with-current-buffer buffer
		   (buffer-string)))))
      buffer))

  (defun +gptel-insert-link (dom)
    (shr-generic dom)
    (when-let* ((href (dom-attr dom 'href)))
      (or (string-match-p ".*duckduckgo\\.com.*" href)
          (string-match-p "\\`\\(/\\|:\\)" href)
          (shr-insert (format " (%s)" href)))))
  (defun +gptel-search-ddg (query)
    (let ((url (format "https://html.duckduckgo.com/html/?q=%s" query)))
      (with-current-buffer (+gptel-url-retrieve url)
        (goto-char (point-min))
        (forward-paragraph)
        (let ((dom (libxml-parse-html-region (point) (point-max))))
          (run-at-time 0 nil #'kill-buffer (current-buffer))
          (with-temp-buffer
            (let ((shr-external-rendering-functions '((a . +gptel-insert-link))))
              (shr-insert-document dom))
            (buffer-substring-no-properties (point-min) (point-max)))))))

  ;; Define custom tools
  (when (fboundp 'gptel-make-tool)
    (gptel-make-tool
     :name "read_url"
     :function (lambda (url)
                 (with-current-buffer (+gptel-url-retrieve url)
                   (goto-char (point-min))
                   (forward-paragraph)
                   (let ((dom (libxml-parse-html-region (point) (point-max))))
                     (run-at-time 0 nil #'kill-buffer (current-buffer))
                     (with-temp-buffer
                       (shr-insert-document dom)
                       (buffer-substring-no-properties (point-min) (point-max))))))
     :description "Fetch and read the contents of a URL"
     :args (list '(:name "url"
                   :type string
                   :description "The URL to read"))
     :category "web")

    (gptel-make-tool
     :name "search_web"
     :function #'+gptel-search-ddg
     :description "Perform a web search using the DuckDuckGo search engine"
     :args (list '(:name "query"
                   :type string
                   :description "The search query string.  When searching the web, one should always use English rather than their native language."))
     :category "web"))


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





(provide 'gptel)
