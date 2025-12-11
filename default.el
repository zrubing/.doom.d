;; lexical-binding: t; -*-

(setq shell-file-name "zsh")

(use-package! exec-path-from-shell
  :config

  (when (daemonp)
    (exec-path-from-shell-initialize))


  ;; 设置为fish，nix-config中在fish中设置了环境变量
  (setq exec-path-from-shell-shell-name "fish")
  (setq exec-path-from-shell-variables '("ANTHROPIC_API_KEY" "ANTHROPIC_BASE_URL") )
  (exec-path-from-shell-initialize))


(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'treesit-extra-load-path "~/.config/tree-sitter-libs")

(setq split-width-threshold 50)


(add-to-list 'auto-mode-alist '("\\.php\\'" . php-ts-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-ts-mode))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


(use-package! nov)

(after! nov
  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                             :height 2.0))
  (add-hook 'nov-mode-hook 'my-nov-font-setup)


  )


(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (php-mode . php-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (css-mode . css-ts-mode)
        (java-mode . java-ts-mode)
        (vue-mode . vue-ts-mode)
        (markdown-mode . markdown-ts-mode)
        (python-mode . python-ts-mode)))


(after! treemacs
  (setq treemacs-collapse-dirs 5)
  (treemacs-follow-mode t))



;; (after! tramp
;;   (add-to-list 'tramp-remote-path "~/.nix-profile/bin")
;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; 配置tramp远程路径时自动使用grep而不是ripgrep
(after! consult
  (defun +consult--ripgrep-maybe-fallback (args)
    "Use grep instead of ripgrep for tramp paths."
    (if (file-remote-p default-directory)
        (consult-grep args)
      (consult-ripgrep args)))
  ;; 重写默认的搜索函数
  (advice-add '+default/search-project :override #'+consult--ripgrep-maybe-fallback))

(use-package! telega
  :init
  (setq telega-server-libs-prefix "~/.local/share/tdlib"))


(after! envrc
  (envrc-global-mode)
  )


;; AI模型统一配置表 - 供gptel、aidermacs等模块共享使用
(setq +gptel-models
      '(
        (default-api-master-jsx
         :type openai
         :host "api.master-jsx.top"
         :endpoint "/v1/chat/completions"
         :stream t
         :protocol "https"
         :key-key "default.me.api.master-jsx.top"
         :models (grok-4-fast-non-reasoning grok-4-fast-reasoning gpt-5-codex))
        (microsoft-api-master-jsx
         :type openai
         :host "api.master-jsx.top"
         :endpoint "/v1/chat/completions"
         :stream t
         :protocol "https"
         :key-key "microsoft.me.api.master-jsx.top"
         :models (gpt-5.1))
        (volcengine-config
         :type openai
         :host "ark.cn-beijing.volces.com"
         :endpoint "/api/v3/chat/completions"
         :stream t
         :protocol "https"
         :key-key "work.console.volcengine.com"
         :models (kimi-k2-thinking-251104 deepseek-v3-2-251201))
        (moonshot-config
         :type openai
         :host "api.moonshot.cn"
         :endpoint "/v1/chat/completions"
         :stream t
         :protocol "https"
         :key-key "api.moonshot.cn"
         :models (kimi-k2-0905-preview kimi-k2-turbo-preview))
        (deepseek-config
         :type openai
         :host "api.deepseek.com"
         :endpoint "/v1/chat/completions"
         :stream t
         :protocol "https"
         :key-key "api.deepseek.com"
         :models (deepseek-chat deepseek-reasoner))
        (bigmodel-config
         :type openai
         :host "open.bigmodel.cn"
         :endpoint "/api/coding/paas/v4/chat/completions"
         :stream t
         :protocol "https"
         :key-key "xiaoqiang.open.bigmodel.cn"
         :models (GLM-4.6 GLM-4.5-Air))
        (openrouter-config
         :type openai
         :host "openrouter.ai"
         :endpoint "/api/v1/chat/completions"
         :stream t
         :protocol "https"
         :key-key "openrouter.ai"
         :models (minimax/minimax-m2))))


(after! diff-hl
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)


  )
