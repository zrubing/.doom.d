;;; agent-shell.el -*- lexical-binding: t; -*-


(use-package! agent-shell
  :init
  (require 'agent-shell)
  :config
  


  ;; (let ((api-key (funcall (plist-get (car (auth-source-search :host "kimi.coding" :user "apikey")) :secret))))
  ;;   (setenv "KIMI_API_KEY" (encode-coding-string api-key 'utf-8)))

  ;; (let ((api-key (funcall (plist-get (car (auth-source-search :host "kimi.coding" :user "url")) :secret))))
  ;;   (setenv "KIMI_URL" (encode-coding-string api-key 'utf-8)))


  ;; (setq agent-shell-anthropic-authentication
  ;;       (agent-shell-anthropic-make-authentication :api-key (getenv "KIMI_API_KEY")))

  ;; (setq agent-shell-anthropic-claude-environment
  ;;       (agent-shell-make-environment-variables
  ;;        "ANTHROPIC_API_KEY" (getenv "KIMI_API_KEY")
  ;;        "ANTHROPIC_MODEL" "kimi-for-coding"
  ;;        "ANTHROPIC_SMALL_FAST_MODEL" "kimi-for-coding"
  ;;        "ANTHROPIC_BASE_URL" (getenv "KIMI_URL")))
  ;; )

  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :api-key (getenv "ANTHROPIC_API_KEY")))

  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         "ANTHROPIC_API_KEY" (getenv "ANTHROPIC_API_KEY")
         "ANTHROPIC_BASE_URL" "https://open.bigmodel.cn/api/anthropic"))
  )


(map! :leader
      (:prefix ("o a" . "agent-shell")
       :desc "Start Claude Code agent" "c" #'agent-shell-anthropic-start-claude-code
       )
      )

(provide 'agent-shell)
