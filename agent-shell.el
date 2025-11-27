;;; agent-shell.el -*- lexical-binding: t; -*-


(use-package! agent-shell
  :init

  (require 'agent-shell)
  )


(after! agent-shell

  (when (getenv "ANTHROPIC_API_KEY")
    (setq agent-shell-anthropic-authentication
          (agent-shell-anthropic-make-authentication :api-key (getenv "ANTHROPIC_API_KEY")))

    (setq agent-shell-anthropic-claude-environment
          (agent-shell-make-environment-variables
           "ANTHROPIC_API_KEY" (getenv "ANTHROPIC_API_KEY")
           "ANTHROPIC_BASE_URL" "https://open.bigmodel.cn/api/anthropic")))


  
  )

(provide 'agent-shell)
