;;; agent-shell.el -*- lexical-binding: t; -*-



(require 'acp)
(require 'agent-shell)


(provide 'agent-shell)

(setq agent-shell-anthropic-claude-environment
      (agent-shell-make-environment-variables
       "ANTHROPIC_API_BASE_URL" (getenv "ANTHROPIC_BASE_URL")
       "ANTHROPIC_API_KEY" (getenv "ANTHROPIC_API_KEY")
      ))
(setopt agent-shell-file-completion-enabled t)
