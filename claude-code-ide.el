;; -*- lexical-binding: t -*-

(use-package! claude-code-ide

  :bind (("C-c C-'" . claude-code-ide-menu))
  :config
  (after! claude-code-ide
    (claude-code-ide-emacs-tools-setup)
    (setq claude-code-ide-terminal-backend 'eat)
    )
  )

(provide 'claude-code-ide)
