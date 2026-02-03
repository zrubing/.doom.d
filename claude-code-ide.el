;; -*- lexical-binding: t -*-

(use-package! claude-code-ide

  :bind (("C-c C-'" . claude-code-ide-menu))
  :config
  (after! claude-code-ide
    (claude-code-ide-emacs-tools-setup)
    (setq claude-code-ide-terminal-backend 'vterm)
    (setq claude-code-ide-cli-extra-flags "--dangerously-skip-permissions")
    (setq claude-code-ide-prevent-reflow-glitch nil)
    )
  )

(provide 'claude-code-ide)
