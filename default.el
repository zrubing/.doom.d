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


(after! diff-hl
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)


  )
