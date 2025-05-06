;;; +functions.el -*- lexical-binding: t; -*-


(defun my/enable-global-lsp-bridge-mode ()
  "Add custom lsp-bridge language server configurations and enable lsp-bridge-mode"
  (dolist (hook (append lsp-bridge-default-mode-hooks acm-backend-capf-mode-hooks))
    (add-hook hook (lambda ()
                     (setq confdir (or (let ((local-conf (expand-file-name "lsp-bridge-config")))
                                         (when (file-directory-p local-conf) local-conf))
                                       "~/.lsp"))
                     ;; check if a directory named ".lsp" exists in the projects root
                     (when-let (proot (projectile-project-root))
                       (let ((d (concat proot ".lsp/")))
                         (when (file-directory-p d) (setq confdir d))))
                     (when (file-directory-p confdir)
                       (setq-local lsp-bridge-user-langserver-dir (concat confdir "/langserver")
                                   lsp-bridge-user-multiserver-dir (concat confdir "/multiserver")))
                     (lsp-bridge-mode 1)))))

