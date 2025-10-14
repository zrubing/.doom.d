;; -*- lexical-binding: t; -*-

(use-package! lsp-proxy
  :config
  ;; (setq lsp-proxy-log-level 3)
  (set-lookup-handlers! 'lsp-proxy-mode
    :definition '(lsp-proxy-find-definition :async t)
    :references '(lsp-proxy-find-references :async t)
    :implementations '(lsp-proxy-find-implementations :async t)
    :type-definition '(lsp-proxy-find-type-definition :async t)
    :documentation '(lsp-proxy-describe-thing-at-point :async t))

  ;; company
  (setq company-idle-delay 0)
  ;; If you encounter issues when typing Vue directives (e.g., v-), you can try setting it to 1. I'm not sure if it's a problem with Volar.
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-idle-delay 0)

  ;; corfu
  (setq corfu-auto-delay 0)
  (setq corfu-popupinfo-delay '(0.1 . 0.1))

  (defun company-box-icons--lsp-proxy (candidate)
    (-when-let* ((proxy-item (get-text-property 0 'lsp-proxy--item candidate))
                 (lsp-item (plist-get proxy-item :item))
                 (kind-num (plist-get lsp-item :kind)))
      (alist-get kind-num company-box-icons--lsp-alist)))

  (after! company-box

    (setq company-box-icons-functions
          (cons #'company-box-icons--lsp-proxy company-box-icons-functions))

    )

  ;; (add-hook 'vue-ts-mode-hook #'lsp-proxy-mode)

  )



(provide 'lsp-proxy)
