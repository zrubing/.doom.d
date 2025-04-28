;; -*- lexical-binding: t; -*-


(use-package! lsp-bridge
  :init
  ;;  :defer t

  ;; (setq lsp-bridge-enable-with-tramp nil)
  ;; :custom
  ;; (lsp-bridge-code-action-enable-popup-menu nil)

  ;; (setq-local lsp-bridge-get-project-path-by-filepath 'projectile-project-root)

  (require 'lsp-bridge-jdtls)
  :config
  (setq lsp-bridge-enable-inlay-hint t)
  ;;(setq lsp-bridge-enable-debug t)
  ;;(global-lsp-bridge-mode)


  (setq lombok-path (substitute-in-file-name "$HOME/.local/share/javalib/lombok.jar"))
  (setq tsdk-path (substitute-in-file-name "$HOME/.config/lsp-bridge-lib/typescript-lib"))
  (setq lsp-bridge-jdtls-jvm-args (list (format "%s%s" "-javaagent:" lombok-path)))
  (setq lsp-bridge-tsdk-path tsdk-path)

  ;;(setq lsp-bridge-php-lsp-server 'phpactor)


  ;; (require 'lsp-bridge-jdtls)
  (add-to-list '+lookup-definition-functions #'lsp-bridge-find-def)
  (add-to-list '+lookup-implementations-functions #'lsp-bridge-find-impl)
  (add-to-list '+lookup-references-functions #'lsp-bridge-find-references)
  (add-to-list '+lookup-documentation-functions #'lsp-bridge-popup-documentation)
  (add-to-list '+lookup-type-definition-functions #'lsp-bridge-find-type-def)
  (define-key evil-normal-state-map "ga" #'lsp-bridge-code-action)
  ;; ;;(define-key lsp-bridge-mode-map (kbd "SPC c x") 'lsp-bridge-diagnostic)

  ;; (global-lsp-bridge-mode)

  (define-key acm-mode-map (kbd "M-k") 'acm-doc-scroll-down)
  (define-key acm-mode-map (kbd "M-j") 'acm-doc-scroll-up)
  (define-key lsp-bridge-mode-map (kbd "M-k") 'lsp-bridge-popup-documentation-scroll-down)
  (define-key lsp-bridge-mode-map (kbd "M-j") 'lsp-bridge-popup-documentation-scroll-up)
  (add-hook 'lsp-bridge-ref-mode-hook 'evil-emacs-state)


  (defadvice load (after give-my-keybindings-priority)
    "Try to ensure that my keybindings always have priority."
    (when (not (eq (car (car minor-mode-map-alist)) 'acm-mode))
      (let ((mykeys (assq 'acm-mode minor-mode-map-alist)))
        (assq-delete-all 'acm-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
  (ad-activate 'load)


  (setq-hook! 'java-ts-mode-hook +format-with 'lsp-bridge-code-format)


  (my/enable-global-lsp-bridge-mode)

  )


(provide 'lsp-bridge)
