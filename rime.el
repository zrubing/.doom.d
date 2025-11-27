;; lexical-binding: t;

(use-package! rime
  :defer t
  :init
  (setq rime-user-data-dir "~/.config/rime"
        rime-share-data-dir "~/.local/share/rime-data/share/rime-data")
  (setq rime-emacs-module-header-root "~/.local/share/emacs/librime/include"
        rime-librime-root "~/.local/share/emacs/librime")
  ;;       rime-librime-root "~/.nix-profile"
  ;;       rime-share-data-dir "~/.local/share/rime-data/share/rime-data")
  :custom
  (default-input-method "rime")
  :bind
  (:map rime-mode-map
        ("C-`" . 'rime-send-keybinding))
  )
