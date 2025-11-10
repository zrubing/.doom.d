;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "+functions")

(add-to-list 'treesit-extra-load-path "~/.config/tree-sitter-libs")

(setq split-width-threshold 50)

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

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 11.0 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans Mono" :size 13))
;;(setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 14))
;;(setq doom-font (font-spec :family "Fira Mono" :size 11))
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'alabaster-dark)

;; Add custom theme path for Alabaster Dark
(add-to-list 'custom-theme-load-path "~/.doom.d/themes/")

;; Ensure theme loads correctly with error handling
(defun my/load-alabaster-dark-theme ()
  "Load alabaster-dark theme with error handling."
  (condition-case err
      (load-theme 'alabaster-dark t)
    (error
     (message "Failed to load alabaster-dark theme: %s" (error-message-string err))
     (message "Falling back to doom-one theme")
     (load-theme 'doom-one t))))

;; Override theme loading to use our custom function
(advice-add 'doom-init-theme-h :override #'my/load-alabaster-dark-theme)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org-roam-dir")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; (setq url-proxy-services
;;       '(("http"  . "localhost:1087")
;;     	("https" . "localhost:1087")))


(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq tramp-verbose 10)
;;(setq directory-abbrev-alist '(("^/ktg-mes" . "/ssh:ktg-mes:~")))
(setq vterm-max-scrollback 100000)
(setq vterm-kill-buffer-on-exit t)
(setq vterm-buffer-name-string "vterm %s")



;; (use-package! counsel-etags)
;; (use-package! org-download)


;; (use-package! treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

(use-package! org-roam
  :defer t
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/org-roam-dir"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))



;; (setq org-agenda-include-diary t)
;; (setq org-time-stamp-custom-formats '("<%Y-%m-%d %a %H:%M>"))

(setq org-agenda-files (directory-files-recursively "~/org-roam-dir/daily/" "^2025-.*\\.org$"))

;; (setq org-agenda-diary-file "~/org-roam-dir/src/standard-diary") ;;2020-03-02 10:47:06
;; (setq diary-file "~/org-roam-dir/src/standard-diary")



(use-package! treemacs
  :defer t
  :init
  :config
  (setq treemacs-collapse-dirs 5)
  (treemacs-follow-mode t)
  )
;;(global-tree-sitter-mode)







;; (setq lsp-java-maven-download-source t)
;;(setq lsp-java-format-settings-url "~/.vscode/eclipse-java-google-style.xml")
;;(setq lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-javaagent:/Users/admin/.m2/repository/org/projectlombok/lombok/1.18.26/lombok-1.18.26.jar"))
;;(add-hook 'java-mode-hook #'lsp)


;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((restclient . t)))

;; (setq org-attach-store-link-p 'attached)
;; (setq org-file-apps
;;       '((auto-mode . emacs)
;;         ("\\.docx\\'" . "open -a /Applications/wpsoffice.app %s")))

(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.docx\\'" . "libreoffice %s")))

;; (setq org-file-apps
;;       '((auto-mode . emacs)
;;         ("\\.pdf\\'" . "zathura %s")))


;; (use-package! kubernetes                ;
;;   :ensure t
;;   :defer t
;;   :commands (kubernetes-overview)
;;   :config
;;   (setq kubernetes-poll-frequency 3600
;;         kubernetes-redraw-frequency 3600))

;; (setq meow-use-clipboard t)
;; (setq immersive-translate-backend 'trans)

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

(use-package! telega
  :init
  (setq telega-server-libs-prefix "~/.local/share/tdlib"))

;; (recentf-mode 1)
;; (global-set-key "\C-x\ \C-r" 'recentf-open-files)


;; (add-to-list 'doom-symbol-fallback-font-families "Symbols Nerd Font")

;; (set-fontset-font
;;  t
;;  'symbol
;;  (cond
;;   ((eq system-type 'windows-nt)
;;    (cond
;;     ((member "Segoe UI Symbol" (font-family-list)) "Segoe UI Symbol")))
;;   ((eq system-type 'darwin)
;;    (cond
;;     ((member "Apple Symbols" (font-family-list)) "Apple Symbols")))
;;   ((eq system-type 'gnu/linux)
;;    (cond
;;     ((member "Symbola" (font-family-list)) "Symbola")))))

;; (use-package!
;;     minibuffer-modifier-keys
;;   :after
;;   (minibuffer-modifier-keys-setup t))


;; ;; (use-package! dired-rsync
;; ;;   :bind (:map dired-mode-map
;; ;;               ("C-c C-r" . dired-rsync))

;; ;; (use-package dired-rsync-transient
;; ;;   :bind (:map dired-mode-map
;; ;;               ("C-c C-x" . dired-rsync-transient)))

;; ;; (use-package! window-numbering
;; ;;   :init
;; ;;   :hook (after-init . window-numbering-mode))



(defun is-wayland ()
  "check if running under wayland"
  ;; (or (getenv "WAYLAND_DISPLAY")
  ;;     (string-equal (getenv "XDG_SESSION_TYPE") "wayland"))
  (cl-case window-system
    (x t)
    (otherwise t)))

;; (if (is-wayland)

;;     (progn
;;       ;; credit: yorickvP on Github
;;       (setq wl-copy-process nil)
;;       (defun wl-copy (text)
;;         (setq wl-copy-process (make-process :name "wl-copy"
;;                                             :buffer nil
;;                                             :command '("wl-copy" "-f" "-n")
;;                                             :connection-type 'pipe
;;                                             :noquery t))
;;         (process-send-string wl-copy-process text)
;;         (process-send-eof wl-copy-process))
;;       (defun wl-paste ()
;;         (if (and wl-copy-process (process-live-p wl-copy-process))
;;             nil ; should return nil if we're the current paste owner
;;           (shell-command-to-string "wl-paste -n | tr -d \r")))
;;       (setq interprogram-cut-function 'wl-copy)
;;       (setq interprogram-paste-function 'wl-paste)))


;; ;; (setq projectile-mode-line "Projectile")
;; (setq tramp-verbose 6)

;; (setq remote-file-name-inhibit-cache nil)
;; (setq vc-ignore-dir-regexp
;;       (format "%s\\|%s"
;;               vc-ignore-dir-regexp
;;               tramp-file-name-regexp))

;; (setq tramp-verbose 1)

;; (setq docker-open-hook '())

;; ;;(use-package! journalctl-mode)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package! ultra-scroll
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package! apheleia
  :after lsp-bridge
  :config
  ;; don't mess up with lsp-mode
  (setq +format-with-lsp nil)
  (setq apheleia-remote-algorithm 'remote))

(after! tramp
  (add-to-list 'tramp-remote-path "~/.nix-profile/bin")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package! topsy
  :after lsp-bridge
  :config
  ;; display a bar to remind editing remote file
  (setcdr (assoc nil topsy-mode-functions)
          (lambda ()
            (when (lsp-bridge-is-remote-file) "[LBR] REMOTE FILE")))

  ;; do not activate when the current major mode is org-mode
  (add-hook 'lsp-bridge-mode-hook (lambda ()
                                    (unless (derived-mode-p 'org-mode 'vue-ts-mode)
                                      (topsy-mode 1))))
  )

;; (after! flycheck
;;   (add-to-list 'flycheck-global-modes 'not shell-mode))

(after! vterm

  ;; rime输入法
  (add-hook 'vterm-mode-hook
            (lambda ()
              (define-key vterm-mode-map (kbd "C-\\") nil)))

  (defun my/set-vterm-shell ()
    (when (string-prefix-p "/docker:" (file-remote-p default-directory))
      (when (eq major-mode 'vterm-mode)
        (let ((shell (if (string-prefix-p "/docker:" (file-remote-p default-directory))
                         "/bin/bash"
                       (or (getenv "SHELL") "/bin/bash"))))
          (vterm-send-string (format "exec %s\n" shell))
          (vterm-send-string "clear\n")))))

  (add-hook 'vterm-mode-hook #'my/set-vterm-shell))


(use-package! eat
  :config
  (setq eat-enable-directory-tracking t)
  (setq eat-shell "zsh"))

(after! dape
  (setq dape-debug t)
  )

(after! envrc
  (envrc-global-mode)
  )

(with-eval-after-load 'vterm
  (setq vterm-shell "fish")
  ;; 解除 M-0 到 M-9 的绑定
  (dolist (key '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
    (define-key vterm-mode-map (kbd (concat "M-" key)) nil)))

;;(use-package! vue-mode)

(load! "ejc-sql-conf")

(load! "gptel")
(load! "aider")

;;(load! "aidermacs")

(load! "emigo")

(load! "lsp-bridge")
(load! "lsp-proxy")


(after! lsp-bridge

  )

(load! "dape")


;; (load! "minuet-custom")

(use-package! vue-ts-mode
  :config
  ;; setup formatter to be used by `SPC c f`
  (after! apheleia
    (setf (alist-get 'vue-ts-mode apheleia-mode-alist) '(prettier)))
  )

(load! "mcp")

(load! "claude-code-ide")

;; (use-package! go-translate
;;   :config
;;   (setq gt-langs '(en zh))
;;   (setq gt-default-translator (gt-translator :engines (gt-youdao-dict-engine)))

;;   )


;; accept completion from copilot and fallback to company
;; (use-package! copilot
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . 'copilot-accept-completion)
;;               ("TAB" . 'copilot-accept-completion)
;;               ("C-TAB" . 'copilot-accept-completion-by-word)
;;               ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


;; (use-package! direnv
;;  :config
;;  (direnv-mode))

(load! "eca")

(load! "agent-shell")

;; (add-hook 'buffer-list-update-hook (lambda ()
;;                                      (unless (active-minibuffer-window)
;;                                        (hide-mode-line-mode))))
