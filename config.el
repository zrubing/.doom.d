;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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
(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 13.0 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans Mono" :size 14))
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
(setq doom-theme 'doom-monokai-pro)

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

;; (use-package! org-roam
;;   :defer t
;;   :ensure t
;;   :init
;;   (setq org-roam-v2-ack t)
;;   :custom
;;   (org-roam-directory (file-truename "~/org-roam-dir"))
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n g" . org-roam-graph)
;;          ("C-c n i" . org-roam-node-insert)
;;          ("C-c n c" . org-roam-capture)
;;          ;; Dailies
;;          ("C-c n j" . org-roam-dailies-capture-today))
;;   :config
;;   ;; If you're using a vertical completion framework, you might want a more informative completion interface
;;   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;;   (org-roam-db-autosync-mode)
;;   ;; If using org-roam-protocol
;;   (require 'org-roam-protocol))



;; (setq org-agenda-include-diary t)
;; (setq org-time-stamp-custom-formats '("<%Y-%m-%d %a %H:%M>"))

;; (setq org-agenda-files '("~/org-roam-dir"))
;; (setq org-agenda-diary-file "~/org-roam-dir/src/standard-diary") ;;2020-03-02 10:47:06
;; (setq diary-file "~/org-roam-dir/src/standard-diary")



(use-package! treemacs
  :defer t
  :init
  :config
  (setq treemacs-collapse-dirs 5)
  (treemacs-follow-mode t)
  )
(global-tree-sitter-mode)







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
  ;; (setq rime-emacs-module-header-root "~/.nix-profile/include"
  ;;       rime-librime-root "~/.nix-profile"
  ;;       rime-share-data-dir "~/.local/share/rime-data/share/rime-data")
  :custom
  (default-input-method "rime"))

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

;; (use-package! dape

;;   :defer t
;;   :load-path "~/.config/emacs/.local/straight/repos/dape"
;;   ;; To use window configuration like gud (gdb-mi)
;;   :init
;;   (setq dape-buffer-window-arrangment 'gud)
;;   :config
;;   ;; Info buffers to the right
;;   ;; (setq dape-buffer-window-arrangment 'right)

;;   ;; To not display info and/or buffers on startup
;;   ;; (remove-hook 'dape-on-start-hooks 'dape-info)
;;   ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

;;   ;; To display info and/or repl buffers on stopped
;;   (add-hook 'dape-on-stopped-hooks 'dape-info)
;;   (add-hook 'dape-on-stopped-hooks 'dape-repl)

;;   ;; By default dape uses gdb keybinding prefix
;;   (setq dape-key-prefix "\C-x\C-a")

;;   ;; Kill compile buffer on build success
;;   ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

;;   ;; Save buffers on startup, useful for interpreted languages
;;   ;; (add-hook 'dape-on-start-hooks
;;   ;;           (defun dape--save-on-start ()
;;   ;;             (save-some-buffers t t)))

;;   ;; Projectile users
;;   (setq dape-cwd-fn 'projectile-project-root)
;;   )


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

(if (is-wayland)

    (progn
      ;; credit: yorickvP on Github
      (setq wl-copy-process nil)
      (defun wl-copy (text)
        (setq wl-copy-process (make-process :name "wl-copy"
                                            :buffer nil
                                            :command '("wl-copy" "-f" "-n")
                                            :connection-type 'pipe
                                            :noquery t))
        (process-send-string wl-copy-process text)
        (process-send-eof wl-copy-process))
      (defun wl-paste ()
        (if (and wl-copy-process (process-live-p wl-copy-process))
            nil ; should return nil if we're the current paste owner
          (shell-command-to-string "wl-paste -n | tr -d \r")))
      (setq interprogram-cut-function 'wl-copy)
      (setq interprogram-paste-function 'wl-paste)))


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

;; (use-package org
;;   :mode ("\\.org\\'" . org-mode)
;;   :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(load! "ejc-sql-conf")

(load! "gptel")

;;(load! "minuet-custom")

(load! "aider")

(load! "lsp-bridge")
