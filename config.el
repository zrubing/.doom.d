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
(setq doom-theme 'doom-one)

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



(use-package! counsel-etags)
(use-package! org-download)

(use-package! ejc-sql)

;; (use-package! treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

(use-package org-roam
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



(setq org-agenda-include-diary t)
(setq org-time-stamp-custom-formats '("<%Y-%m-%d %a %H:%M>"))

(setq org-agenda-files '("~/org-roam-dir"))
(setq org-agenda-diary-file "~/org-roam-dir/src/standard-diary") ;;2020-03-02 10:47:06
(setq diary-file "~/org-roam-dir/src/standard-diary")




(use-package! insert-translated-name
  :load-path "~/.config/emacs/.local/straight/repos/insert-translated-name")


(use-package! treemacs
  :init
  :config
  (setq treemacs-collapse-dirs 5)
  (treemacs-follow-mode t)
  )

;; (use-package! web-mode
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
;;   (setq web-mode-enable-auto-pairing nil)

;;   )
;; (use-package! vue-ts-mode
;;   :load-path "~/.config/emacs/.local/straight/repos/vue-ts-mode"
;;   :config
;;   (setq treesit-language-source-alist
;;         ''((vue "https://github.com/tree-sitter-grammars/tree-sitter-vue")
;;         (css "https://github.com/tree-sitter/tree-sitter-css")
;;         (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;         )
;;   )
;; )

(global-tree-sitter-mode)


(use-package! lsp-bridge
  :load-path "~/.config/emacs/.local/straight/repos/lsp-bridge"
  :init

  ;; (setq lsp-bridge-user-langserver-dir "~/.config/doom/lsp-bridge/langserver"
  ;;       lsp-bridge-user-multiserver-dir "~/.config/doom/lsp-bridge/multiserver")

  (setq lsp-bridge-enable-with-tramp nil)

  (setq lombok-path (substitute-in-file-name "$HOME/.local/share/javalib/lombok.jar"))
  (setq lsp-bridge-jdtls-jvm-args (list (format "%s%s" "-javaagent:" lombok-path)))
  ;; (setq lsp-bridge-multi-lang-server-extension-list
  ;;         '((("css" "less" "scss") . "css_emmet")
  ;;         (("html") . "html_emmet")
  ;;         (("vue") . "volar_emmet")))

  ;; (setq lsp-bridge-single-lang-server-extension-list
  ;;       '(
  ;;         (("vue") . "volar")
  ;;         (("wxml") . "wxml-language-server")
  ;;         (("html") . "vscode-html-language-server")
  ;;         (("astro") . "astro-ls")
  ;;         (("typ") . "typst-lsp")
  ;;         ))


  :custom
  (lsp-bridge-code-action-enable-popup-menu nil)
  (lsp-bridge-enable-inlay-hint t)

  (setq-local lsp-bridge-get-project-path-by-filepath 'projectile-project-root)

  :config
  (require 'yasnippet)
  (yas-global-mode 1)


  (require 'lsp-bridge-jdtls)
  (add-to-list '+lookup-definition-functions #'lsp-bridge-find-def)
  (add-to-list '+lookup-implementations-functions #'lsp-bridge-find-impl)
  (add-to-list '+lookup-references-functions #'lsp-bridge-find-references)
  (add-to-list '+lookup-documentation-functions #'lsp-bridge-popup-documentation)
  (add-to-list '+lookup-type-definition-functions #'lsp-bridge-find-type-def)
  (define-key evil-normal-state-map "ga" #'lsp-bridge-code-action)
  ;;(define-key lsp-bridge-mode-map (kbd "SPC c x") 'lsp-bridge-diagnostic)

  (global-lsp-bridge-mode)

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


  ;;(setq-hook! 'java-mode-hook +format-with 'lsp-bridge-code-format)

  )

;; (use-package! eaf
;;   :load-path "~/.config/emacs/.local/straight/repos/emacs-application-framework"
;;   )

;; (use-package! eaf-pdf-viewer
;;   :load-path "~/.config/emacs/.local/straight/repos/eaf-pdf-viewer"
;;   :custom
;;   (setq eaf-pdf-dark-mode nil)

;;   )


;; (use-package! eaf-browser
;;   :load-path "~/.config/emacs/.local/straight/repos/eaf-browser"
;;   :custom
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser)
;;   :config
;;   (defalias 'browse-web #'eaf-open-browser)
;;   (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki


;; (require 'eaf-image-viewer)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-org-previewer)
;; (require 'eaf-mindmap)
;; (require 'eaf-file-browser)
;; (require 'eaf-file-sender)
;; (require 'eaf-airshare)
;; (require 'eaf-jupyter)
;; (require 'eaf-markmap)


(add-to-list 'load-path "~/.config/emacs/.local/straight/repos/mind-wave")
(require 'mind-wave)
;; (setq mind-wave-enable-log t)
;; (add-to-list 'load-path "~/.config/emacs/.local/straight/repos/Bard.el")
;; (require 'bard)
;;(setq bard-http-proxy "http://127.0.0.1:1080") ;; You may need to set up a proxy if you are not in a region or country Google Bard allowed.



;; https://github.com/emacs-eaf/emacs-application-framework/wiki/Evil
(add-to-list 'load-path "~/.config/emacs/.local/straight/repos/emacs-application-framework/extension")
;;(require 'eaf-evil)


;; (setq lsp-java-maven-download-source t)
;;(setq lsp-java-format-settings-url "~/.vscode/eclipse-java-google-style.xml")
;;(setq lsp-java-vmargs '("-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "-javaagent:/Users/admin/.m2/repository/org/projectlombok/lombok/1.18.26/lombok-1.18.26.jar"))
;;(add-hook 'java-mode-hook #'lsp)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))

(setq org-attach-store-link-p 'attached)
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.docx\\'" . "open -a /Applications/wpsoffice.app %s")))

(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.pdf\\'" . "zathura %s")))


;; (use-package! dap-mode
;;   :config
;;   (dap-register-debug-template "ysx-rice"
;;                              (list :type "java"
;;                                    :request "launch"
;;                                    :args ""
;;                                    :vmArgs "-ea -Dmyapp.instance.name=myapp_1"
;;                                    :projectName "myapp"
;;                                    :mainClass "com.domain.AppRunner"
;;                                    :env '(("DEV" . "1"))))
;;   )


(use-package! kubernetes                ;
  :ensure t
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

;; (use-package! dired-sidebar
;;   :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
;;   :ensure t
;;   :commands (dired-sidebar-toggle-sidebar)
;;   :init
;;   (add-hook 'dired-sidebar-mode-hook
;;             (lambda ()
;;               (unless (file-remote-p default-directory)
;;                 (auto-revert-mode))))
;;   :config
;;   (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
;;   (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

;;   (setq dired-sidebar-subtree-line-prefix "__")
;;   (setq dired-sidebar-theme 'nerd)
;;   (setq dired-sidebar-use-term-integration t)
;;   (setq dired-sidebar-use-custom-font t))

;; (use-package! kubernetes-evil
;;   :ensure t
;;   :after kubernetes)

;; alias kn='f() { [ "$1" ] && kubectl config set-context --current --namespace $1 || kubectl config view --minify | grep namespace | cut -d" " -f6 ; } ; f'
;;
;; (setq shell-file-name "zsh")

;;(setq shell-command-switch "-lc")

;; (defun kubernetes-switch-namespace (item)
;;   (interactive
;;    (list (completing-read "Choose an item: " '("sdsx-farmland" "hn-xjj-project" "ysx-rice" "homestead-product-hbdz"))))
;;     (shell-command (concat "kn " item))
;;   )


;; (setq telega-proxies
;;       (list
;;        '(:server "127.0.0.1" :port 1087 :enable t
;;                  :type (:@type "proxyTypeSocks5" :username nil :password nil))
;;        ))
(setq meow-use-clipboard t)
(setq immersive-translate-backend 'trans)

(use-package! rime
  :init
  (setq rime-user-data-dir "~/.config/rime")
  (setq rime-emacs-module-header-root "~/.nix-profile/include"
        rime-librime-root "~/.nix-profile"
        rime-share-data-dir "~/.local/share/rime-data/share/rime-data")
  :custom
  (default-input-method "rime"))

(recentf-mode 1)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


(add-to-list 'doom-symbol-fallback-font-families "Symbols Nerd Font")

(set-fontset-font
 t
 'symbol
 (cond
  ((eq system-type 'windows-nt)
   (cond
    ((member "Segoe UI Symbol" (font-family-list)) "Segoe UI Symbol")))
  ((eq system-type 'darwin)
   (cond
    ((member "Apple Symbols" (font-family-list)) "Apple Symbols")))
  ((eq system-type 'gnu/linux)
   (cond
    ((member "Symbola" (font-family-list)) "Symbola")))))

(use-package!
    minibuffer-modifier-keys
  :after
  (minibuffer-modifier-keys-setup t))

(use-package! dape

  :load-path "~/.config/emacs/.local/straight/repos/dape"
  ;; To use window configuration like gud (gdb-mi)
  :init
  (setq dape-buffer-window-arrangment 'gud)
  :config
  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangment 'right)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; By default dape uses gdb keybinding prefix
  (setq dape-key-prefix "\C-x\C-a")

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks
  ;;           (defun dape--save-on-start ()
  ;;             (save-some-buffers t t)))

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root)
  )


;; (use-package! dired-rsync
;;   :bind (:map dired-mode-map
;;               ("C-c C-r" . dired-rsync))

;; (use-package dired-rsync-transient
;;   :bind (:map dired-mode-map
;;               ("C-c C-x" . dired-rsync-transient)))

;; (use-package! window-numbering
;;   :init
;;   :hook (after-init . window-numbering-mode))



(use-package! devdocs
  :init
  (global-set-key (kbd "C-h D") 'devdocs-lookup))


(defun is-wayland ()
  "check if running under wayland"
  ;; (or (getenv "WAYLAND_DISPLAY")
  ;;     (string-equal (getenv "XDG_SESSION_TYPE") "wayland"))
  (cl-case window-system
        (x nil)
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


;; (setq projectile-mode-line "Projectile")
(setq tramp-verbose 6)

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(setq tramp-verbose 1)

(setq docker-open-hook '())

;;(use-package! journalctl-mode)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(load! "ejc-sql-conf")

(load! "gptel")

(load! "minuet-custom")

(load! "aider")
