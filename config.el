;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))


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
(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 13 :weight 'semi-light)
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


(use-package! treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

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

(use-package! web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (setq web-mode-enable-auto-pairing nil)
  )

;;   )
;; (use-package! vue-ts-mode
;;   :load-path "~/.config/emacs/.local/straight/repos/vue-ts-mode"
;; )

;; (global-tree-sitter-mode)


;; (use-package! lsp-bridge
;;   :load-path "~/.config/emacs/.local/straight/repos/lsp-bridge"
;;   :init

;;   ;; (setq lsp-bridge-user-langserver-dir "~/.config/doom/lsp-bridge/langserver"
;;   ;;       lsp-bridge-user-multiserver-dir "~/.config/doom/lsp-bridge/multiserver")

;;   (setq lsp-bridge-enable-with-tramp nil)

;;   (setq lombok-path (substitute-in-file-name "$HOME/.m2/repository/org/projectlombok/lombok/1.18.30/lombok-1.18.30.jar"))
;;   (setq lsp-bridge-jdtls-jvm-args (list (format "%s%s" "-javaagent:" lombok-path)))
;;   ;; (setq lsp-bridge-multi-lang-server-extension-list
;;   ;;         '((("css" "less" "scss") . "css_emmet")
;;   ;;         (("html") . "html_emmet")
;;   ;;         (("vue") . "volar_emmet")))

;;   ;; (setq lsp-bridge-single-lang-server-extension-list
;;   ;;       '(
;;   ;;         (("vue") . "volar")
;;   ;;         (("wxml") . "wxml-language-server")
;;   ;;         (("html") . "vscode-html-language-server")
;;   ;;         (("astro") . "astro-ls")
;;   ;;         (("typ") . "typst-lsp")
;;   ;;         ))


;;   :custom
;;   (lsp-bridge-code-action-enable-popup-menu nil)

;;   (setq-local lsp-bridge-get-project-path-by-filepath 'projectile-project-root)

;;   :config
;;   (require 'yasnippet)
;;   (yas-global-mode 1)


;;   (require 'lsp-bridge-jdtls)
;;   (add-to-list '+lookup-definition-functions #'lsp-bridge-find-def)
;;   (add-to-list '+lookup-implementations-functions #'lsp-bridge-find-impl)
;;   (add-to-list '+lookup-references-functions #'lsp-bridge-find-references)
;;   (add-to-list '+lookup-documentation-functions #'lsp-bridge-popup-documentation)
;;   (add-to-list '+lookup-type-definition-functions #'lsp-bridge-find-type-def)
;;   (define-key evil-normal-state-map "ga" #'lsp-bridge-code-action)
;;   ;;(define-key lsp-bridge-mode-map (kbd "SPC c x") 'lsp-bridge-diagnostic)

;;   (global-lsp-bridge-mode)

;;   (define-key acm-mode-map (kbd "M-k") 'acm-doc-scroll-down)
;;   (define-key acm-mode-map (kbd "M-j") 'acm-doc-scroll-up)
;;   (define-key lsp-bridge-mode-map (kbd "M-k") 'lsp-bridge-popup-documentation-scroll-down)
;;   (define-key lsp-bridge-mode-map (kbd "M-j") 'lsp-bridge-popup-documentation-scroll-up)
;;   (add-hook 'lsp-bridge-ref-mode-hook 'evil-emacs-state)


;;   (defadvice load (after give-my-keybindings-priority)
;;     "Try to ensure that my keybindings always have priority."
;;     (when (not (eq (car (car minor-mode-map-alist)) 'acm-mode))
;;       (let ((mykeys (assq 'acm-mode minor-mode-map-alist)))
;;         (assq-delete-all 'acm-mode minor-mode-map-alist)
;;         (add-to-list 'minor-mode-map-alist mykeys))))
;;   (ad-activate 'load)


;;   ;;(setq-hook! 'java-mode-hook +format-with 'lsp-bridge-code-format)

;;   )

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
;; (setq org-file-apps
;;       '((auto-mode . emacs)
;;         ("\\.docx\\'" . "open -a /Applications/wpsoffice.app %s")))

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
(setq shell-file-name "zsh")

;; devcontainer
(add-to-list 'tramp-remote-path "/root/.local/bin")



(setq! enable-remote-dir-locals t)


(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))
(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (my-reload-dir-locals-for-current-buffer))))))

(add-hook 'emacs-lisp-mode-hook
          (defun enable-autoreload-for-dir-locals ()
            (when (and (buffer-file-name)
                       (equal dir-locals-file
                              (file-name-nondirectory (buffer-file-name))))
              (add-hook 'after-save-hook
                        'my-reload-dir-locals-for-all-buffer-in-this-directory
                        nil t))))

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


;; credit: yorickvP on Github
;; (setq wl-copy-process nil)
;; (defun wl-copy (text)
;;   (setq wl-copy-process (make-process :name "wl-copy"
;;                                       :buffer nil
;;                                       :command '("wl-copy" "-f" "-n")
;;                                       :connection-type 'pipe
;;                                       :noquery t))
;;   (process-send-string wl-copy-process text)
;;   (process-send-eof wl-copy-process))
;; (defun wl-paste ()
;;   (if (and wl-copy-process (process-live-p wl-copy-process))
;;       nil ; should return nil if we're the current paste owner
;;     (shell-command-to-string "wl-paste -n | tr -d \r")))
;; (setq interprogram-cut-function 'wl-copy)
;; (setq interprogram-paste-function 'wl-paste)


;; (setq projectile-mode-line "Projectile")
(setq tramp-verbose 6)

;; (advice-add 'projectile-project-root :before-while
;;   (lambda (&optional dir)
;;     (not (file-remote-p (or dir default-directory)))))

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(setq tramp-verbose 1)

(global-pangu-spacing-mode 1)

(use-package! eglot-booster
  :after eglot
  :config
  (eglot-booster-mode)
  (corfu-mode))

(setq docker-open-hook '())

;;(use-package! journalctl-mode)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(load! "ejc-sql-conf")
(load! "org-publish-conf")


(use-package! eglot-java
  :config
  (add-hook 'java-mode-hook 'eglot-java-mode)
  (add-hook 'java-ts-mode-hook 'eglot-java-mode)
  (with-eval-after-load 'eglot-java
    (define-key eglot-java-mode-map (kbd "C-c l n") #'eglot-java-file-new)
    (define-key eglot-java-mode-map (kbd "C-c l x") #'eglot-java-run-main)
    (define-key eglot-java-mode-map (kbd "C-c l t") #'eglot-java-run-test)
    (define-key eglot-java-mode-map (kbd "C-c l N") #'eglot-java-project-new)
    (define-key eglot-java-mode-map (kbd "C-c l T") #'eglot-java-project-build-task)
    (define-key eglot-java-mode-map (kbd "C-c l R") #'eglot-java-project-build-refresh))
  )

(use-package! eglot
  :config
  :hook (((js-ts-mode json-ts-mode yaml-ts-mode typescript-ts-mode java-ts-mode mhtml-mode css-ts-mode vue-ts-mode) . eglot-ensure))
  :preface
  (defun vue-eglot-init-options ()
    (let ((tsdk-path (expand-file-name
                      "lib"
                      (string-trim-right (shell-command-to-string "npm list --global --parseable typescript | head -n1")))))
      `(:typescript (:tsdk ,tsdk-path
                     :languageFeatures (:completion
                                        (:defaultTagNameCase "both"
    					 :defaultAttrNameCase "kebabCase"
    					 :getDocumentNameCasesRequest nil
    					 :getDocumentSelectionRequest nil)
                                        :diagnostics
                                        (:getDocumentVersionRequest nil))
                     :documentFeatures (:documentFormatting
                                        (:defaultPrintWidth 100
    					 :getDocumentPrintWidthRequest nil)
                                        :documentSymbol t
                                        :documentColor t)))))
  :config
  (define-derived-mode vue-mode web-mode "Vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  (add-hook 'vue-mode-hook #'eglot-ensure)

  (add-to-list 'eglot-server-programs
               `(vue-mode . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))

  ;;rust
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'rust-ts-mode-hook 'eglot-ensure)

  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))


  (add-to-list 'eglot-server-programs
               `(c-ts-mode . ("clangd" "-log=verbose" :initializationOptions (:compilationDatabasePath "/workspaces/r4ldriver/linux_raspberrypi/build_4b/") )))


  (with-eval-after-load 'eglot
    (require 'eglot-x)
    (eglot-x-setup))


  ;; project-find-function which uses projectile methods to find
  ;; the projectile project associated with a directory.
  ;; If projectile not loaded, or directory is not in a project,
  ;; hopefully returns nil.
  ;; (defun me:project-finder (dir)
  ;;   (if (fboundp 'projectile-project-root)
  ;;       (let ((root (projectile-project-root dir)))
  ;;         (and root (cons 'transient root)))))
  ;; (add-to-list 'project-find-functions #'me:project-finder)

  )

(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq evil-shift-width 4))))

;;(setq! treesit-auto-langs '(python rust go vue java))

(setq! evil-shift-width 4)

(with-eval-after-load 'company (define-key company-active-map (kbd "TAB") 'company-complete-selection) (define-key company-active-map (kbd "<tab>") 'company-complete-selection))

(use-package! gptel
  :config
  ;; OPTIONAL configuration
  (setq! auth-source-debug t)
  (setq! gptel-default-mode 'org-mode)
  (setq! gptel-api-key (lambda()
                         (gptel-api-key-from-auth-source "chatapi.onechats.top") ))



  (defvar gptel--openai-proxy
    (gptel-make-openai
        "ChatGptProxy"
      :key 'gptel-api-key
      :host "chatapi.onechats.top"
      :stream t
      :models '("gpt-4o-mini-2024-07-18" "gpt-3.5-turbo" "gpt-4o"))
    )

  (defvar gptel--deepseek-proxy
    (gptel-make-openai "DeepseekProxy"
      :key (lambda() (gptel-api-key-from-auth-source "api.deepseek.com"))
      :host "api.deepseek.com"
      :stream t
      :models '("deepseek-chat" "deepseek-coder"))
    )

  (defvar gptel--openai-proxy-claude
    (gptel-make-openai
        "ClaudeProxy"
      :key 'gptel-api-key
      :host "chatapi.onechats.top"
      :stream t
      :models '("deepseek-coder" "deepseek-chat" "gpt4o-mini" "claude-3-haiku-20240307" "claude-3-5-sonnet-20240620"))
    )

  ;; (defvar gptel--anthropic

  ;;   (gptel-make-anthropic "ClaudeProxy"
  ;;     :key 'gptel-api-key
  ;;     :header
  ;;     (lambda () (when-let (key (gptel--get-api-key))
  ;;                  `(("Authorization" . ,(concat "Bearer " key)))))
  ;;     :host "chatapi.onechats.top"
  ;;     :stream t
  ;;     :endpoint "/v1/chat/completions"
  ;;     :models '("claude-3-haiku-20240307")
  ;;     )


  ;;   )

  (defvar gptel--ollama
    (gptel-make-ollama "Ollama"
      :host "localhost:11434"
      :stream t
      :models '("qwen2:1.5b" "deepseek-coder-v2:latest" "scomper/minicpm-v2.5:latest" "gemma2:9b" "phi3:medium"))
    )

  ;; (setq!
  ;;  gptel-backend gptel--openai-proxy
  ;;  )

  ;; (setq! gptel-model "gpt-4o-mini-2024-07-18") )
  ;;
  (setq!
   gptel-backend gptel--deepseek-proxy
   )

  (setq! gptel-model "deepseek-chat") )



(use-package! go-translate
  :config

  ;; translate
  (setq gt-langs '(en zh))
  (setq gt-default-translator (gt-translator :engines (gt-google-engine)))

  (setq gt-preset-translators
        `((ts-1 . ,(gt-translator
                    :taker (gt-taker :langs '(en zh) :text 'word)
                    :engines (gt-google-engine)
                    :render (gt-buffer-render)))
          (ts-2 . ,(gt-translator
                    :taker (gt-taker :langs '(en zh) :text 'sentence)
                    :engines (gt-google-engine)
                    :render (gt-insert-render)))
          (ts-3 . ,(gt-translator
                    :taker (gt-taker :langs '(en zh) :text 'buffer
                                     :pick 'word :pick-pred (lambda (w) (length> w 6)))
                    :engines (gt-google-engine)
                    :render (gt-overlay-render :type 'help-echo)))))

  )
