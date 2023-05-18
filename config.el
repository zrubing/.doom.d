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
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 14))
;;
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
(setq org-directory "~/org/")


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

(setq url-proxy-services
      '(("http"  . "localhost:7890")
    	("https" . "localhost:7890")))




(add-to-list 'initial-frame-alist '(fullscreen . maximized))


(use-package! org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/org-roam-dir"))
  (org-roam-dailies-directory "daily/")
  (org-attach-directory "~/org-roam-dir/attach/")
  (add-to-list 'org-tags-exclude-from-inheritance "project")

  :bind (
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n f" . org-roam-node-find)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))


(setq org-agenda-include-diary t)
(setq org-time-stamp-custom-formats '("<%Y-%m-%d %a %H:%M>"))

(setq org-agenda-diary-file "~/org-roam-dir/src/standard-diary") ;;2020-03-02 10:47:06
(setq diary-file "~/org-roam-dir/src/standard-diary")

;; (setq lsp-bridge-enable-profile t)
;; (setq exec-path (append exec-path '("/Users/admin/pypy3.9-v7.3.11-macos_arm64/bin")))
;; (setq lsp-bridge-python-command "pypy3")

(add-load-path! "~/.config/emacs/.local/straight/repos/emacs-application-framework")
(add-load-path! "~/.config/emacs/.local/straight/repos/eaf-browser")
(add-load-path! "~/.config/emacs/.local/straight/repos/eaf-pdf-viewer")
(add-load-path! "~/.config/emacs/.local/straight/repos/eaf-file-sender")
(add-load-path! "~/.config/emacs/.local/straight/repos/lsp-bridge")

(add-load-path! "~/.config/emacs/.local/straight/repos/popweb/extension/dict")
(require 'popweb-dict)

(add-load-path! "~/.config/emacs/.local/straight/repos/websocket-bridge")
(add-load-path! "~/.config/emacs/.local/straight/repos/dictionary-overlay")
(require 'websocket-bridge)
(require 'dictionary-overlay)


(require 'eaf)
(require 'eaf-browser)
(require 'eaf-file-sender)
(require 'eaf-pdf-viewer)


;; lombok support
(setq lombok-path (substitute-in-file-name "$HOME/dotfiles/private/libraries/lombok-1.18.26.jar"))
(setq lsp-bridge-jdtls-jvm-args (list (format "%s%s" "-javaagent:" lombok-path) "-XX:+OptimizeStringConcat" "-XX:MaxMetaspaceSize=1G" "-Xms512m" "-Xmx2048m" "-Xmn512m" "-Xss2m" "-XX:+UseG1GC" "-XX:+UseLargePages"))

;; (lsp-bridge-jdtls-project-cache-dir "/Users/admin/vscodeWorkspace/homestead-server-dz")

(require 'yasnippet)
(yas-global-mode 1)
(require 'lsp-bridge)
(require 'lsp-bridge-jdtls)

;; (add-load-path! "~/.config/emacs/.local/straight/repos/ejc-sql")
;; (require 'ejc-sql)

(global-lsp-bridge-mode)



(setq eaf-proxy-type "http")
(setq eaf-proxy-host "127.0.0.1")
(setq eaf-proxy-port "7890")

(add-to-list 'load-path "~/.config/emacs/.local/straight/repos/mind-wave")
(require 'mind-wave)
;; (setq mind-wave-enable-log t)


(require 'eaf-evil)
;; eaf会把C-SPC当成evil的leader-key，在你加载'eaf-evil之后使用eaf时就需要在eaf中键入C-SPC使用evil leader下的键。
;; 我们只需要将这个键设置为 SPC或你自己的evil-leader-key即可
(setq eaf-evil-leader-key "SPC")




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


(use-package! kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))


(use-package! kubernetes-evil
  :ensure t
  :after kubernetes)

;; alias kn='f() { [ "$1" ] && kubectl config set-context --current --namespace $1 || kubectl config view --minify | grep namespace | cut -d" " -f6 ; } ; f'
;;
(setq shell-file-name "zsh")
(setq shell-command-switch "-ic")

(defun kubernetes-switch-namespace (item)
  (interactive
   (list (completing-read "Choose an item: " '("sdsx-farmland" "hn-xjj-project" "ysx-rice" "homestead-product-hbdz"))))
    (shell-command (concat "kn " item))
  )
