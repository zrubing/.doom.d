;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(use-package! multi-vterm :ensure t)

;; we need this wrapper to match Projectile's API
(defun projectile-vc-root-dir (dir)
  "Retrieve the root directory of the project at DIR using `vc-root-dir'."
  (let ((default-directory dir))
    (vc-root-dir)))

(setq projectile-project-root-functions '(projectile-vc-root-dir
                                          projectile-root-bottom-up
                                          ))



(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq lsp-java-java-path "/usr/local/Cellar/openjdk@11/11.0.10/bin/java")

(setq org-roam-directory (file-truename "~/org-roam-dir"))


(setq lsp-java-vmargs '(
                          "-javaagent:/Users/admin/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar"
                          ))


(setq lsp-java-configuration-runtimes '[(:name "JavaSE-1.8"
                        :path "/Library/Java/JavaVirtualMachines/jdk1.8.0_201.jdk/Contents/Home")
                        (:name "JavaSE-11"
                         :path "/usr/local/Cellar/openjdk@11/11.0.10"
                        :default t)])

(setq org-roam-directory (file-truename "~/org-roam-dir"))


(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

(setq url-proxy-services
          '(("http"  . "localhost:1081")
    	("https" . "localhost:1081")))

(setq dash-docs-docsets (list "Spring Framework" "Rust"))
