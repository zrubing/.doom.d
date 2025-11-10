;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)

(package! counsel-etags)
(package! org)

(package! org-download)
(package! org-pomodoro)

(package! benchmark-init)
(package! org-roam)
(package! rime
  :recipe (:host github :repo "DogLooksGood/emacs-rime"  :files ("*.el" "Makefile" "lib.c" "*.so")))
(package! immersive-translate)
(package! go-mode)
;;(package! doom-modeline :pin "918730eff72e")
;;(package! dired+)
;;(package! dired-recent)
;; (package! dired-sidebar)
(package! telega)
(package! org-modern
  :recipe (:host github :repo "minad/org-modern"
           :files ("org-modern.el")
           ))
;; (package! pangu-spacing
;;   :recipe (:host github :repo "coldnew/pangu-spacing"
;;            :files ("pangu-spacing.el")))


;;https://github.com/purcell/exec-path-from-shell
(package! exec-path-from-shell
  :recipe (:host github :repo "purcell/exec-path-from-shell"))

;; (package! mind-wave
;;   :recipe (:host github :repo "manateelazycat/mind-wave" :build nil))

;; (package! bard
;;   :recipe (:host github :repo "AllTheLife/Bard.el" :build nil))

;; (package! eaf
;;   :recipe (:host github :repo "emacs-eaf/emacs-application-framework"
;;                  :build nil))

;; (package! insert-translate-name
;;   :recipe (:host github :repo "manateelazycat/insert-translated-name"))

;; (package! eaf-browser
;;    :recipe (:host github :repo "emacs-eaf/eaf-browser" :build nil))

;; (package! eaf-terminal
;;   :recipe (:host github :repo "emacs-eaf/eaf-terminal"))

;; (package! eaf-pdf-viewer
;;   :recipe (:host github :repo "emacs-eaf/eaf-pdf-viewer"))



;; (package! eaf-file-sender
;;   :recipe (:host github :repo "emacs-eaf/eaf-file-sender"))

(package! kele.el
  :recipe (:host github :repo "jinnovation/kele.el"))

(package! plz
  :recipe (:host github :repo "alphapapa/plz.el"))




;; (package! magit-popup
;;   :recipe (:host github :repo "magit/magit-popup"))

(when (package! lsp-bridge
        :recipe (:host github
                 :repo "manateelazycat/lsp-bridge"
                 :branch "master"
                 :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                 ;; do not perform byte compilation or native compilation for lsp-bridge
                 :build (:not compile)))
  (package! markdown-mode)
  (package! yasnippet))

(package! typescript
  :recipe (:host github :repo "emacs-typescript/typescript.el"))

;; (package! dap-mode
;;   :recipe (:host github :repo "emacs-lsp/dap-mode" :build nil))

;; (package! popweb
;;   :recipe (:host github :repo "manateelazycat/popweb"
;;                  :files ("*.el" "*.py" "*.js")))

;; (package! websocket-bridge
;;   :recipe (:host github :repo "ginqi7/websocket-bridge" :build nil))

;; (package! dictionary-overlay
;;   :recipe (:host github :repo "ginqi7/dictionary-overlay"
;;            :build nil))


(package! websocket)

(package! kubernetes
  :recipe (:host github :repo "kubernetes-el/kubernetes-el" :files ("*.el")))

(package! kubernetes-evil
  :recipe (:host github :repo "kubernetes-el/kubernetes-el" :files ("*.el")))

;; (package! ejc-sql
;;   :recipe (:host github :repo "kostafey/ejc-sql" :build nil))

(package! rest-client
  :recipe (:host github :repo "pashky/restclient.el" :build nil))

(package! vue-ts-mode
  :recipe (:host github :repo "8uff3r/vue-ts-mode" :files ("*.el")))

;; (package! dape
;;   :recipe (:host github :repo "svaante/dape" :build nil))
(package! repeat)

;; (package! web-mode)

(package! minibuffer-modifier-keys)

(package! vue-mode)
;;(package! alert)
;; (package! treesit-auto)


(package! dired-rsync)

(package! dired-rsync-transient)

;; (package! go-translate)

(package! window-numbering)

(package! ejc-sql)

(package! flx-ido)

(package! auto-complete)

(package! devdocs)

(package! verb)

(package! journalctl-mode)

;; (package! transient
;;   :pin "df5856bb960933c91ec391de79f75ecd2cb55e52")
;; (package! magit
;;   :pin "0c134614665c70552aff6786844e4792365ff7e5")

(package! gptel)

(package! topsy)

;;(package! treemacs :pin "5fa84199501fd43e5573b1277a2b1699c7473cc1")
;;(package! persp-mode :pin "f146ddccaf598feb402664bc6848b60321b2dc78")
;; (package! persp-mode :pin "40e9993a9711cba5fb56dfec81a507fabeba9668")
;; (package! diff-hl :pin "39f076efa85110c4bcc9b73994f30a7d52312c98")


(package! minuet
  :recipe (:host github :repo "milanglacier/minuet-ai.el"
                 ))

(package! aider :recipe (:host github :repo "tninja/aider.el" :files ("*.el") ))

(package! aidermacs :recipe (:host github :repo "MatthewZMD/aidermacs" :files ("*.el")))

(package! ultra-scroll
  :recipe (:host github :repo "jdtsmith/ultra-scroll" :files ("ultra-scroll.el")))

(package! emigo :recipe (:host github :repo "MatthewZMD/emigo"
                         :files ("*.el" "*.py" "queries")
                         :build (:not compile)
                         ))
(package! mcp :recipe (:host github :repo "lizqwerscott/mcp.el" :files ("*.el")))

(package! csv-mode)

(package! eat
  :recipe (:host codeberg :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el"))))
;; (package! direnv)
(package! envrc)

(package! claude-code-ide
  :recipe (:host github :repo "manzaltu/claude-code-ide.el"
           :files ("*.el" "*.sh")
           :build (:not compile)))

(package! emacs-web-server
  :recipe (:host github :repo "skeeto/emacs-web-server" :files ("*.el"))
  )

(package! grid-table
  :recipe (:host github :repo "yibie/grid-table" :files ("*.el" "plugins")))

;; (package! copilot
;;   :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

(unpin! org-roam)
(package! org-roam-ui)

(package! eglot)
(package! lsp-proxy :recipe (:host github :repo "jadestrong/lsp-proxy"
                             :files ("*.el")
                             :build (:not compile)
                             ))
(package! company-box)


(package! shell-maker)
(package! acp :recipe (:host github :repo "xenodium/acp.el"))
(package! agent-shell :recipe (:host github :repo "xenodium/agent-shell"))

(package! eca :recipe (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))

;; Alabaster themes
(package! alabaster-theme
  :recipe (:host github :repo "uzhne/alabaster-emacs" :files ("alabaster-theme.el")))

;; Custom Alabaster Dark theme (local file)
;; Local file created at: ~/.doom.d/themes/alabaster-dark-theme.el
