(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(elfeed-feeds
   '("https://rsshub.app/hackernews/threads/comments_list/dang"))
 '(ignored-local-variable-values
   '((etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc")
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/" "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(safe-local-variable-values
   '((setq-default eglot-workspace-configuration
                   '(:rust-analyzer
                     (:linkedProjects
                      ["/workspaces/cicv-r4l-4-zrubing/src_e1000/rust-project.json"]
                      :server
                      (:extraEnv
                       (:RA_LOG_FILE "/workspaces/cicv-r4l-4-zrubing/rust-analyzer.log" :RA_LOG "debug"))
                      :cargo
                      (:sysroot "discover" :buildScripts
                       (:enable t)
                       :procMacro
                       (:enable t)))))
     (eval with-eval-after-load 'eglot
           (add-to-list 'eglot-server-programs
                        `(rustic-mode "RA_LOG=lsp_server=debug RA_LOG_FILE=analyzer.log rust-analyzer" :initializationOptions
                          (:linkedProjects
                           ["/workspaces/cicv-r4l-4-zrubing/src_e1000/rust-project.json"]))))
     (eval setq-default eglot-workspace-configuration
           '(:rust-analyzer
             (:linkedProjects
              ["/workspaces/cicv-r4l-4-zrubing/src_e1000/rust-project.json"]
              :cargo
              (:sysroot "discover" :buildScripts
               (:enable t)
               :procMacro
               (:enable t)))))
     (eval setq-default eglot-workspace-configuration
           '(:rust-analyzer
             (:linkedProjects
              ["/workspaces/cicv-r4l-4-zrubing/src_e1000/rust-project.json"]
              :procMacro
              (:server "/usr/local/rustup/toolchains/nightly-x86_64-unknown-linux-gnu/libexec/rust-analyzer-proc-macro-srv"))))
     (eval setq-default eglot-workspace-configuration
           '(:rust-analyzer
             (:procMacro
              (:enable nil))
             (:linkedProjects
              ["/workspaces/cicv-r4l-4-zrubing/src_e1000/rust-project.json"])))
     (eval setq-default eglot-workspace-configuration
           '(:rust-analyzer
             (:linkedProjects
              ["/workspaces/cicv-r4l-4-zrubing/src_e1000/rust-project.json"])))
     (eval progn
           (pp-buffer)
           (indent-buffer)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
