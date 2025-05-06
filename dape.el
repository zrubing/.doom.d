;;; dape.el -*- lexical-binding: t; -*-
;;;

(after! dape
  ;; (add-to-list 'dape-configs
  ;;              `(jdtls
  ;;                modes (java-mode java-ts-mode)
  ;;                ensure (lambda (config)
  ;;                         (let ((file (dape-config-get config :filePath)))
  ;;                           (unless (and (stringp file) (file-exists-p file))
  ;;                             (user-error "Unable to locate :filePath `%s'" file))
  ;;                           (with-current-buffer (find-file-noselect file)
  ;;                             (unless (and (featurep 'lsp-bridge) (lsp-bridge-has-lsp-server-p))
  ;;                               (user-error "No lsp-bridge instance active in buffer %s" (current-buffer)))
  ;;                             (unless (lsp-bridge-capable-p "vscode.java.resolveClasspath")
  ;;                               (user-error "Jdtls instance does not bundle java-debug-server, please install")))))
  ;;                fn (lambda (config)
  ;;                     (with-current-buffer
  ;;                         (find-file-noselect (dape-config-get config :filePath))
  ;;                       (if-let* ((server (lsp-bridge-get-server)))
  ;;                           (pcase-let ((`[,module-paths ,class-paths]
  ;;                                        (lsp-bridge-execute-command server
  ;;                                                                    "vscode.java.resolveClasspath"
  ;;                                                                    (vector (plist-get config :mainClass)
  ;;                                                                            (plist-get config :projectName))))
  ;;                                       (port (lsp-bridge-execute-command server
  ;;                                                                        "vscode.java.startDebugSession" nil)))
  ;;                             (thread-first config
  ;;                                          (plist-put 'port port)
  ;;                                          (plist-put :modulePaths module-paths)
  ;;                                          (plist-put :classPaths class-paths)))
  ;;                         server)))
  ;;                ,@(cl-flet ((resolve-main-class (key)
  ;;                             (ignore-errors
  ;;                               (let* ((main-classes
  ;;                                       (with-no-warnings
  ;;                                         (lsp-bridge-execute-command
  ;;                                          (lsp-bridge-get-server)
  ;;                                          "vscode.java.resolveMainClass"
  ;;                                          (file-name-nondirectory
  ;;                                           (directory-file-name (dape-cwd))))))
  ;;                                      (main-class
  ;;                                       (or (seq-find (lambda(val)
  ;;                                                       (equal (plist-get val :filePath)
  ;;                                                              (buffer-file-name)))
  ;;                                                     main-classes)
  ;;                                           (aref main-classes 0))))
  ;;                                 (plist-get main-class key)))))
  ;;                    `(:filePath
  ;;                      ,(lambda ()
  ;;                         (or (resolve-main-class :filePath)
  ;;                             (expand-file-name (dape-buffer-default) (dape-cwd))))
  ;;                      :mainClass
  ;;                      ,(lambda () (resolve-main-class :mainClass))
  ;;                      :projectName
  ;;                      ,(lambda () (resolve-main-class :projectName))))
  ;;                :args ""
  ;;                :stopOnEntry nil
  ;;                :type "java"
  ;;                :request "launch"
  ;;                :vmArgs " -XX:+ShowCodeDetailsInExceptionMessages"
  ;;                :console "integratedConsole"
  ;;                :internalConsoleOptions "neverOpen")
  ;;              )
  )
(provide 'dape)
