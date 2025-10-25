;;; dape.el --- DAP Elisp package -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for DAP (Debug Adapter Protocol) integration

;;; Code:

(use-package dape
  :config
  (add-to-list 'dape-configs
               '(java
                 :type "server"
                 :host "127.0.0.1"
                 :port 5005
                 :request "attach"
                 :name "Java Attach"))
  )

(provide 'dape)

;;; dape.el ends here
