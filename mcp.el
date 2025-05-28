;; -*- lexical-binding: t; -*-

(use-package! mcp
  :config
  (require 'mcp-hub)
  (after! gptel
    (require 'gptel-integrations)
    )


  (load! "mcp-conf")


  )


(provide 'mcp.el)
