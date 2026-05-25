;; -*- lexical-binding: t; -*-

(use-package! msgpack)
(use-package! tramp-rpc
  :after tramp
  :config

  (setq tramp-rpc-deploy-git-build-policy 'release)

  ;; 让 /rpc:/ 路径保存到 recentf
  (after! recentf
    (add-to-list 'recentf-keep
                 (lambda (file)
                   (and (stringp file)
                        (string-match-p "/rpc:" file)))))

  )
