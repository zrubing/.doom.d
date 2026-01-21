;; -*- lexical-binding: t; -*-

(use-package! tramp-rpc
  :after tramp
  :config
  ;; 让 /rpc:/ 路径保存到 recentf
  (after! recentf
    (add-to-list 'recentf-keep
                 (lambda (file)
                   (and (stringp file)
                        (string-match-p "/rpc:" file)))))

  )
