


(use-package! ghostel)

(after! ghostel

  (evil-set-initial-state 'ghostel-mode 'emacs)

  (with-eval-after-load 'ghostel
    ;; === 1. 彻底禁用 evil-ghostel 的自动行为（最重要！）===
    (when (fboundp 'evil-ghostel-mode)
      (remove-hook 'ghostel-mode-hook #'evil-ghostel-mode))

    ;; === 2. 强制 Ghostel 进入 Emacs state ===
    (add-hook 'ghostel-mode-hook #'evil-emacs-state)

    ;; === 3. Shift + Enter → 发送换行 ===
    (define-key ghostel-mode-map (kbd "S-<return>")
                (lambda () (interactive)
                  (process-send-string (get-buffer-process (current-buffer)) "\n")))

    ;; === 4. Ctrl + D → 发送 EOF（退出 shell）===
    (define-key ghostel-mode-map (kbd "C-d")
                (lambda () (interactive)
                  (process-send-string (get-buffer-process (current-buffer)) "\C-d")))

    ;; === 新增：Ctrl + U → 发送给 shell（删除到行首）===
    (define-key ghostel-mode-map (kbd "C-u")
                (lambda () (interactive)
                  (process-send-string (get-buffer-process (current-buffer)) "\C-u")))

    )

  )
