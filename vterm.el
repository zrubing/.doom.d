;; lexical-binding: t; -*-

(after! vterm

  (with-eval-after-load 'vterm
    (setq vterm-shell "bash")
    ;; 解除 M-0 到 M-9 的绑定
    (dolist (key '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
      (define-key vterm-mode-map (kbd (concat "M-" key)) nil)))

  ;; rime输入法
  (add-hook 'vterm-mode-hook
            (lambda ()
              (define-key vterm-mode-map (kbd "C-\\") nil)))

  (defun my/set-vterm-shell ()
    (when (string-prefix-p "/docker:" (file-remote-p default-directory))
      (when (eq major-mode 'vterm-mode)
        (let ((shell (if (string-prefix-p "/docker:" (file-remote-p default-directory))
                         "/bin/bash"
                       (or (getenv "SHELL") "/bin/bash"))))
          (vterm-send-string (format "exec %s\n" shell))
          (vterm-send-string "clear\n")))))

  (add-hook 'vterm-mode-hook #'my/set-vterm-shell))
