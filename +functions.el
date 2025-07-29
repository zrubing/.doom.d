;;; +functions.el -*- lexical-binding: t; -*-


(defun my/enable-global-lsp-bridge-mode ()
  "Add custom lsp-bridge language server configurations and enable lsp-bridge-mode"
  (dolist (hook (append lsp-bridge-default-mode-hooks acm-backend-capf-mode-hooks))
    (add-hook hook (lambda ()
                     (setq confdir (or (let ((local-conf (expand-file-name "lsp-bridge-config")))
                                         (when (file-directory-p local-conf) local-conf))
                                       "~/.doom.d/lsp-bridge-config"))
                     ;; check if a directory named ".lsp" exists in the projects root
                     (when-let (proot (projectile-project-root))
                       (let ((d (concat proot ".lsp/")))
                         (when (file-directory-p d) (setq confdir d))))
                     (when (file-directory-p confdir)
                       (setq-local lsp-bridge-user-langserver-dir (concat confdir "/langserver")
                                   lsp-bridge-user-multiserver-dir (concat confdir "/multiserver")))
                     (lsp-bridge-mode 1)))))


(defun +jump-to-mapper-xml-exact ()
  "Projectile 用完整文件名 base-nameMapper.xml 精确匹配，唯一时直接跳 SQL 节点。"
  (interactive)
  (let* ((method (thing-at-point 'symbol t))
         (java-file (buffer-file-name))
         (base-name (when (and java-file
                               (string-match "\\([^/]+\\)Mapper\\.java$" java-file))
                      (match-string 1 java-file)))
         (exact-name (concat base-name "Mapper.xml")))
    (unless (and method base-name)
      (user-error "无法解析 Mapper 名称"))
    ;; 精确匹配整个文件名
    (let* ((candidates
            (cl-remove-if-not
             (lambda (f) (string= (file-name-nondirectory f) exact-name))
             (projectile-current-project-files)))
           (xml-file (cond ((null candidates)
                            (user-error "未找到 %s" exact-name))
                           ((cdr candidates)        ; 多于 1 个（理论上不应出现）
                            (completing-read "多个同名文件: " candidates nil t))
                           (t
                            (car candidates)))))
      (when xml-file
        (find-file (expand-file-name xml-file (projectile-project-root)))
        (goto-char (point-min))
        (if (re-search-forward
             (format "<\\(?:select\\|insert\\|update\\|delete\\)[^>]*id=\"%s\""
                     (regexp-quote method))
             nil t)
            (progn
              (beginning-of-line)
              (recenter-top-bottom)
              )
          (message "未找到 id=\"%s\" 的节点" method))))))

(with-eval-after-load 'java-ts-mode
  (define-key java-ts-mode-map (kbd "C-c m") #'+jump-to-mapper-xml-exact))
