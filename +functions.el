;;; +functions.el -*- lexical-binding: t; -*-


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


(after! grid-table
  (after! ejc-sql
    (defun my/eval-sql-region ()

      (grid-open)

      "Evaluate SQL bounded by the selection area."
      (interactive "r")
      (ejc-check-connection)
      (let ((sql (buffer-substring beg end)))
        (ejc-eval-user-sql sql
                           :display-result nil
                           :result-file (ejc-next-result-file-path)
                           ))
      (if (region-active-p)
          (deactivate-mark))

      )))
