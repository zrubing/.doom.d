;; lexical-binding: t;

(use-package! org-roam)

(after! org-roam
  ;; Key bindings (global)
  (map! "C-c n l" #'org-roam-buffer-toggle
        "C-c n f" #'org-roam-node-find
        "C-c n g" #'org-roam-graph
        "C-c n i" #'org-roam-node-insert
        "C-c n c" #'org-roam-capture
        "C-c n j" #'org-roam-dailies-capture-today)

  ;; Basic configuration
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (file-truename "~/org-roam-dir"))

  ;; Node display template
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

  ;; Enable database autosync
  (org-roam-db-autosync-mode)

  ;; Load protocol support
  (require 'org-roam-protocol))



;; (setq org-agenda-include-diary t)
(setq org-time-stamp-custom-formats '("<%Y-%m-%d %a %H:%M>"))
(setq org-agenda-files (directory-files-recursively (expand-file-name "~/org-roam-dir/") "^2026.*\\.org$"))

(defvar jojo/code-workspace-root
  (expand-file-name "~/codeWorkspace/")
  "Base path for my code workspace.")

(with-eval-after-load 'ol
  (org-link-set-parameters
   "cw"
   :follow (lambda (path in-emacs)
             (org-link-open-as-file
              (expand-file-name path jojo/code-workspace-root)
              in-emacs))
   :face '(:foreground "orange" :underline t)))

(after! (:and org verb)
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  )
