;;; ejc-sql-conf.el -*- lexical-binding: t; -*-



(use-package! ejc-sql)

(add-hook 'ejc-sql-minor-mode-hook
          (lambda ()
            (auto-complete-mode t)
            (ejc-ac-setup)))


(setq ejc-use-flx t)
(setq ejc-flx-threshold 2)


(add-hook 'ejc-sql-connected-hook
          (lambda ()
            (ejc-set-fetch-size 50)
            (ejc-set-max-rows 50)
            (ejc-set-show-too-many-rows-message t)
            (ejc-set-column-width-limit 25)
            (ejc-set-use-unicode t)))

(load! "ejc-sql-connect-conf")

(provide 'ejc-sql-conf)
