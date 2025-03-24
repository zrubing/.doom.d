;;; ejc-sql-conf.el -*- lexical-binding: t; -*-



(add-hook 'ejc-sql-minor-mode-hook
          (lambda ()
            (auto-complete-mode t)
            (ejc-ac-setup)))


(setq ejc-use-flx t)
(setq ejc-flx-threshold 2)

(setq ejc-result-table-impl 'ejc-result-mode)

(add-hook 'ejc-sql-connected-hook
          (lambda ()
            (ejc-set-fetch-size 50)
            (ejc-set-max-rows 50)
            (ejc-set-show-too-many-rows-message t)
            (ejc-set-column-width-limit 48)
            (ejc-set-use-unicode t)))

(load! "ejc-sql-connect-conf")

(provide 'ejc-sql-conf)
