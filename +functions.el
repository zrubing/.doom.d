;;; +functions.el -*- lexical-binding: t; -*-


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
