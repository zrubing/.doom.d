
(use-package! kele
  :init
  (map! :prefix ("s-k" . "kele")) ;; 这个不等价于 bind-keymap，仅示意
  :config
  (kele-mode 1)
  :bind-keymap
  ("s-k" . kele-command-map))
