;;; -*- lexical-binding: t; -*-
;;;

(defun get-secret (host)
  ;; Get the secret for the given host from the auth-source file.

  (let ((auth-info (auth-source-search :host host :require '(:secret))))
    (when auth-info
      (let ((entry (car auth-info)))
        (plist-get entry :secret)))))

(use-package! minuet
  :defer t
  :bind
  (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
   ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion

   :map minuet-active-mode-map
   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
   ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
   ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
   ;; Accept the first line of completion, or N lines with a numeric-prefix:
   ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
   ("M-a" . #'minuet-accept-suggestion-line)
   ("M-e" . #'minuet-dismiss-suggestion))

  :init
  ;; if you want to enable auto suggestion.
  ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

  :config
  (setq minuet-provider 'codestral)

  ;; Required when defining minuet-ative-mode-map in insert/normal states.
  ;; Not required when defining minuet-active-mode-map without evil state.
  (add-hook 'minuet-active-mode-hook #'evil-normalize-keymaps)


  (plist-put minuet-codestral-options :api-key (get-secret "mistral.ai"))


  (minuet-set-optional-options minuet-openai-fim-compatible-options :top_p 0.9)


  )
(provide 'minuet-custom.el)
