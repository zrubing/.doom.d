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


  (setq minuet-provider 'openai-compatible)
  (setq minuet-request-timeout 3.5)
  (setq minuet-auto-suggestion-throttle-delay 1.5) ;; Increase to reduce costs and avoid rate limits
  (setq minuet-auto-suggestion-debounce-delay 0.6) ;; Increase to reduce costs and avoid rate limits

  (let ((api-key (funcall (plist-get (car (auth-source-search :host "openrouter.ai")) :secret))))
    (setenv "OPENROUTER_API_KEY" (encode-coding-string api-key 'utf-8)))

  (plist-put minuet-openai-compatible-options :end-point "https://openrouter.ai/api/v1/chat/completions")
  (plist-put minuet-openai-compatible-options :api-key "OPENROUTER_API_KEY")
  (plist-put minuet-openai-compatible-options :model "google/gemini-2.0-flash-lite-001")
  ;;(plist-put minuet-openai-compatible-options :model "qwen/qwen3-8b:free")




  ;;(plist-put minuet-openai-compatible-options :model "meta-llama/llama-4-scout:free")
  ;; (plist-put minuet-openai-compatible-options :end-point "https://api.siliconflow.cn/v1/chat/completions")
  ;; (plist-put minuet-openai-compatible-options :api-key (get-secret "siliconflow.cn"))
  ;; ;;(plist-put minuet-openai-compatible-options :model "Qwen/Qwen2.5-Coder-7B-Instruct")
  ;; (plist-put minuet-openai-compatible-options :model "Qwen/Qwen2.5-Coder-32B-Instruct")




  ;; Prioritize throughput for faster completion
  (minuet-set-optional-options minuet-openai-compatible-options :provider '(:sort "throughput"))
  (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 128)
  (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9)

  ;; Required when defining minuet-ative-mode-map in insert/normal states.
  ;; Not required when defining minuet-active-mode-map without evil state.
  (add-hook 'minuet-active-mode-hook #'evil-normalize-keymaps)




  )
(provide 'minuet-custom.el)
