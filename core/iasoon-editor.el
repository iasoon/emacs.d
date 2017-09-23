;; avoid buffers with the same name
(use-package uniquify
  ;; uniquify package is built-in
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; leave special buffers alone
  (setq uniquify-ignore-buffers-re "^\\*")
  )

(use-package smartparens
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)

  (show-smartparens-global-mode +1)
  (smartparens-global-mode +1)
  :diminish smartparens-mode)

(provide 'iasoon-editor)
