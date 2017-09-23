
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

(provide 'iasoon-editor)
