;; Use company as completion front-end

(use-package company
  :config
  (global-company-mode)

  (setq company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-idle-delay 0.2
        company-require-match nil)
  (add-to-list 'company-frontends 'company-tng-frontend)
  (define-key company-active-map (kbd "<tab>") 'company-select-next)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

;; ycmd

(use-package ycmd
  :config
  (setq ycmd-server-command `("python" ,(file-truename "~/src/ycmd/ycmd/")))
  (setq ycmd-rust-src-path (file-truename "~/src/rust/src/"))
  :diminish ycmd-mode)

(use-package company-ycmd
  :after ycmd
  :config (company-ycmd-setup))

(provide 'iasoon-completion)
