;; Use company as completion front-end

(use-package company
  :config
  (global-company-mode)

  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-require-match nil))

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
