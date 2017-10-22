(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook 'ycmd-mode))

(use-package cargo
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (setq compilation-ask-about-save nil)
  :diminish cargo-minor-mode)


(provide 'iasoon-rust)
