(use-package rust-mode)


(use-package racer
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (setq racer-rust-src-path "~/src/rust/src")
  :diminish racer-mode)


(use-package cargo
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (setq compilation-ask-about-save nil)
  :diminish cargo-minor-mode)


(provide 'iasoon-rust)
