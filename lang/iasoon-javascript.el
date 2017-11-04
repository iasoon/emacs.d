
(setq js-indent-level 2)
;; for better json editing
(use-package json-mode)

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :config
  (setq js2-global-externs '("module" "require")))

(use-package tern
  :after js2-mode
  :config
  (setq tern-command (append tern-command '("--no-port-file")))
  (add-hook 'js2-mode-hook (lambda () (tern-mode t))))

(use-package company-tern
  :after tern
  :config
  (add-to-list 'company-backends 'company-tern))

;; for javascript runtime integration
(use-package indium)

(provide 'iasoon-javascript)
