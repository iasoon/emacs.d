(setq python-shell-interpreter "python3")

(use-package anaconda-mode
  :config (add-hook 'python-mode-hook 'anaconda-mode))

(use-package company-anaconda
  :after anaconda-mode
  :config (add-to-list 'company-backends 'company-anaconda))


(provide 'iasoon-python)
