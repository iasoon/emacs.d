
(setq js-indent-level 2)
;; for better json editing
(use-package json-mode)

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)))

;; for javascript runtime integration
(use-package indium)

(provide 'iasoon-javascript)
