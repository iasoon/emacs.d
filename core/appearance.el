;; Claim back valuable real estate
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; This is super annoying
(blink-cursor-mode -1)

;; pwetty colours!
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; Show line and column number in modeline
(line-number-mode t)
(column-number-mode t)

(provide 'appearance)
