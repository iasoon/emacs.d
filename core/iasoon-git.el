;; Use magit. Duh.
(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package magithub
  :after magit
  :ensure t
  :config (magithub-feature-autoinject t))

;; show git status in fringe
(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode)
  :diminish git-gutter-mode)

(provide 'iasoon-git)
