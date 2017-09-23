;; Use magit. Duh.
(use-package magit
  :bind
  ("C-x g" . magit-status))

;; show git status in fringe
(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode)
  :diminish git-gutter-mode)

(provide 'iasoon-git)
