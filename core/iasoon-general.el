(use-package which-key
  :config
  (which-key-mode +1)
  :diminish which-key-mode)

;; better scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Automatically update unmodified buffers whose files have changed.
(global-auto-revert-mode 1)
(diminish 'auto-revert-mode)

;; please no sudden loud noises
(setq ring-bell-function 'ignore)

;; Tab indentation is a disease; a cancer of this planet.
;; Turn it off and let's never talk about this default again.
(set-default 'indent-tabs-mode nil)

(use-package hungry-delete
  :config
  (defadvice hungry-delete-backward (before sp-delete-pair-advice activate)
    (save-match-data (sp-delete-pair (ad-get-arg 0))))
  (global-hungry-delete-mode)
  :diminish (hungry-delete-mode))

(use-package crux
  :bind
  ("C-a" . crux-move-beginning-of-line)
  ("C-k" . crux-smart-kill-line))

;; auto-fill comments
(setq comment-auto-fill-only-comments 1)
(setq-default fill-column 80)

;; tramp
(setq tramp-default-method "ssh")

;; avy
(use-package avy
  :config
  ;; dvorak home row keys
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  (setq avy-timeout-seconds 0.4)
  :bind ("M-s" . avy-goto-char-timer))

(use-package projectile
  :demand t
  :config
  (projectile-global-mode t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'projectile-dired)
  :diminish projectile-mode)

(use-package counsel-projectile
  :demand t
  :config
  :bind (:map projectile-mode-map
              ("C-x f" . counsel-projectile-find-file)
              ;; TODO: make this work without projectile
              ("M-s" . counsel-projectile-rg)))

(bind-key "C-c c" 'ivy-switch-buffer)

(provide 'iasoon-general)
