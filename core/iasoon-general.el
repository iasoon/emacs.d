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
  :config (global-hungry-delete-mode)
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
  :diminish projectile-mode)

(use-package counsel-projectile
  :demand t
  :config
  (counsel-projectile-on)
  (global-set-key (kbd "C-c p s r") 'counsel-projectile-rg))

(use-package company
  :config
  (global-company-mode)

  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-require-match nil))

(provide 'iasoon-general)
