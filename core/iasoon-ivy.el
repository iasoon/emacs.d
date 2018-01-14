(use-package ivy
  :config

  ;; use fuzzy matching
  (setq ivy-re-builders-alist
	'((swiper . ivy--regex-plus)
	  (t . ivy--regex-fuzzy)))

  ;; no initial inputs; fuzzy all the way
  (setq ivy-initial-inputs-alist nil)

  ;; dont list current and parent directory
  (setq ivy-extra-directories nil)

  ;; allow selecting the prompt string
  (setq ivy-use-selectable-prompt t)

  ;; delayed updates for more repsonsivity
  (setq counsel-async-filter-update-time 10000)
  (setq ivy-dynamic-exhibit-delay-ms 20)

  (ivy-mode)
  :diminish ivy-mode)




(use-package swiper
  :bind ("C-s" . swiper))

(use-package counsel
  :config

  ;; fuzzy matching
  (use-package flx)

  ;; smex in counsel-M-x
  (use-package smex)

  (setq counsel-rg-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s .")
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-h b" . counsel-descbinds))


(provide 'iasoon-ivy)
