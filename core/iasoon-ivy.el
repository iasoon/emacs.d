(use-package ivy
  :config
  ;; fuzzy matching
  (setq ivy-re-builders-alist
	'((swiper . ivy--regex-plus)
	  (t . ivy--regex-fuzzy)))
  ;; no initial inputs; fuzzy all the way
  (setq ivy-initial-inputs-alist nil)
  ;; dont list current and parent directory
  (setq ivy-extra-directories nil)
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
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file))


(provide 'iasoon-ivy)
