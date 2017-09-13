(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ui
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(blink-cursor-mode -1)

(use-package beacon
  :config
  (beacon-mode +1))

(use-package which-key
  :config
  (which-key-mode +1))

;; better scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; ivy mode
(use-package ivy
  :config
  ;; fuzzy matching
  (setq ivy-re-builders-alist
	'((t . ivy--regex-fuzzy)))
  ;; no initial inputs; fuzzy all the way
  (setq ivy-initial-inputs-alist nil))

(use-package swiper)
(use-package counsel
  :bind ("M-x" . counsel-M-x))

(use-package flx)  ; for fuzzy matching
(use-package smex) ; for counsel-M-x

;; git
(use-package magit)

;; rust development
(use-package rust-mode)
(use-package racer
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (setq racer-rust-src-path "~/src/rust/src")
  :diminish racer-mode)

(use-package cargo
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (setq compilation-ask-about-save nil)
  :diminish cargo-minor-mode)
