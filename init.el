(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq config-dir (file-name-directory load-file-name))
;; add core modules to load path
(add-to-list 'load-path (expand-file-name "core" config-dir))

;; Don't save backups to current directory
(setq backup-directory-alist
      '(("." . "~/.emacs.d/bak")))

;; ivy mode
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
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file))

(use-package flx)  ; for fuzzy matching
(use-package smex) ; for counsel-M-x

;; git
(use-package magit
  :bind
  ("C-x g" . magit-status))

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

(require 'appearance)
