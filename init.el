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

(require 'iasoon-general)
(require 'iasoon-appearance)
(require 'iasoon-ivy)
