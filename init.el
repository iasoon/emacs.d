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

;; add language modules to load path
(add-to-list 'load-path (expand-file-name "lang" config-dir))

;; Don't save backups to current directory
(setq backup-directory-alist
      '(("." . "~/.emacs.d/bak")))


;; core
(require 'iasoon-general)
(require 'iasoon-appearance)
(require 'iasoon-ivy)
(require 'iasoon-git)

;; languages
(require 'iasoon-c)
(require 'iasoon-rust)
(require 'iasoon-javascript)
