(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq config-dir (file-name-directory load-file-name))

;; save customizations
(setq custom-file (expand-file-name "custome.el" config-dir))

;; add core modules to load path
(add-to-list 'load-path (expand-file-name "core" config-dir))

;; add language modules to load path
(add-to-list 'load-path (expand-file-name "lang" config-dir))

;; no autosaves or backups
(setq auto-save-default nil)
(setq make-backup-files nil)
 
;; core
(require 'iasoon-general)
(require 'iasoon-appearance)
(require 'iasoon-ivy)
(require 'iasoon-git)
(require 'iasoon-editor)

;; languages
(require 'iasoon-c)
(require 'iasoon-rust)
(require 'iasoon-javascript)
(require 'iasoon-python)
