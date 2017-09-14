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

(use-package which-key
  :config
  (which-key-mode +1))

;; better scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Automatically update unmodified buffers whose files have changed.
(global-auto-revert-mode 1)

;; Don't save backups to current directory
(setq backup-directory-alist
      '(("." . "~/.emacs.d/bak")))

(use-package hungry-delete
  :config (global-hungry-delete-mode)
  :diminish (hungry-delete-mode))

(use-package crux
  :bind
  ("C-a" . crux-move-beginning-of-line)
  ("C-k" . crux-smart-kill-line))

(auto-fill-mode 1)
(setq comment-auto-fill-only-comments 1)
(setq-default fill-column 80)

(use-package avy
  :config
  ;; dvorak home row keys
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  (setq avy-timeout-seconds 0.1)
  :bind ("M-s" . avy-goto-char-timer))

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
