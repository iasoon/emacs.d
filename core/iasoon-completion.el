;; Use company as completion front-end

(use-package company
  :config
  (global-company-mode)

  (setq company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-idle-delay 0.2
        company-require-match nil
        ;; align annotations to right hand side
        company-tooltip-align-annotations t)
  (add-to-list 'company-frontends 'company-tng-frontend)
  (company-tng-configure-default)
  (define-key company-active-map (kbd "<tab>") 'company-select-next)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)

  (setq tab-always-indent 'complete)

  (defvar completion-at-point-functions-saved nil)

  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let ((completion-at-point-functions-saved completion-at-point-functions)
          (completion-at-point-functions '(company-complete-common-wrapper)))
      (indent-for-tab-command arg)))

  (defun company-complete-common-wrapper ()
    (let ((completion-at-point-functions completion-at-point-functions-saved))
      (company-complete-common)))

  :bind (:map company-mode-map
         ("<tab>" . company-indent-for-tab-command)))

;; ycmd
(use-package ycmd
  :config
  (setq ycmd-server-command `("python" ,(file-truename "~/src/ycmd/ycmd/")))
  (setq ycmd-rust-src-path (file-truename "~/src/rust/src/"))
  :diminish ycmd-mode)

(use-package company-ycmd
  :after ycmd
  :config (company-ycmd-setup))

(provide 'iasoon-completion)
