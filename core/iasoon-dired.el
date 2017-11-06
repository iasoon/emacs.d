(require 'dired)

(define-key dired-mode-map "f" 'counsel-file-jump)

(setq dired-listing-switches "-laGh1v --group-directories-first")

(use-package dired-ranger
  :bind (:map dired-mode-map
              ("c" . dired-ranger-copy)
              ("y" . dired-ranger-paste)))

(provide 'iasoon-dired)
