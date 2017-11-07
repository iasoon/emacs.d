(require 'dired)
(require 'dired-x)

(define-key dired-mode-map "f" 'counsel-file-jump)

(setq dired-listing-switches "-laGh1v --group-directories-first")

;; don't prompt for recursive actions
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)


(use-package dired-ranger
  :bind (:map dired-mode-map
              ("c" . dired-ranger-copy)
              ("y" . dired-ranger-paste)))

(provide 'iasoon-dired)
