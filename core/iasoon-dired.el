(define-key dired-mode-map "f" 'counsel-file-jump)

(use-package dired-ranger
  :bind (:map dired-mode-map
              ("c" . dired-ranger-copy)
              ("y" . dired-ranger-paste)))

(provide 'iasoon-dired)
