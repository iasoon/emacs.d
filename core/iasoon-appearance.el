;;
;; General settings
;;

;; Fonts
(set-frame-font "Source Code Pro 11" nil t)
(set-face-bold-p 'bold nil)

;; Claim back valuable real estate
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; This is super annoying
(blink-cursor-mode -1)

(use-package nord-theme
  :config
  (setq nord-comment-brightness 15)
  (setq nord-uniform-mode-lines t)
  (load-theme 'nord t))

;;
;; Fringes
;;

(use-package fringe-helper)

;; more subtle git-gutter marks
(eval-after-load 'git-gutter-fringe
  '(progn
     (fringe-helper-define 'git-gutter-fr:added '(center repeated)
       "XX......")
     (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
       "XX......")
     (fringe-helper-define 'git-gutter-fr:deleted 'bottom
       "X......."
       "XX......"
       "XXX....."
       "XXXX....")))

(provide 'iasoon-appearance)
