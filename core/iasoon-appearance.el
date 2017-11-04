;;
;; General settings
;;

;; Fonts
(defun set-font (font-str)
  "Set the default font to the FONT-STR parameter."
  (add-to-list 'default-frame-alist '(font-str))
  (set-face-attribute 'default t :font font-str))

(set-font "Fira Mono 12")



;; Claim back valuable real estate
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; This is super annoying
(blink-cursor-mode -1)

;; pwetty colours!
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; Show line and column number in modeline
(line-number-mode t)
(column-number-mode t)


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
