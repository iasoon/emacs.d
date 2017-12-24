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


;;
;; Theme
;;

(use-package nord-theme
  :config
  (setq nord-comment-brightness 15)
  (setq nord-uniform-mode-lines t)
  (load-theme 'nord t))


(let ((class '((class color) (min-colors 89)))
  (nord0 (if (display-graphic-p) "#2E3440" nil))
  (nord1 (if (display-graphic-p) "#3B4252" "black"))
  (nord2 (if (display-graphic-p) "#434C5E" "#434C5E"))
  (nord3 (if (display-graphic-p) "#4C566A" "brightblack"))
  (nord4 (if (display-graphic-p) "#D8DEE9" "#D8DEE9"))
  (nord5 (if (display-graphic-p) "#E5E9F0" "white"))
  (nord6 (if (display-graphic-p) "#ECEFF4" "brightwhite"))
  (nord7 (if (display-graphic-p) "#8FBCBB" "cyan"))
  (nord8 (if (display-graphic-p) "#88C0D0" "brightcyan"))
  (nord9 (if (display-graphic-p) "#81A1C1" "blue"))
  (nord10 (if (display-graphic-p) "#5E81AC" "brightblue"))
  (nord11 (if (display-graphic-p) "#BF616A" "red"))
  (nord12 (if (display-graphic-p) "#D08770" "brightyellow"))
  (nord13 (if (display-graphic-p) "#EBCB8B" "yellow"))
  (nord14 (if (display-graphic-p) "#A3BE8C" "green"))
  (nord15 (if (display-graphic-p) "#B48EAD" "magenta")))

  (custom-theme-set-faces
   'nord
   ;; dired
   `(dired-directory ((,class (:foreground ,nord9))))
   `(dired-marked ((,class (:foreground ,nord13))))

   ;; ivy
   `(ivy-current-match ((t (:background ,nord2))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,nord0))))
   `(ivy-minibuffer-match-face-2 ((t (:foreground ,nord7))))
   `(ivy-minibuffer-match-face-3 ((t (:foreground ,nord8))))
   `(ivy-minibuffer-match-face-4 ((t (:foreground ,nord9))))

   ;; hydra
   `(hydra-face-blue ((t (:foreground ,nord10))))
   `(hydra-face-teal ((t (:foreground ,nord8))))
   `(hydra-face-red ((t (:foreground ,nord11))))
   `(hydra-face-pink ((t (:foreground ,nord15))))
   ))

(provide 'iasoon-appearance)
