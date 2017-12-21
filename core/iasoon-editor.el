;; avoid buffers with the same name
(use-package uniquify
  ;; uniquify package is built-in
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; leave special buffers alone
  (setq uniquify-ignore-buffers-re "^\\*")
  )

(use-package smartparens
  :demand
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)

  (sp-pair "{" nil :post-handlers'(("||\n[i]" "RET")
                                   ("| " "SPC")))

  (show-smartparens-global-mode +1)
  (smartparens-global-mode +1)
  :bind (("C-M-k" . sp-kill-hybrid-sexp))
  :diminish smartparens-mode)

(use-package expand-region
  :bind ("C-r" . er/expand-region))

(use-package hydra)

(use-package multiple-cursors)
  
(defhydra multiple-cursors-hydra (:hint nil)
  "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_s_] Sort regions
^ ^             ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("s" mc/sort-regions)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil))



(global-set-key (kbd "C-=") 'multiple-cursors-hydra/body)

(provide 'iasoon-editor)
