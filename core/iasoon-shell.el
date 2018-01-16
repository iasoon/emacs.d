(require 'shell)

;; Open project shell or global shell
(defun iasoon-shell ()
  (interactive)
  (if (projectile-project-p)
      (projectile-run-shell)
    (shell)))

;; Track shell pwd through procfs for maximum accuracy
;; This way, 360-noscoping directories is possible
(defun track-shell-directory-procfs ()
  (shell-dirtrack-mode 0)
  (add-hook 'comint-preoutput-filter-functions
            (lambda (str)
              (prog1 str
                (when (string-match comint-prompt-regexp str)
                  (cd (file-symlink-p
                       (format "/proc/%s/cwd" (process-id
                                               (get-buffer-process
                                                (current-buffer)))))))))
            nil t))

(add-hook 'shell-mode-hook 'track-shell-directory-procfs)


;; TODO maybe make this a general utility function
;; TODO this is no longer neccesary here, but I'm leaving this here because
;; it might prove useful later on.
(defun override-local-minor-mode-key (mode key def)
  "Override a minor mode keybinding for the current buffer"
  (let* ((oldmap (cdr (assoc mode minor-mode-map-alist)))
         (newmap (or (cdr (assoc mode minor-mode-overriding-map-alist))
                     (let ((map (make-sparse-keymap)))
                       (set-keymap-parent map oldmap)
                       (push `(,mode . ,map) minor-mode-overriding-map-alist)
                       map))))
    (define-key newmap key def)))


(bind-key* "C-c C-s" 'iasoon-shell)

(bind-key "TAB" 'company-complete-common shell-mode-map)

(provide 'iasoon-shell)
