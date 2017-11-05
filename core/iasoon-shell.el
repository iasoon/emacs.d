;; Open project shell or global shell
(defun iasoon-shell ()
1  (interactive)
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

(bind-key "C-c C-s" 'iasoon-shell)

(provide 'iasoon-shell)
