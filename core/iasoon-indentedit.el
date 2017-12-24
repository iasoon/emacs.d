(defun indentedit-line-blank-p ()
  (save-excursion
    (back-to-indentation)
    (eolp)))

(defun indentedit-next ()
  (interactive)
  (let ((pos-indent (current-indentation)))
    (while (and (zerop (forward-line))
             (or (indentedit-line-blank-p)
               (> (current-indentation) pos-indent))))
    (back-to-indentation)))

(defun indentedit-prev ()
  (interactive)
  (let ((pos-indent (current-indentation)))
    (while (and (zerop (forward-line -1))
             (or (indentedit-line-blank-p)
               (> (current-indentation) pos-indent))))
    (back-to-indentation)))

(define-minor-mode indentedit-mode
  :lighter "indentedit"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-n") 'indentedit-next)
            (define-key map (kbd "M-p") 'indentedit-prev)
            map))

(provide 'iasoon-indentedit)
