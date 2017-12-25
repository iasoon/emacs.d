(defun indentedit-line-blank-p ()
  (save-excursion
    (back-to-indentation)
    (eolp)))

(defun indentedit-forward (&optional direction)
  (let ((direction (or direction (direction)))
        (pos-indent (current-indentation)))
    (while (and (zerop (forward-line direction))
             (or (indentedit-line-blank-p)
               (> (current-indentation) pos-indent))))))

(defun indentedit--opening-sexp ()
  (save-excursion
    (let ( (beg (line-beginning-position))
           (end (line-end-position))
           (sexp t))
      (move-beginning-of-line nil)
      (while (progn
               (setq sexp (sp-get-thing))
               (when sexp (goto-char (sp-get sexp :end)))
               (and sexp (>= end (point)))))
      (when (<= beg (sp-get sexp :beg) end) sexp))))

(defun indentedit--forward-line ()
  (interactive)
  (if-let ((sexp (indentedit--opening-sexp)))
    (goto-char (sp-get sexp :end))
    (let ((ok (zerop (forward-line 1))))
      (when ok (back-to-indentation))
      ok)))

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