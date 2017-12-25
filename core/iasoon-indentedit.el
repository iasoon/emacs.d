(defun indentedit-line-blank-p ()
  (save-excursion
    (back-to-indentation)
    (eolp)))

(defun indentedit-forward (&optional direction)
  (let ((direction (or direction 1))
        (pos-indent (current-indentation)))
    (while (and (zerop (forward-line direction))
             (or (indentedit-line-blank-p)
               (> (current-indentation) pos-indent))))
    (back-to-indentation)))

(defun indentedit-next ()
  (interactive)
  (indentedit-forward 1))

(defun indentedit-prev ()
  (interactive)
  (indentedit-forward -1))

(defun indentedit-up ()
  (interactive)
  (let ((pos-indent (current-indentation)))
    (while (and (zerop (forward-line -1))
             (or (indentedit-line-blank-p)
               (>= (current-indentation) pos-indent))))
    (back-to-indentation)))

(defun indentedit-mark ()
  (interactive)
  (set-mark (line-beginning-position))
  ;; move to next item, which is past the current one
  (indentedit-forward 1)
  ;; step back, skipping empty
  (while (and (zerop (forward-line -1))
              (indentedit-line-blank-p)))
  (move-end-of-line nil))



(define-minor-mode indentedit-mode
  :lighter "indentedit"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-n") 'indentedit-next)
            (define-key map (kbd "M-p") 'indentedit-prev)
            (define-key map (kbd "M-u") 'indentedit-up)
            (define-key map (kbd "M-m") 'indentedit-mark)
            map))



;; TOOD
;; The idea here is to consider a line with a hanging sexp a single line.
;; I suspect this will be well-suited for editing c-style languages.
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

(provide 'iasoon-indentedit)
