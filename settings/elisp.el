(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
	(kill-sexp -1)
	(insert (format "%S" value))))

(global-set-key (kbd "C-c e") 'replace-last-sexp)
