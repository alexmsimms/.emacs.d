(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "M-C") 'toggle-comment-on-line)

(setq-default ispell-program-name "aspell")
(setq-default ispell-list-command "list")

(global-set-key (kbd "C-c e") 'replace-last-sexp )
