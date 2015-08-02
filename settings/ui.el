;;(set-default-font "Source Code Pro 11")
(load-theme 'monokai t)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq-default indicate-empty-lines t)
(show-paren-mode 1)
(column-number-mode 1)
(set-cursor-color "#FD971F")
(setq inhibit-startup-screen t)

(sml/setup)
(sml/apply-theme 'automatic)
(rich-minority-mode)


(defun prev-window ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x C-n") 'prev-window)

(require 'windmove)
(windmove-default-keybindings 'super)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

