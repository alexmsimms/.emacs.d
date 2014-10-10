;; Initialize Package Manager ;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
;; Package Manager Initialized ;;

;; Guarantee all packages are installed on start
(require 'cl)
(defvar packages-list
  '(helm
    magit
    auctex
    paredit
    nyan-mode
    smart-mode-line
    monokai-theme)
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

;; Set Emacs Path from shell
(when (memq window-system '(mac ns))
  (add-to-list 'packages-list "exec-path-from-shell")
  (exec-path-from-shell-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
	("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(electric-pair-delete-adjacent-pairs t)
 '(electric-pair-mode t)
 '(sml/position-percentage-format "")
 '(sml/replacer-regexp-list
   (quote
	(("^~/org/" ":Org:")
	 ("^~/\\.emacs\\.d/" ":ED:")
	 ("^/sudo:.*:" ":SU:")
	 ("^~/Documents/" ":Doc:")
	 ("^~/Dropbox/" ":DB:")
	 ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
	 ("^~/[Gg]it/" ":Git:")
	 ("^~/[Gg]it[Hh]ub/" ":Git:")
	 ("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")
	 (":Doc:Cloud/" ":Cloud:"))))
 '(tab-width 4)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; UI Enhancements ;;
(when (member "Source Code Pro" (font-family-list))
  (set-default-font "Source Code Pro 12"))
(load-theme 'monokai t)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq-default indicate-empty-lines t)
(show-paren-mode 1)
(column-number-mode 1)


(sml/setup)
(sml/apply-theme 'automatic)

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier nil)

(setq inhibit-startup-screen t)

;; UI Enhanced ;;

;; TRAMP Settings ;;
(setq tramp-default-method "ssh")
(setq tramp-default-user "asimms1")
;; TRAMP Set ;;

(load "~/.emacs.d/settings/helm.elc")

;; Nyan Mode ;;
(setq nyan-bar-length 16)
(nyan-mode)

(setq-default ispell-program-name "/usr/local/bin/aspell")
(setq-default ispell-list-command "list")


;; make backup to a designated dir, mirroring the full path
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/backup/")
	 (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath ))
	 (backupFilePath (replace-regexp-in-string "//" "/" (concat
														 backupRootDir filePath "~"))))
    (make-directory (file-name-directory
					 backupFilePath) (file-name-directory backupFilePath))
	
    backupFilePath))

(setq make-backup-file-name-function 'my-backup-file-name)


(setq save-place-file "~/.emacs.d/saved-places")
(require 'saveplace)
(setq-default save-place t)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(set-cursor-color "#FD971F")

