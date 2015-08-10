;; Initialize Package Manager ;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
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
    clojure-mode
    clojure-mode-extra-font-locking
    cider
	
    smart-mode-line
    monokai-theme
    )
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
	("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
	 "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f"
	 default)))
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$|^\\.")
 '(dired-use-ls-dired nil)
 '(electric-pair-delete-adjacent-pairs t)
 '(electric-pair-mode t)
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "-")
 '(glasses-uncapitalize-p t)
 '(helm-split-window-default-side (quote other))
 '(initial-scratch-message nil)
 '(magit-use-overlays nil)
 '(org-agenda-files (quote ("~/agenda/agenda.org")))
 '(org-highlight-latex-and-related (quote (latex script entities)))
 '(rm-blacklist (quote (" hl-p" " Helm")))
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
	 (":Doc:Cloud/" ":Cloud:")
	 (":Doc:phonphon/" ":ΦΦ:")
	 ("homework" "hw")
	 ("^/ssh:asimms1@.*cs.swarthmore.edu:/" ":CS:")
	 (":CS:home/asimms1" ":CS:~")
	 (":Doc:algorithms/?" ":Algorithms:")
	 ("~/Google Drive/?" ":GD:")
	 (":GD:code.pyret.org/?" ":CPO:"))))
 '(tab-width 4)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun load-settings (name)
  (load (format "~/.emacs.d/settings/%s.el" name)))

(load-settings "elisp")
(load-settings "clojure")
(load-settings "editing")
(load-settings "helm")
(load-settings "persistence")
(load-settings "ui")
(load-settings "misc")



