;; Initialize Package Manager ;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
			 '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
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
    monokai-theme
    slime
    tuareg ;; \
    utop   ;;  > OCAML
    merlin ;; /
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

;; Set Emacs Path from shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (setq mac-option-key-is-meta nil
		mac-command-key-is-meta t
		mac-command-modifier 'meta
		mac-option-modifier 'super))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(custom-safe-themes (quote ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$|^\\.")
 '(dired-use-ls-dired nil)
 '(electric-pair-delete-adjacent-pairs t)
 '(electric-pair-mode t)
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "-")
 '(glasses-uncapitalize-p t)
 '(helm-split-window-default-side (quote other))
 '(glasses-separator "-")
 '(initial-scratch-message nil)
 '(magit-use-overlays nil)
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

;; UI Enhancements ;;
(when (member "Source Code Pro" (font-family-list))
  (set-default-font "Source Code Pro 11"))
(load-theme 'monokai t)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq-default indicate-empty-lines t)
(show-paren-mode 1)
(column-number-mode 1)


(sml/setup)
(sml/apply-theme 'automatic)
(rich-minority-mode)


;; From the "mastering emacs" guy
(defvar mode-line-cleaner-alist
  `((yas/minor-mode . " υ")
    (paredit-mode . " π")
    (eldoc-mode . "")
    (abbrev-mode . "")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (emacs-lisp-mode . "EL")
	(python-mode . "PY" )
	(pyret-mode . "ARR"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")


(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))


(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(setq inhibit-startup-screen t)

;; UI Enhanced ;;

;; TRAMP Settings ;;
(setq tramp-default-method "ssh")
(setq tramp-default-user "asimms1")
;; TRAMP Set ;;

(load "~/.emacs.d/settings/helm.elc")
(load "~/.emacs.d/settings/pyret.el")



;; Nyan Mode ;;
(setq nyan-bar-length 16)
(nyan-mode)

(if (memq window-system '(mac ns))
	(setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "aspell"))

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

(defun prev-window ()
   (interactive)
   (other-window -1))
(global-set-key (kbd "C-x C-n") 'prev-window)
(global-set-key (kbd "C-x n") 'prev-window)



;; SLIME things
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (paredit-mode t)))
(setq inferior-lisp-program "sbcl")

(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(require 'windmove)
(windmove-default-keybindings 'super)


;; Further Ocaml things
(when (memq window-system '(mac ns))
 (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
 (setq auto-mode-alist
	   (append '(("\\.ml[ily]?$" . tuareg-mode)
				 ("\\.topml$" . tuareg-mode))
			   auto-mode-alist)) 
 (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
 (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
 (add-hook 'tuareg-mode-hook 'merlin-mode)
 (setq merlin-use-auto-complete-mode t)
 (setq merlin-error-after-save nil)

 (setq opam-share (substring (shell-command-to-string "opam config var share") 0 -1))
 (add-to-list 'load-path "/Users/alex/.opam/system/share/emacs/site-lisp")
 (require 'ocp-indent))


;; Haskell things
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

;; M-x haskell-mode-stylish-buffer in a repl

(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))


