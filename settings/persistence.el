;; make backup to a designated dir, mirroring the full path
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/backup/")
		 (filePath
		  (replace-regexp-in-string "[A-Za-z]:" "" fpath ))
		 (backupFilePath
		  (replace-regexp-in-string "//" "/" (concat
											  backupRootDir filePath "~"))))
    (make-directory (file-name-directory
					 backupFilePath) (file-name-directory backupFilePath))
	
    backupFilePath))

(setq make-backup-file-name-function 'my-backup-file-name)


(setq save-place-file "~/.emacs.d/saved-places")
(require 'saveplace)
(setq-default save-place t)
