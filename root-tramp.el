(require 'tramp)
(require 'dired)

(defun sudoedit (source-path)
  (let ((tramp-path (concat "/sudo::" source-path)))
    (find-file tramp-path)))

(defun sudoedit-and-close ()
  (interactive)
  (let ((kill-buffer-name (buffer-name))
	(current-mode major-mode)
	(file-name (buffer-file-name))
	(dir-name (ignore-errors (dired-current-directory))))
    (kill-buffer kill-buffer-name)
    (if (eq current-mode 'dired-mode)
	(sudoedit dir-name)
      (sudoedit file-name))))

(defun auto-tramp-file ()
  (unless (or (string-match "^/sudo:" (buffer-file-name))
	      (file-writable-p (buffer-file-name)))
    (sudoedit-and-close)))

(add-hook 'find-file-hook 'auto-tramp-file)

(provide 'root-tramp)
