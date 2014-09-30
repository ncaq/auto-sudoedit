;; -*- lexical-binding: t -*-

(require 'tramp)

(require 'f)

(defun tramp-path (s)
  (concat "/sudo::" s))

(defun current-path ()
  (or (buffer-file-name) list-buffers-directory))

(defun sudoedit ()
  (interactive)
  (find-file (tramp-path (current-path))))

(defun sudoedit-1 (s)
  (find-file (tramp-path s)))

(defun sudoedit-and-kill ()
  (interactive)
  (let ((old-buffer-name (current-path)))
    (kill-this-buffer)
    (sudoedit-1 old-buffer-name)))

(defun auto-sudoedit ()
  (if (or (f-writable? (current-path))
          (tramp-tramp-file-p (current-path)))
      ()
    (sudoedit-and-kill)))

(add-hook 'find-file-hook  'auto-sudoedit t)
(add-hook 'dired-mode-hook 'auto-sudoedit t)

(provide 'auto-sudoedit)
