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

(defun sudoedit-and-close ()
  (interactive)
  (let ((old-buffer-name (current-path)))
    (kill-this-buffer)
    (sudoedit-1 old-buffer-name)))

(defun auto-sudoedit-and-close ()
  (if (or (f-writable? (current-path))
          (tramp-tramp-file-p (current-path)))
      ()
    (sudoedit-and-close)))

(add-hook 'find-file-hook  'auto-sudoedit-and-close)
(add-hook 'dired-mode-hook 'auto-sudoedit-and-close)

(provide 'root-tramp)