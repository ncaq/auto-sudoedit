;; -*- lexical-binding: t -*-
(require 'tramp)

(defun tramp-path (s)
  (concat "/sudo::" s))

(defun sudoedit (s)
  (interactive "i")
  (if s
      (find-file (tramp-path s))
    (let ((current-path (or (buffer-file-name) list-buffers-directory)))
      (find-file (tramp-path current-path)))))

(defun sudoedit-and-close ()
  (interactive)
  (let ((old-buffer-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (sudoedit old-buffer-name)))

(defun auto-sudoedit-and-close ()
  (if (or (file-writable-p (buffer-file-name))
          (tramp-tramp-file-p (buffer-file-name)))
      ()
    (sudoedit-and-close)))

(add-hook 'find-file-hook 'auto-sudoedit-and-close)

(provide 'root-tramp)
