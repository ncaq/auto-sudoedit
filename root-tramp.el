;; -*- lexical-binding: t -*-
(require 'tramp)
(require 'f)

(defun tramp-path (s)
  (concat "/sudo::" s))

(defun current-path ()
  (or (buffer-file-name) list-buffers-directory))

(defun sudoedit (s)
  (interactive "i")
  (if s
      (find-file (tramp-path s))
    (find-file (tramp-path (current-path)))))

(defun sudoedit-and-close ()
  (interactive)
  (let ((old-buffer-name (current-path)))
    (kill-this-buffer)
    (sudoedit old-buffer-name)))

(defun auto-sudoedit-and-close ()
  (if (or (f-writable? (current-path))
          (tramp-tramp-file-p (current-path)))
      ()
    (sudoedit-and-close)))

(add-hook 'find-file-hook  'auto-sudoedit-and-close)
(add-hook 'dired-mode-hook 'auto-sudoedit-and-close)

(provide 'root-tramp)