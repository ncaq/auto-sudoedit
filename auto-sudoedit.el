;; -*- lexical-binding: t -*-

;; Description: auto sudo edit by tramp when find not writable file
;; Author: ncaq <ncaq@ncaq.net>
;; Maintainer: ncaq <ncaq@ncaq.net>
;; Version: 0.0.0
;; URL: https://github.com/ncaq/auto-sudoedit
;; Package-Requires: ((f "0.19.0"))

(require 'f)
(require 'tramp)

(defun tramp-path (s)
  (concat "/sudo::" s))

(defun current-path ()
  (or (buffer-file-name) list-buffers-directory))

(defun sudoedit (s)
  (interactive (current-path))
  (find-file (tramp-path s)))

(defun sudoedit-and-kill ()
  (interactive)
  (let ((old-buffer-name (current-path)))
    (kill-this-buffer)
    (sudoedit old-buffer-name)))

;;;###autoload
(defun auto-sudoedit ()
  (if (or (f-writable? (current-path))
          (tramp-tramp-file-p (current-path)))
      ()
    (sudoedit-and-kill)))

;;;###autoload
(add-hook 'find-file-hook  'auto-sudoedit)
;;;###autoload
(add-hook 'dired-mode-hook 'auto-sudoedit)

(provide 'auto-sudoedit)
