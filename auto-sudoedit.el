;;; auto-sudoedit.el --- auto sudo edit by tramp -*- lexical-binding: t -*-

;; Author: ncaq <ncaq@ncaq.net>
;; Version: 0.0.0
;; Package-Requires: ((emacs "24")(f "0.19.0"))
;; URL: https://github.com/ncaq/auto-sudoedit

;;; Commentary:

;; when find-file-hook and dired-mode-hook, and current path not writable
;; re-open tramp sudo edit automatic

;;; Code:

(require 'f)
(require 'tramp)

(defun auto-sudoedit-tramp-path (s)
  "Argument S is tramp sudo path."
  (concat "/sudo::" s))

(defun auto-sudoedit-current-path ()
  "Current path file or dir."
  (or (buffer-file-name) list-buffers-directory))

(defun auto-sudoedit-sudoedit (s)
  "Open sudoedit.  Argument S is path."
  (interactive (auto-sudoedit-current-path))
  (find-file (auto-sudoedit-tramp-path s)))

(defun auto-sudoedit-sudoedit-and-kill ()
  "Open sudoedit and kill."
  (interactive)
  (let ((old-buffer-name (auto-sudoedit-current-path)))
    (kill-this-buffer)
    (auto-sudoedit-sudoedit old-buffer-name)))

(defun auto-sudoedit ()
  "`auto-sudoedit' hook."
  (unless (or
           (f-writable? (auto-sudoedit-current-path))
           (tramp-tramp-file-p (auto-sudoedit-current-path)))
    (auto-sudoedit-sudoedit-and-kill)))

;;;###autoload
(define-minor-mode
  auto-sudoedit-mode
  "automatic do sudo by tramp when need root file"
  :init-value 1
  :lighter " ASE"
  (if auto-sudoedit-mode
      (progn
        (add-hook 'find-file-hook  'auto-sudoedit)
        (add-hook 'dired-mode-hook 'auto-sudoedit)
        )
    (remove-hook 'find-file-hook  'auto-sudoedit)
    (remove-hook 'dired-mode-hook 'auto-sudoedit)
    ))

(provide 'auto-sudoedit)

;;; auto-sudoedit.el ends here
