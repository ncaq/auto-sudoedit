;;; auto-sudoedit.el --- auto sudo edit by tramp -*- lexical-binding: t -*-

;; Author: ncaq <ncaq@ncaq.net>
;; Version: 0.0.0
;; Package-Requires: ((emacs "24.4")(f "0.19.0"))
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

(defun auto-sudoedit-sudoedit (s)
  "Open sudoedit.  Argument S is path."
  (interactive (auto-sudoedit-current-path))
  (find-file (auto-sudoedit-tramp-path s)))

(defun auto-sudoedit-should-activate (curr-path)
  "Return non-nil if auto-sudoedit should activate for CURR-PATH."
  (not
   (or
    ;; Don't activate for tramp files
    (tramp-tramp-file-p curr-path)
    ;; Don't activate on sudo do not exist
    (not (executable-find "sudo")))))

(defun auto-sudoedit (orig-func &rest args)
  "`auto-sudoedit' around-advice."
  (let ((curr-path (car args)))
    (if (auto-sudoedit-should-activate curr-path)
        ;; Current path may not exist; back up to the first existing parent
        ;; and see if it's writable
        (let ((first-existing-path (f-traverse-upwards #'f-exists? curr-path)))
          (if (not (and first-existing-path (f-writable? first-existing-path)))
              (let ((tramp-path (auto-sudoedit-tramp-path curr-path)))
                (apply orig-func tramp-path (cdr args)))
            (apply orig-func args)))
      (apply orig-func args))))

;;;###autoload
(define-minor-mode
  auto-sudoedit-mode
  "automatic do sudo by tramp when need root file"
  :init-value 0
  :lighter " ASE"
  (if auto-sudoedit-mode
      (progn
        (advice-add 'find-file :around 'auto-sudoedit)
        (advice-add 'dired :around 'auto-sudoedit))
    (advice-remove 'find-file 'auto-sudoedit)
    (advice-remove 'dired 'auto-sudoedit)))

(provide 'auto-sudoedit)

;;; auto-sudoedit.el ends here
