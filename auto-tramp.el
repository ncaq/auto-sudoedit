;;; -*- coding: utf-8; lexical-binding: t -*-
(require 'tramp)
;;本当は,ファイル開く→前のファイル消す という順番にしたい けれど,名前を変えた後のBufferを,Uniq識別方法が違っても消す方法がわからない.
(defun sudoedit-and-close ()
  (interactive)
  (setq nowrite-buffer-name (buffer-name))
  (setq nowrite-file-name (buffer-file-name))
  (setq tramp-buffer-name (concat "/sudo::" nowrite-file-name))
  (kill-buffer nowrite-buffer-name)
  (find-file tramp-buffer-name)
  (message "sudoedit done"))

(defun auto-tramp ()
  (if
      (and
       (not (string-match "^/sudo:" (buffer-file-name)))
       (not (file-writable-p (buffer-file-name))))
      (sudoedit-and-close)))

(defun set-auto-tramp ()
  (add-hook 'find-file-hook 'auto-tramp))

(provide 'auto-tramp)
