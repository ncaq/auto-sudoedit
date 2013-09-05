;;; -*- coding: utf-8; lexical-binding: t -*-

;;本当は,ファイル開く→前のファイル消す という順番にしたい けれど,名前を変えた後のBufferを,Uniq識別方法が違っても消す方法がわからない.
(defun sudoedit-and-close ()
  (interactive)
  (setq nowrite-buffer-name (buffer-name))
  (setq nowrite-file-name (buffer-file-name))
  (setq write-file-name (concat "/sudo::" nowrite-file-name))
  (kill-buffer nowrite-buffer-name)
  (find-file write-file-name))
