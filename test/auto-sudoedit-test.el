;;; auto-sudoedit-test.el --- Tests for auto-sudoedit -*- lexical-binding: t -*-

;;; Commentary:

;; ERT tests for auto-sudoedit.

;;; Code:

(require 'ert)
(require 'auto-sudoedit)

(ert-deftest auto-sudoedit-path/local-other-user ()
  "Local path owned by another user should be converted to sudo path."
  (cl-letf (((symbol-function 'auto-sudoedit-file-owner) (lambda (_) "root"))
            ((symbol-function 'auto-sudoedit-current-user) (lambda (_) "ncaq")))
    (let ((result (auto-sudoedit-path "/etc/hosts")))
      (should (equal (car result) "root"))
      (should (equal (cdr result) "/sudo::/etc/hosts")))))

(ert-deftest auto-sudoedit-path/local-same-user ()
  "Local path owned by current user should not be converted."
  (cl-letf (((symbol-function 'auto-sudoedit-file-owner) (lambda (_) "ncaq"))
            ((symbol-function 'auto-sudoedit-current-user) (lambda (_) "ncaq")))
    (let ((result (auto-sudoedit-path "/home/ncaq/file.txt")))
      (should (null (car result)))
      (should (equal (cdr result) "/home/ncaq/file.txt")))))

(ert-deftest auto-sudoedit-path/owner-unknown ()
  "When file owner cannot be determined, should not convert."
  (cl-letf (((symbol-function 'auto-sudoedit-file-owner) (lambda (_) nil))
            ((symbol-function 'auto-sudoedit-current-user) (lambda (_) "ncaq")))
    (let ((result (auto-sudoedit-path "/some/path")))
      (should (null (car result)))
      (should (equal (cdr result) "/some/path")))))

(ert-deftest auto-sudoedit-path/already-sudo ()
  "Already a sudo tramp path should not be converted again."
  (cl-letf (((symbol-function 'auto-sudoedit-file-owner) (lambda (_) "root"))
            ((symbol-function 'auto-sudoedit-current-user) (lambda (_) "ncaq")))
    (let* ((sudo-path "/sudo::/etc/hosts")
           (result (auto-sudoedit-path sudo-path)))
      ;; auto-sudoedit-path-from-tramp-ssh-like returns curr-path when method is sudo,
      ;; so tramp-path equals curr-path, and the "not equal" check fails.
      (should (null (car result)))
      (should (equal (cdr result) sudo-path)))))

(ert-deftest auto-sudoedit-path/ssh-other-user ()
  "SSH tramp path owned by another user should be converted to sudo."
  (cl-letf (((symbol-function 'auto-sudoedit-file-owner) (lambda (_) "root"))
            ((symbol-function 'auto-sudoedit-current-user) (lambda (_) "ncaq")))
    (let ((result (auto-sudoedit-path "/ssh:host:/etc/hosts")))
      (should (equal (car result) "root"))
      ;; The result should use sudo method for the target path.
      (should (string-match-p "sudo:" (cdr result)))
      (should (string-match-p "root@" (cdr result)))
      (should (string-match-p "/etc/hosts\\'" (cdr result))))))

(ert-deftest auto-sudoedit-path/ssh-same-user ()
  "SSH tramp path owned by current user should not be converted."
  (cl-letf (((symbol-function 'auto-sudoedit-file-owner) (lambda (_) "ncaq"))
            ((symbol-function 'auto-sudoedit-current-user) (lambda (_) "ncaq")))
    (let ((result (auto-sudoedit-path "/ssh:host:/home/ncaq/file")))
      (should (null (car result)))
      (should (equal (cdr result) "/ssh:host:/home/ncaq/file")))))

(ert-deftest auto-sudoedit-path-from-tramp-ssh-like/basic-ssh ()
  "Basic SSH path should be converted to sudo."
  (let ((result (auto-sudoedit-path-from-tramp-ssh-like "/ssh:example.com:/etc/hosts" "root")))
    (should (string-match-p "sudo:" result))
    (should (string-match-p "root@" result))
    (should (string-match-p "example\\.com" result))
    (should (string-match-p "/etc/hosts\\'" result))))

(ert-deftest auto-sudoedit-path-from-tramp-ssh-like/ssh-with-user ()
  "SSH path with explicit user should be converted to sudo."
  (let ((result (auto-sudoedit-path-from-tramp-ssh-like "/ssh:admin@example.com:/etc/hosts" "root")))
    (should (string-match-p "sudo:" result))
    (should (string-match-p "root@" result))
    (should (string-match-p "example\\.com" result))
    (should (string-match-p "/etc/hosts\\'" result))))

(ert-deftest auto-sudoedit-path-from-tramp-ssh-like/ssh-with-port ()
  "SSH path with port should be converted to sudo preserving the port."
  (let ((result (auto-sudoedit-path-from-tramp-ssh-like "/ssh:example.com#2222:/etc/hosts" "root")))
    (should (string-match-p "sudo:" result))
    (should (string-match-p "root@" result))
    (should (string-match-p "example\\.com" result))
    (should (string-match-p "#2222" result))
    (should (string-match-p "/etc/hosts\\'" result))))

(ert-deftest auto-sudoedit-path-from-tramp-ssh-like/already-sudo ()
  "Path with sudo method should be returned as-is."
  (let* ((sudo-path "/sudo:root@localhost:/etc/hosts")
         (result (auto-sudoedit-path-from-tramp-ssh-like sudo-path "root")))
    (should (equal result sudo-path))))

(ert-deftest auto-sudoedit-path-from-tramp-ssh-like/scp-method ()
  "SCP method should also be converted to sudo."
  (let ((result (auto-sudoedit-path-from-tramp-ssh-like "/scp:example.com:/etc/hosts" "root")))
    (should (string-match-p "sudo:" result))
    (should (string-match-p "root@" result))
    (should (string-match-p "example\\.com" result))
    (should (string-match-p "/etc/hosts\\'" result))))

(ert-deftest auto-sudoedit-path-from-tramp-ssh-like/multi-hop ()
  "Multi-hop path should be converted to sudo with target host."
  (let ((result (auto-sudoedit-path-from-tramp-ssh-like "/ssh:jump|ssh:target:/etc/hosts" "root")))
    (should (string-match-p "sudo:" result))
    (should (string-match-p "root@" result))
    (should (string-match-p "target" result))
    (should (string-match-p "/etc/hosts\\'" result))))

(ert-deftest auto-sudoedit-current-path/file-buffer ()
  "In a file buffer, should return variable `buffer-file-name'."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/test-file.el")
    (should (equal (auto-sudoedit-current-path) "/tmp/test-file.el"))))

(ert-deftest auto-sudoedit-current-path/dired-buffer ()
  "In a dired-like buffer, should return `list-buffers-directory'."
  (with-temp-buffer
    (setq buffer-file-name nil)
    (setq list-buffers-directory "/tmp/test-dir/")
    (should (equal (auto-sudoedit-current-path) "/tmp/test-dir/"))))

(ert-deftest auto-sudoedit-current-path/no-path ()
  "In a buffer with no file and no directory, should return nil."
  (with-temp-buffer
    (setq buffer-file-name nil)
    (setq list-buffers-directory nil)
    (should (null (auto-sudoedit-current-path)))))

(ert-deftest auto-sudoedit-current-user/local-path ()
  "For a local path, should return function `user-login-name'."
  (should (equal (auto-sudoedit-current-user "/etc/hosts") (user-login-name))))

(ert-deftest auto-sudoedit-current-user/tramp-path ()
  "For a tramp path, should return the remote user via function `tramp-get-remote-uid'."
  (cl-letf (((symbol-function 'tramp-get-remote-uid)
             (lambda (_ id-format)
               (when (eq id-format 'string)
                 "remoteuser"))))
    (should (equal (auto-sudoedit-current-user "/ssh:host:/etc/hosts") "remoteuser"))))

(ert-deftest auto-sudoedit-mode/enable-adds-hooks ()
  "Enabling the mode should add hooks."
  (unwind-protect
      (progn
        (auto-sudoedit-mode 1)
        (should (memq #'auto-sudoedit find-file-hook))
        (should (memq #'auto-sudoedit dired-mode-hook)))
    (auto-sudoedit-mode -1)))

(ert-deftest auto-sudoedit-mode/disable-removes-hooks ()
  "Disabling the mode should remove hooks."
  (auto-sudoedit-mode 1)
  (auto-sudoedit-mode -1)
  (should-not (memq #'auto-sudoedit find-file-hook))
  (should-not (memq #'auto-sudoedit dired-mode-hook)))

(ert-deftest auto-sudoedit-file-owner/existing-file ()
  "For an existing file, should return a string (the owner name)."
  ;; /etc/hosts should exist on any Unix-like system.
  (let ((owner (auto-sudoedit-file-owner "/etc/hosts")))
    (should (stringp owner))))

(ert-deftest auto-sudoedit-file-owner/nonexistent-file ()
  "For a nonexistent file, should return nil."
  (should (null (auto-sudoedit-file-owner "/nonexistent/path/file"))))

(ert-deftest auto-sudoedit-current-path/file-takes-priority ()
  "When both variable `buffer-file-name' and variable `list-buffers-directory' are set, file takes priority."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/file.el")
    (setq list-buffers-directory "/tmp/dir/")
    (should (equal (auto-sudoedit-current-path) "/tmp/file.el"))))

(ert-deftest auto-sudoedit-sudoedit/calls-find-file-with-sudo-path ()
  "Function `auto-sudoedit-sudoedit' should call function `find-file' with the converted sudo path."
  (let (find-file-called-with)
    (cl-letf (((symbol-function 'auto-sudoedit-file-owner) (lambda (_) "root"))
              ((symbol-function 'auto-sudoedit-current-user) (lambda (_) "ncaq"))
              ((symbol-function 'find-file) (lambda (path) (setq find-file-called-with path))))
      (auto-sudoedit-sudoedit "/etc/hosts")
      (should (equal find-file-called-with "/sudo::/etc/hosts")))))

(ert-deftest auto-sudoedit-sudoedit/same-user-opens-original ()
  "Function `auto-sudoedit-sudoedit' with same user should open the original path."
  (let (find-file-called-with)
    (cl-letf (((symbol-function 'auto-sudoedit-file-owner) (lambda (_) "ncaq"))
              ((symbol-function 'auto-sudoedit-current-user) (lambda (_) "ncaq"))
              ((symbol-function 'find-file) (lambda (path) (setq find-file-called-with path))))
      (auto-sudoedit-sudoedit "/home/ncaq/file.txt")
      (should (equal find-file-called-with "/home/ncaq/file.txt")))))

(ert-deftest auto-sudoedit/file-buffer-not-writable ()
  "Hook should reopen file via sudo when owned by a different user."
  (with-temp-buffer
    (setq buffer-file-name "/etc/hosts")
    (let ((auto-sudoedit-ask nil)
          (recentf-list (list "/etc/hosts"))
          revert-buffer-args)
      (cl-letf (((symbol-function 'auto-sudoedit-file-owner) (lambda (_) "root"))
                ((symbol-function 'auto-sudoedit-current-user) (lambda (_) "ncaq"))
                ((symbol-function 'tramp-tramp-file-p) (lambda (_) nil))
                ((symbol-function 'set-visited-file-name) (lambda (path &optional _) (setq buffer-file-name path)))
                ((symbol-function 'revert-buffer) (lambda (&rest args) (setq revert-buffer-args args))))
        (auto-sudoedit)
        (should (equal buffer-file-name "/sudo::/etc/hosts"))
        ;; The old path should be removed from recentf-list.
        (should (null recentf-list))
        ;; revert-buffer should be called with (t t) to skip confirmation.
        (should (equal revert-buffer-args '(t t)))))))

(ert-deftest auto-sudoedit/same-user-no-change ()
  "Hook should not change anything when file is owned by current user."
  (with-temp-buffer
    (setq buffer-file-name "/home/ncaq/file.txt")
    (let ((auto-sudoedit-ask nil)
          (original-name buffer-file-name))
      (cl-letf (((symbol-function 'auto-sudoedit-file-owner) (lambda (_) "ncaq"))
                ((symbol-function 'auto-sudoedit-current-user) (lambda (_) "ncaq")))
        (auto-sudoedit)
        (should (equal buffer-file-name original-name))))))

(ert-deftest auto-sudoedit/nil-path-no-error ()
  "Hook should not error when current path is nil."
  (with-temp-buffer
    (setq buffer-file-name nil)
    (setq list-buffers-directory nil)
    (should-not (auto-sudoedit))))

(ert-deftest auto-sudoedit/ask-confirmed ()
  "Hook should proceed when variable `auto-sudoedit-ask' is t and user confirms."
  (with-temp-buffer
    (setq buffer-file-name "/etc/hosts")
    (let ((auto-sudoedit-ask t)
          (recentf-list (list "/etc/hosts"))
          revert-buffer-called)
      (cl-letf (((symbol-function 'auto-sudoedit-file-owner) (lambda (_) "root"))
                ((symbol-function 'auto-sudoedit-current-user) (lambda (_) "ncaq"))
                ((symbol-function 'tramp-tramp-file-p) (lambda (_) nil))
                ((symbol-function 'y-or-n-p) (lambda (_) t))
                ((symbol-function 'set-visited-file-name) (lambda (path &optional _) (setq buffer-file-name path)))
                ((symbol-function 'revert-buffer) (lambda (&rest _) (setq revert-buffer-called t))))
        (auto-sudoedit)
        (should (equal buffer-file-name "/sudo::/etc/hosts"))
        (should revert-buffer-called)))))

(ert-deftest auto-sudoedit/ask-denied ()
  "Hook should not proceed when variable `auto-sudoedit-ask' is t and user denies."
  (with-temp-buffer
    (setq buffer-file-name "/etc/hosts")
    (let ((auto-sudoedit-ask t)
          (original-name buffer-file-name))
      (cl-letf (((symbol-function 'auto-sudoedit-file-owner) (lambda (_) "root"))
                ((symbol-function 'auto-sudoedit-current-user) (lambda (_) "ncaq"))
                ((symbol-function 'tramp-tramp-file-p) (lambda (_) nil))
                ((symbol-function 'y-or-n-p) (lambda (_) nil)))
        (auto-sudoedit)
        (should (equal buffer-file-name original-name))))))

(ert-deftest auto-sudoedit/recentf-not-popped-when-different ()
  "Hook should not pop recentf-list when the top entry differs from current path."
  (with-temp-buffer
    (setq buffer-file-name "/etc/hosts")
    (let ((auto-sudoedit-ask nil)
          (recentf-list (list "/some/other/file"))
          revert-buffer-called)
      (cl-letf (((symbol-function 'auto-sudoedit-file-owner) (lambda (_) "root"))
                ((symbol-function 'auto-sudoedit-current-user) (lambda (_) "ncaq"))
                ((symbol-function 'tramp-tramp-file-p) (lambda (_) nil))
                ((symbol-function 'set-visited-file-name) (lambda (path &optional _) (setq buffer-file-name path)))
                ((symbol-function 'revert-buffer) (lambda (&rest _) (setq revert-buffer-called t))))
        (auto-sudoedit)
        (should (equal recentf-list (list "/some/other/file")))
        (should revert-buffer-called)))))

(ert-deftest auto-sudoedit/tramp-writable-no-change ()
  "Hook should not reopen when tramp file is already writable."
  (with-temp-buffer
    (setq buffer-file-name "/ssh:host:/etc/hosts")
    (let ((auto-sudoedit-ask nil)
          (original-name buffer-file-name))
      (cl-letf (((symbol-function 'auto-sudoedit-file-owner) (lambda (_) "root"))
                ((symbol-function 'auto-sudoedit-current-user) (lambda (_) "ncaq"))
                ((symbol-function 'tramp-tramp-file-p) (lambda (_) t))
                ((symbol-function 'tramp-sh-handle-file-writable-p) (lambda (_) t)))
        (auto-sudoedit)
        (should (equal buffer-file-name original-name))))))

(ert-deftest auto-sudoedit/tramp-not-writable-reopens ()
  "Hook should reopen via sudo when tramp file is not writable."
  (with-temp-buffer
    (setq buffer-file-name "/ssh:host:/etc/hosts")
    (let ((auto-sudoedit-ask nil)
          (recentf-list nil)
          revert-buffer-called)
      (cl-letf (((symbol-function 'auto-sudoedit-file-owner) (lambda (_) "root"))
                ((symbol-function 'auto-sudoedit-current-user) (lambda (_) "ncaq"))
                ((symbol-function 'tramp-tramp-file-p) (lambda (_) t))
                ((symbol-function 'tramp-sh-handle-file-writable-p) (lambda (_) nil))
                ((symbol-function 'set-visited-file-name) (lambda (path &optional _) (setq buffer-file-name path)))
                ((symbol-function 'revert-buffer) (lambda (&rest _) (setq revert-buffer-called t))))
        (auto-sudoedit)
        (should (string-match-p "sudo:" buffer-file-name))
        (should (string-match-p "/etc/hosts\\'" buffer-file-name))
        (should revert-buffer-called)))))

(ert-deftest auto-sudoedit/dired-buffer-not-writable ()
  "Hook should update variable `dired-directory' when directory is owned by another user."
  (with-temp-buffer
    (setq buffer-file-name nil)
    (setq dired-directory "/root/")
    (setq list-buffers-directory "/root/")
    (setq default-directory "/root/")
    (let ((auto-sudoedit-ask nil)
          (recentf-list nil)
          revert-buffer-args
          unadvertise-called-with
          advertise-called)
      (cl-letf (((symbol-function 'auto-sudoedit-file-owner) (lambda (_) "root"))
                ((symbol-function 'auto-sudoedit-current-user) (lambda (_) "ncaq"))
                ((symbol-function 'tramp-tramp-file-p) (lambda (_) nil))
                ((symbol-function 'dired-unadvertise) (lambda (dir) (setq unadvertise-called-with dir)))
                ((symbol-function 'dired-advertise) (lambda () (setq advertise-called t)))
                ((symbol-function 'revert-buffer) (lambda (&rest args) (setq revert-buffer-args args))))
        (auto-sudoedit)
        (should (equal dired-directory "/sudo::/root/"))
        (should (equal list-buffers-directory "/sudo::/root/"))
        (should (equal default-directory "/sudo::/root/"))
        (should (equal unadvertise-called-with "/root/"))
        (should advertise-called)
        (should (equal revert-buffer-args '(t t)))))))

(ert-deftest auto-sudoedit-mode/toggle ()
  "Enabling then disabling the mode should restore original hook state."
  (auto-sudoedit-mode -1)
  (let ((original-find-file-hook (copy-sequence find-file-hook))
        (original-dired-mode-hook (copy-sequence dired-mode-hook)))
    (unwind-protect
        (progn
          (auto-sudoedit-mode 1)
          (auto-sudoedit-mode -1)
          (should (equal find-file-hook original-find-file-hook))
          (should (equal dired-mode-hook original-dired-mode-hook)))
      (auto-sudoedit-mode -1))))

(ert-deftest auto-sudoedit-mode/lighter ()
  "Mode lighter should be \" ASE\"."
  (unwind-protect
      (progn
        (auto-sudoedit-mode 1)
        (should (member '(auto-sudoedit-mode " ASE") minor-mode-alist)))
    (auto-sudoedit-mode -1)))

;; Local Variables:
;; fill-column: 120
;; End:
;;; auto-sudoedit-test.el ends here
