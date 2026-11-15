;;; nowis-pi-test.el --- Fleet display regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nowis-pi)

(ert-deftest nowis-pi-fleet-open-keeps-default-and-other-window-distinct ()
  (with-temp-buffer
    (let ((session '(:name "test" :cwd "/tmp/"))
          calls)
      (insert (propertize "test" 'nowis-pi-session session))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'nowis-pi--open-session)
                 (lambda (&rest args) (push args calls))))
        (call-interactively #'nowis-pi-fleet-open-at-point)
        (call-interactively #'nowis-pi-fleet-open-other-window))
      (should (equal (nreverse calls)
                     '(("test" "/tmp/" switch)
                       ("test" "/tmp/" other-window))))
      (should (eq (keymap-lookup nowis-pi-fleet-mode-map "RET")
                  #'nowis-pi-fleet-open-at-point))
      (should (eq (keymap-lookup nowis-pi-fleet-mode-map "o")
                  #'nowis-pi-fleet-open-other-window)))))

(ert-deftest nowis-pi-open-other-window-preserves-fleet-and-live-session ()
  (require 'ghostel)
  (let ((fleet (generate-new-buffer " *Fleet display test*"))
        (terminal (generate-new-buffer " *Pi display test*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer fleet)
          (with-current-buffer terminal
            (setq major-mode 'ghostel-mode))
          (let ((fleet-window (selected-window))
                (display-buffer-alist nil)
                (display-buffer-base-action nil)
                (pop-up-windows t))
            (cl-letf (((symbol-function 'nowis-pi--find-task-buffer)
                       (lambda (&rest _) terminal))
                      ((symbol-function 'nowis-pi--buffer-attached-p)
                       (lambda (_) t))
                      ((symbol-function 'nowis-pi--cache-put) #'ignore)
                      ((symbol-function 'nowis-pi--read-meta) #'ignore)
                      ((symbol-function 'ghostel-exec)
                       (lambda (&rest _) (ert-fail "Do not respawn a live session"))))
              (should (eq (nowis-pi--open-session "test" "/tmp/" 'other-window)
                          terminal))
              (should (eq (window-buffer fleet-window) fleet))
              (should-not (eq (selected-window) fleet-window))
              (should (eq (window-buffer (selected-window)) terminal))
              ;; Reopening reuses the existing terminal window, not a third one.
              (select-window fleet-window)
              (nowis-pi--open-session "test" "/tmp/" 'other-window)
              (should (= (length (window-list)) 2))
              (should (eq (window-buffer fleet-window) fleet)))))
      (kill-buffer terminal)
      (kill-buffer fleet))))

(defmacro nowis-pi-test--with-socket-dir (dir &rest body)
  "Run BODY with an empty cache and a temporary socket directory DIR."
  (declare (indent 1))
  `(let* ((,dir (file-name-as-directory (make-temp-file "nowis-pi-test" t)))
          (nowis-pi-socket-directory ,dir)
          (nowis-pi--disk-cache (make-hash-table :test #'equal)))
     (unwind-protect (progn ,@body)
       (delete-directory ,dir t))))

(defun nowis-pi-test--write-manifest (dir name cwd)
  "Write a manifest for session NAME in DIR recording CWD."
  (with-temp-file (expand-file-name (concat name ".cwd") dir)
    (insert cwd "\n" dir name ".sock\n\nbash\n")))

(ert-deftest nowis-pi-read-meta-keeps-cwd-on-the-sessions-own-host ()
  "A manifest's directory belongs to the session's host, not the caller's."
  (nowis-pi-test--with-socket-dir dir
    (nowis-pi-test--write-manifest dir "agent" "/home/someone/proj")
    ;; Reading a local session from a remote buffer must not borrow the
    ;; TRAMP prefix of `default-directory'.
    (let ((default-directory "/ssh:nohost-nowis-pi:/tmp/"))
      (should (equal (plist-get (nowis-pi--read-meta nil "agent") :cwd)
                     "/home/someone/proj/")))))

(ert-deftest nowis-pi-session-host-comes-from-the-identity ()
  "OSC 7 may move `default-directory' to another host; the session stays."
  (nowis-pi-test--with-socket-dir dir
    (let ((buf (generate-new-buffer " *pi host test*")))
      (unwind-protect
          (progn
            (nowis-pi--cache-put nil "agent" "/tmp/proj/")
            (with-current-buffer buf
              (setq major-mode 'ghostel-mode
                    ;; What Ghostel records after `ssh' to another machine.
                    default-directory "/ssh:nohost-nowis-pi:/srv/")
              (setq-local ghostel-identity
                          (nowis-pi--task-identity nil "agent" "bash")))
            (let ((sessions (nowis-pi--all-known-sessions nil)))
              (should (= (length sessions) 1))
              (should (equal (plist-get (car sessions) :host) "local"))
              (should (equal (plist-get (car sessions) :cwd) "/tmp/proj/")))
            ;; A local `cd' still follows the shell.
            (with-current-buffer buf
              (setq default-directory "/tmp/proj/sub/"))
            (should (equal (plist-get (car (nowis-pi--all-known-sessions nil))
                                      :cwd)
                           "/tmp/proj/sub/")))
        (kill-buffer buf)))))

(ert-deftest nowis-pi-nil-scan-mode-reads-no-disk ()
  "An empty cache means scanned and empty, not unscanned."
  (nowis-pi-test--with-socket-dir dir
    (cl-letf (((symbol-function 'nowis-pi--scan-host-disk)
               (lambda (&rest _) (ert-fail "nil scan mode touched the disk"))))
      (should-not (nowis-pi--all-known-sessions nil)))))

(ert-deftest nowis-pi-new-names-check-the-manifest-on-disk ()
  "The cache alone would let a new session reuse a forgotten name."
  (nowis-pi-test--with-socket-dir dir
    (nowis-pi-test--write-manifest dir "taken" "/tmp")
    (should (nowis-pi--name-taken-p nil "taken"))
    (should (equal (nowis-pi--next-free-name nil "taken") "taken-2"))
    (should (equal (nowis-pi--next-free-name nil "free") "free"))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "taken")))
      (should-error (nowis-pi--read-new-spec nil "free") :type 'user-error))))

(ert-deftest nowis-pi-status-change-redraws-only-for-pi-sessions ()
  "A plain shell terminal must not schedule a Fleet redraw."
  (let (scheduled)
    (cl-letf (((symbol-function 'nowis-pi--schedule-fleet-refresh)
               (lambda () (setq scheduled t))))
      (with-temp-buffer
        (setq major-mode 'ghostel-mode)
        (setq-local ghostel-identity '((kind . shell)))
        (nowis-pi--on-status-change "working" nil nil)
        (should-not scheduled))
      (with-temp-buffer
        (setq major-mode 'ghostel-mode)
        (setq-local ghostel-identity (nowis-pi--task-identity nil "agent"))
        (nowis-pi--on-status-change "working" nil nil)
        (should scheduled)))))

(ert-deftest nowis-pi-fleet-empty-hint-follows-the-keymap ()
  "The empty Fleet repeats whatever key the keymap binds, not a literal."
  (nowis-pi-test--with-socket-dir dir
    (let ((nowis-pi-hosts '(nil)))
      (with-temp-buffer
        (let ((text (nowis-pi-fleet--render-content nil)))
          (should (string-match-p "No active Pi agents" text))
          (should (string-match-p
                   (regexp-quote (key-description
                                  (where-is-internal #'nowis-pi-new
                                                     nowis-pi-fleet-mode-map t)))
                   text))
          ;; Rebinding the command must change the hint.
          (keymap-set nowis-pi-fleet-mode-map "c" nil)
          (keymap-set nowis-pi-fleet-mode-map "N" #'nowis-pi-new)
          (unwind-protect
              (let ((rebound (nowis-pi-fleet--render-content nil)))
                (should (string-match-p "N starts one" rebound)))
            (keymap-set nowis-pi-fleet-mode-map "N" nil)
            (keymap-set nowis-pi-fleet-mode-map "c" #'nowis-pi-new)))))))

(provide 'nowis-pi-test)
;;; nowis-pi-test.el ends here
