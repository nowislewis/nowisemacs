;;; leader-lazy-test.el --- Lazy menu regression tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'transient)

;; Transient inspects direct session commands; load that menu dependency only.
(let ((load-path (cons (expand-file-name "lib/easysession" user-emacs-directory)
                       load-path)))
  (require 'easysession))

(with-temp-buffer
  (insert-file-contents (expand-file-name "init.el" user-emacs-directory))
  (goto-char (point-min))
  (search-forward "(transient-define-prefix apps-leader-map")
  (goto-char (match-beginning 0))
  (eval (read (current-buffer))))

(defun leader-test--suffix-command (key)
  (plist-get (cdr (transient-get-suffix 'apps-leader-map key)) :command))

(ert-deftest leader-lazy-menu-renders-without-loading-gtd ()
  (cl-letf (((symbol-function 'nowis/gtd-resume-task) '(autoload "nowis-gtd" nil t)))
    (should (autoloadp (symbol-function 'nowis/gtd-resume-task)))
    (unwind-protect
      (progn
        (transient-setup 'apps-leader-map)
        (should (autoloadp (symbol-function 'nowis/gtd-resume-task)))
        (with-current-buffer (get-buffer " *transient*")
          (dolist (label '("Resume Task" "Context" "Agenda" "Today's Journal"))
            (should (string-match-p label (buffer-string))))))
      (when (get-buffer " *transient*") (kill-buffer " *transient*")))))

(ert-deftest leader-lazy-command-preserves-interactive-prefix ()
  (let (received)
    (cl-letf (((symbol-function 'nowis/gtd-resume-task)
               (lambda (prefix) (interactive "P") (setq received prefix))))
      (let ((current-prefix-arg '(4)))
        (call-interactively (leader-test--suffix-command "g")))
      (should (equal received '(4))))))

(ert-deftest leader-lazy-command-loads-target-on-execution ()
  (let* ((dir (make-temp-file "leader-lazy-test-" t))
         (load-path (cons dir load-path))
         (original (symbol-function 'nowis/gtd-resume-task)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "leader-lazy-target.el" dir)
            (insert "(defun nowis/gtd-resume-task () (interactive) 'executed)\n"))
          (fset 'nowis/gtd-resume-task nil)
          (autoload 'nowis/gtd-resume-task "leader-lazy-target" nil t)
          (should (autoloadp (symbol-function 'nowis/gtd-resume-task)))
          (should (eq (call-interactively (leader-test--suffix-command "g")) 'executed))
          (should-not (autoloadp (symbol-function 'nowis/gtd-resume-task))))
      (fset 'nowis/gtd-resume-task original)
      (delete-directory dir t))))
