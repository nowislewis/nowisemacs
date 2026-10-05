;;; local-autoload-test.el --- Local autoload regression tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'capsule)

(ert-deftest capsule-local-autoloads-exclude-tests-and-remove-stale-entries ()
  (let* ((user-emacs-directory (make-temp-file "local-autoload-test-" t))
         (dir (expand-file-name "lisp" user-emacs-directory))
         (source (expand-file-name "local-test-command.el" dir))
         (output (expand-file-name "lisp-autoloads.el" dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "tests" dir) t)
          (with-temp-file source
            (insert ";;;###autoload\n(defun capsule-local-test-command () (interactive))\n"))
          (with-temp-file (expand-file-name "tests/ignored.el" dir)
            (insert ";;;###autoload\n(defun capsule-ignored-test-command () (interactive))\n"))
          (capsule-generate-local-autoloads)
          (load output nil t)
          (should (autoloadp (symbol-function 'capsule-local-test-command)))
          (should (commandp 'capsule-local-test-command))
          (should-not (fboundp 'capsule-ignored-test-command))
          (should-not (file-exists-p (concat source "c")))
          (delete-file source)
          (capsule-generate-local-autoloads)
          (with-temp-buffer
            (insert-file-contents output)
            (should-not (search-forward "capsule-local-test-command" nil t))))
      (fmakunbound 'capsule-local-test-command)
      (delete-directory user-emacs-directory t))))
