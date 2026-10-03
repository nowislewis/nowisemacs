;;; capsule-test.el --- Capsule build and activation tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'capsule)

(defmacro capsule-test--with-config (&rest body)
  "Bind a fresh configuration for tests of internal steps."
  (declare (indent 0))
  `(let ((capsule--config (capsule--read-config))) ,@body))

(defmacro capsule-test--with-package (&rest body)
  "Run BODY in a temporary configuration with a differently named submodule."
  (declare (indent 0))
  `(let* ((user-emacs-directory (file-name-as-directory
                                (make-temp-file "capsule-test-" t)))
          (capsule-drones-directory (expand-file-name "lib/" user-emacs-directory))
          (pkg-dir (expand-file-name "package with spaces" capsule-drones-directory))
          (modules (expand-file-name ".gitmodules" user-emacs-directory)))
     (unwind-protect
         (progn
           (make-directory pkg-dir t)
           (process-lines "git" "config" "--file" modules
                          "submodule.different-name.path" "lib/package with spaces")
           ,@body)
       (delete-directory user-emacs-directory t))))

(ert-deftest capsule-pre-build-no-command ()
  (capsule-test--with-package
    (capsule-test--with-config
      (should-not (capsule--run-pre-build-command pkg-dir)))))

(ert-deftest capsule-pre-build-no-gitmodules ()
  (capsule-test--with-package
    (delete-file modules)
    (capsule-test--with-config
      (should-not (capsule--run-pre-build-command pkg-dir)))))

(ert-deftest capsule-pre-build-cwd-and-source-generation ()
  (capsule-test--with-package
    (process-lines "git" "config" "--file" modules
                   "submodule.different-name.pre-build-command"
                   "printf ';;;###autoload\n(defun capsule-generated () t)\n' > generated.el")
    (require 'loaddefs-gen)
    (capsule-test--with-config (capsule--prepare-package pkg-dir))
    (should (file-exists-p (expand-file-name "generated.el" pkg-dir)))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "package with spaces-autoloads.el" pkg-dir))
      (should (search-forward "capsule-generated" nil t)))))

(ert-deftest capsule-pre-build-failure-stops-autoloads ()
  (capsule-test--with-package
    (process-lines "git" "config" "--file" modules
                   "submodule.different-name.pre-build-command" "exit 7")
    (capsule-test--with-config
      (should-error (capsule--prepare-package pkg-dir)))
    (should-not (file-exists-p
                 (expand-file-name "package with spaces-autoloads.el" pkg-dir)))))

(ert-deftest capsule-pre-build-malformed-config ()
  (capsule-test--with-package
    (with-temp-file modules (insert "[broken\n"))
    (should-error
     (capsule-test--with-config (capsule--run-pre-build-command pkg-dir)))))

(ert-deftest capsule-pre-build-unregistered-package ()
  (capsule-test--with-package
    (process-lines "git" "config" "--file" modules
                   "submodule.different-name.pre-build-command" "exit 7")
    (let ((other (expand-file-name "other/" capsule-drones-directory)))
      (make-directory other)
      (capsule-test--with-config
        (should-not (capsule--run-pre-build-command other))))))

(ert-deftest capsule-byte-compile-failure-signals ()
  (capsule-test--with-package
    (with-temp-file (expand-file-name "broken.el" pkg-dir)
      (insert ";;; -*- lexical-binding: t; -*-\n(defun broken (\n"))
    (capsule-test--with-config
      (should-error (capsule--compile-package pkg-dir)))))

(ert-deftest capsule-activation-path-order-and-idempotence ()
  (capsule-test--with-package
    (let* ((load-path (copy-sequence load-path))
           (sub (expand-file-name "lisp" pkg-dir))
           (dependency (expand-file-name "z-dependency" capsule-drones-directory)))
      (make-directory sub)
      (make-directory dependency)
      (with-temp-file (expand-file-name "capsule-test-dependency.el" dependency)
        (insert "(provide 'capsule-test-dependency)\n"))
      (with-temp-file (expand-file-name "package with spaces-autoloads.el" pkg-dir)
        (insert "(require 'capsule-test-dependency)\n"))
      (unwind-protect
          (progn
            (capsule-initialize)
            (should (featurep 'capsule-test-dependency))
            (should (equal (cl-subseq load-path 0 3) (list pkg-dir sub dependency)))
            (let ((activated-path (copy-sequence load-path)))
              (capsule-test--with-config (capsule--setup-load-path-all))
              (capsule-initialize)
              (should (equal load-path activated-path))))
        (when (featurep 'capsule-test-dependency)
          (unload-feature 'capsule-test-dependency t))))))

(ert-deftest capsule-git-literal-arguments-and-failure ()
  (capsule-test--with-package
    (let (seen)
      (cl-letf (((symbol-function 'process-file)
                 (lambda (program _in _out _display &rest args)
                   (setq seen (list program default-directory args))
                   7)))
        (should-error (capsule--git "rm" "--" "lib/a;echo bad"))
        (should (equal seen (list "git" user-emacs-directory
                                 '("rm" "--" "lib/a;echo bad"))))))))

(ert-deftest capsule-remove-stops-before-git-rm ()
  (capsule-test--with-package
    (let (calls)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'capsule--git)
                 (lambda (&rest args) (push args calls) (error "Git failed"))))
        (should-error (capsule-remove-package "package with spaces"))
        (should (= (length calls) 1))
        (should (file-directory-p pkg-dir))))))

(ert-deftest capsule-package-name-validation ()
  (capsule-test--with-package
    (dolist (name '("" "." ".." "../outside" "nested/name"))
      (should-error (capsule--package-path name)))
    (should (equal (capsule--package-path "package with spaces")
                   "lib/package with spaces"))))

(ert-deftest capsule-single-build-ignores-bulk-skip ()
  (capsule-test--with-package
    (let ((capsule-skip-autoloads-packages '("package with spaces"))
          (load-path (copy-sequence load-path)))
      (capsule-batch-prepare)
      (should-not (file-exists-p (expand-file-name "package with spaces-autoloads.el" pkg-dir)))
      (with-temp-file (expand-file-name "generated.el" pkg-dir)
        (insert ";;; -*- lexical-binding: t; -*-\n;;;###autoload\n(defun capsule-test-generated () t)\n"))
      (capsule-batch-build-single "package with spaces")
      (should (file-exists-p (expand-file-name "generated.elc" pkg-dir)))
      (should (file-exists-p (expand-file-name "package with spaces-autoloads.el" pkg-dir))))))

(ert-deftest capsule-compile-excludes-configured-paths ()
  (capsule-test--with-package
    (let* ((sub (expand-file-name "lisp" pkg-dir))
           (root-file (expand-file-name "same.el" pkg-dir))
           (sub-file (expand-file-name "same.el" sub))
           (other-file (expand-file-name "other.el" pkg-dir))
           (artifact (concat root-file "c")))
      (make-directory sub)
      (dolist (file (list root-file sub-file other-file))
        (with-temp-file file (insert ";;; -*- lexical-binding: t; -*-\n")))
      (with-temp-file artifact (insert "existing artifact"))
      (process-lines "git" "config" "--file" modules "--add"
                     "submodule.different-name.no-byte-compile" "./same.el")
      (process-lines "git" "config" "--file" modules "--add"
                     "submodule.different-name.no-byte-compile" "other.el")
      (require 'bytecomp)
      (require 'comp)
      (dolist (native '(nil t))
        (let ((capsule-use-native-compile native) seen)
          (cl-letf (((symbol-function 'byte-compile-file)
                     (lambda (file &rest _) (push file seen) t))
                    ((symbol-function 'native-compile)
                     (lambda (file &rest _) (push file seen) t)))
            (capsule-test--with-config (capsule--compile-package pkg-dir)))
          (should (equal seen (list sub-file)))))
      (should (file-exists-p artifact)))))

(ert-deftest capsule-compile-exclusion-preserves-autoloads ()
  (capsule-test--with-package
    (let ((load-path (copy-sequence load-path)))
      (with-temp-file (expand-file-name "skip.el" pkg-dir)
        (insert ";;; -*- lexical-binding: t; -*-\n;;;###autoload\n(defun capsule-test-skipped () t)\n"))
      (process-lines "git" "config" "--file" modules
                     "submodule.different-name.no-byte-compile" "skip.el")
      (capsule--build-package pkg-dir)
      (should-not (file-exists-p (expand-file-name "skip.elc" pkg-dir)))
      (with-temp-buffer
        (insert-file-contents (expand-file-name "package with spaces-autoloads.el" pkg-dir))
        (should (search-forward "capsule-test-skipped" nil t))))))

(ert-deftest capsule-extra-load-path-is-additive-and-unique ()
  (capsule-test--with-package
    (let* ((load-path (copy-sequence load-path))
           (parent (expand-file-name "extensions" pkg-dir))
           (extra (expand-file-name "nested" parent))
           (outside (expand-file-name "extra-libraries" pkg-dir))
           (file (expand-file-name "nested.el" extra))
           (other (expand-file-name "extra.el" outside)))
      (make-directory extra t)
      (make-directory outside)
      (dolist (path (list file other))
        (with-temp-file path (insert ";;; -*- lexical-binding: t; -*-\n")))
      (dolist (path '("extensions/nested" "extensions/nested/" "extra-libraries"))
        (process-lines "git" "config" "--file" modules "--add"
                       "submodule.different-name.load-path" path))
      (capsule-test--with-config
        (should (equal (capsule--get-package-dirs pkg-dir)
                       (list pkg-dir parent extra outside)))
        (let ((files (capsule--collect-el-files pkg-dir)))
          (should (= (length files) 2))
          (should (member file files))
          (should (member other files)))
        (capsule--setup-load-path-all))
      (should (equal (cl-subseq load-path 0 4) (list pkg-dir parent extra outside))))))

(ert-deftest capsule-config-snapshot-is-fresh-per-operation ()
  (capsule-test--with-package
    (process-lines "git" "config" "--file" modules "--add"
                   "submodule.different-name.load-path" "first")
    (process-lines "git" "config" "--file" modules "--add"
                   "submodule.different-name.load-path" "second")
    (process-lines "git" "config" "--file" modules
                   "submodule.different-name.pre-build-command" "echo first\necho second")
    (let ((calls 0) (original (symbol-function 'process-file)))
      (cl-letf (((symbol-function 'process-file)
                 (lambda (&rest args) (setq calls (1+ calls)) (apply original args))))
        (capsule-test--with-config
          (should (equal (capsule--package-config-values pkg-dir "load-path")
                         '("first" "second")))
          (should (equal (capsule--package-config-values pkg-dir "pre-build-command")
                         '("echo first\necho second")))
          (process-lines "git" "config" "--file" modules "--replace-all"
                         "submodule.different-name.load-path" "changed")
          (should (equal (capsule--package-config-values pkg-dir "load-path")
                         '("first" "second"))))
        (should (= calls 1))
        (capsule-test--with-config
          (should (equal (capsule--package-config-values pkg-dir "load-path") '("changed"))))
        (should (= calls 2))))))

(ert-deftest capsule-setup-reads-config-once-for-many-packages ()
  (capsule-test--with-package
    (make-directory (expand-file-name "other" capsule-drones-directory))
    (let ((load-path (copy-sequence load-path))
          (calls 0) (original (symbol-function 'process-file)))
      (cl-letf (((symbol-function 'process-file)
                 (lambda (&rest args) (setq calls (1+ calls)) (apply original args))))
        (capsule-test--with-config (capsule--setup-load-path-all))
        (should (= calls 1))))))

(ert-deftest capsule-programmatic-add-does-not-prompt ()
  (capsule-test--with-package
    (let (calls built)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (error "Unexpected prompt")))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) (error "Unexpected confirmation")))
                ((symbol-function 'capsule--git) (lambda (&rest args) (push args calls)))
                ((symbol-function 'capsule--build-package) (lambda (dir) (setq built dir))))
        (capsule-add-package "https://example.invalid/repo.git" "chosen")
        (should (equal built (expand-file-name "chosen" capsule-drones-directory)))
        (should (= (length calls) 2))))))

(ert-deftest capsule-nested-extensions-are-opt-in ()
  (capsule-test--with-package
    (let* ((extensions (expand-file-name "extensions" pkg-dir))
           (chosen (expand-file-name "chosen" extensions))
           (unused (expand-file-name "unused" extensions))
           (chosen-file (expand-file-name "chosen.el" chosen))
           (unused-file (expand-file-name "unused.el" unused)))
      (make-directory chosen t)
      (make-directory unused t)
      (dolist (file (list chosen-file unused-file))
        (with-temp-file file (insert ";;; -*- lexical-binding: t; -*-\n")))
      (capsule-test--with-config
        (should-not (capsule--collect-el-files pkg-dir)))
      (process-lines "git" "config" "--file" modules
                     "submodule.different-name.load-path" "extensions/chosen")
      (capsule-test--with-config
        (should (equal (capsule--collect-el-files pkg-dir) (list chosen-file)))))))

(ert-deftest capsule-public-compile-accepts-arbitrary-directory ()
  (capsule-test--with-package
    (let ((directory (expand-file-name "local-lisp" user-emacs-directory))
          (load-path (copy-sequence load-path)))
      (make-directory directory)
      (with-temp-file (expand-file-name "local.el" directory)
        (insert ";;; -*- lexical-binding: t; -*-\n(defun capsule-test-local () t)\n"))
      (capsule-batch-compile directory)
      (should (file-exists-p (expand-file-name "local.elc" directory))))))

(ert-deftest capsule-complete-build-reads-one-snapshot ()
  (capsule-test--with-package
    (let ((load-path (copy-sequence load-path))
          (calls 0) (original (symbol-function 'process-file)))
      (with-temp-file (expand-file-name "local.el" pkg-dir)
        (insert ";;; -*- lexical-binding: t; -*-\n(defun capsule-test-local () t)\n"))
      (cl-letf (((symbol-function 'process-file)
                 (lambda (&rest args) (setq calls (1+ calls)) (apply original args))))
        (capsule--build-package pkg-dir)
        (should (= calls 1))))))

(ert-deftest capsule-explicit-load-path-invalid-is-error ()
  (capsule-test--with-package
    (dolist (path '("missing" ".."))
      (process-lines "git" "config" "--file" modules "--replace-all"
                     "submodule.different-name.load-path" path)
      (capsule-test--with-config
        (should-error (capsule--get-package-dirs pkg-dir))))))

(ert-deftest capsule-native-unavailable-stops-before-prepare ()
  (capsule-test--with-package
    (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil))
              ((symbol-function 'capsule--read-config)
               (lambda () (error "Should not read configuration"))))
      (should-error (capsule-batch-prepare t) :type 'error)
      (should-error (capsule-batch-build-single "package with spaces" t) :type 'error))))

(ert-deftest capsule-clean-preserves-unowned-autoloads-and-unselected-files ()
  (capsule-test--with-package
    (let ((source (expand-file-name "local.el" pkg-dir))
          (autoloads (expand-file-name "package with spaces-autoloads.el" pkg-dir))
          (unused (expand-file-name "unused/nested.elc" pkg-dir)))
      (with-temp-file source (insert ";;; -*- lexical-binding: t; -*-\n"))
      (with-temp-file (concat source "c") (insert "old bytecode"))
      (with-temp-file autoloads (insert ";; Upstream autoload file\n"))
      (make-directory (file-name-directory unused) t)
      (with-temp-file unused (insert "unselected"))
      (capsule-batch-clean)
      (should-not (file-exists-p (concat source "c")))
      (should (file-exists-p autoloads))
      (should (file-exists-p unused))
      (with-temp-file autoloads
        (insert ";; Generated by Capsule; safe for capsule-batch-clean.\n"))
      (capsule-batch-clean)
      (should-not (file-exists-p autoloads)))))

(provide 'capsule-test)
;;; capsule-test.el ends here
