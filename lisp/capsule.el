;;; capsule.el --- Minimal package manager -*- lexical-binding: t; -*-

;; A ultra-simplified replacement for borg.
;; Only handles: autoload generation, byte compilation, and package activation.

(require 'cl-lib)

(defvar capsule-drones-directory
  (expand-file-name "lib/" user-emacs-directory)
  "Directory containing package subdirectories.")

(defvar capsule-compile-directories '("elisp" "lisp" "extensions")
  "Whitelist of subdirectories to compile and load.")

(defvar capsule-skip-autoloads-packages '("reader")
  "List of package names to skip when generating autoloads.
Each element should be a package name (string) as it appears in lib/ directory.")

;;; Runtime: Initialize packages

;;;###autoload
(defun capsule-initialize ()
  "Initialize all packages in lib/ directory.
Adds each package to load-path and loads each package's autoloads file."
  (let ((count 0)
        (new-load-path '()))
    ;; Collect all directories and load autoloads
    (dolist (pkg-dir (directory-files capsule-drones-directory t "^[^.]"))
      (when (file-directory-p pkg-dir)
        (let* ((pkg-name (file-name-nondirectory pkg-dir))
               (autoloads-file (expand-file-name
                               (format "%s-autoloads.el" pkg-name)
                               pkg-dir)))
          ;; Load autoloads if exists
          (when (file-exists-p autoloads-file)
            (with-demoted-errors "Error loading %s: %%s" autoloads-file
              (load autoloads-file nil t)))
          ;; Collect package root directory
          (push pkg-dir new-load-path)
          ;; Collect whitelisted subdirectories
          (dolist (subdir capsule-compile-directories)
            (let ((subdir-path (expand-file-name subdir pkg-dir)))
              (when (file-directory-p subdir-path)
                (push subdir-path new-load-path))))
          (cl-incf count))))
    ;; Add all collected directories to load-path at once (in reverse order)
    (setq load-path (nconc (nreverse new-load-path) load-path))
    (message "Capsule: Initialized %d packages" count)))

;;; Build-time: Batch functions (called from Makefile)

(defun capsule--should-compile-p (file)
  "Return non-nil if FILE should be compiled."
  (let ((filename (file-name-nondirectory file)))
    (and (string-suffix-p ".el" filename)
         (not (string-prefix-p "." filename))
         (not (string-suffix-p "-autoloads.el" filename))
         (not (string-suffix-p "-pkg.el" filename))
         (not (string-suffix-p "-test.el" filename))
         (not (string-suffix-p "-tests.el" filename)))))

(defun capsule--collect-el-files (dir)
  "Collect .el files from DIR and whitelisted subdirectories."
  (let ((files (directory-files dir t "^[^.].*\\.el\\'")))
    ;; Collect files in whitelisted subdirectories
    (dolist (subdir capsule-compile-directories)
      (let ((subdir-path (expand-file-name subdir dir)))
        (when (file-directory-p subdir-path)
          (setq files (append files
                             (directory-files-recursively subdir-path "^[^.].*\\.el\\'"))))))
    ;; Filter files
    (cl-remove-if-not #'capsule--should-compile-p files)))

(defun capsule--get-package-dirs (package)
  "Get scan directories for PACKAGE (root + whitelisted subdirs)."
  (let* ((pkg-dir (if (file-name-absolute-p package)
                      package
                    (expand-file-name package capsule-drones-directory)))
         (scan-dirs (list pkg-dir)))
    (dolist (subdir capsule-compile-directories)
      (let ((subdir-path (expand-file-name subdir pkg-dir)))
        (when (file-directory-p subdir-path)
          (push subdir-path scan-dirs))))
    (nreverse scan-dirs)))

(defun capsule--generate-autoloads (pkg-dir)
  "Generate autoloads for package at PKG-DIR."
  (let* ((pkg-name (file-name-nondirectory pkg-dir))
         (autoloads-file (expand-file-name (format "%s-autoloads.el" pkg-name) pkg-dir))
         (scan-dirs (capsule--get-package-dirs pkg-dir)))
    (message " Creating %s..." autoloads-file)
    (let ((default-directory pkg-dir))
      (loaddefs-generate
       scan-dirs
       autoloads-file
       nil  ; no excludes
       nil)))) ; no extra-data

(defvar capsule-use-native-compile nil
  "Whether to use native compilation instead of byte compilation.")

(defun capsule--compile-package (pkg-dir)
  "Compile all Elisp files in PKG-DIR."
  (let ((byte-compile-warnings '(not free-vars unresolved))
        (files (capsule--collect-el-files pkg-dir)))
    (if capsule-use-native-compile
        ;; Native compilation
        (progn
          (require 'comp)
          (dolist (file files)
            (native-compile file)))
      ;; Byte compilation
      (dolist (file files)
        (byte-compile-file file)))))

(defun capsule--setup-load-path-all ()
  "Add all packages and their whitelisted subdirectories to load-path."
  (let ((pkg-dirs (cl-remove-if-not #'file-directory-p
                                    (directory-files capsule-drones-directory t "^[^.]"))))
    (dolist (pkg-dir pkg-dirs)
      (dolist (dir (capsule--get-package-dirs pkg-dir))
        (add-to-list 'load-path dir)))))

(defun capsule-batch-autoloads ()
  "Generate autoloads for all packages.
This function is meant to be called from Emacs --batch mode."
  (unless noninteractive
    (error "capsule-batch-autoloads is only for batch mode"))

  (require 'loaddefs-gen)

  (let ((pkg-dirs (cl-remove-if-not #'file-directory-p
                                    (directory-files capsule-drones-directory t "^[^.]"))))
    (dolist (pkg-dir pkg-dirs)
      (let ((pkg-name (file-name-nondirectory pkg-dir)))
        (if (member pkg-name capsule-skip-autoloads-packages)
            (message "\n--- [%s] (skipped) ---\n" pkg-name)
          (message "\n--- [%s] ---\n" pkg-name)
          (capsule--generate-autoloads pkg-dir))))))

(defun capsule-batch-compile-single (package)
  "Compile a single PACKAGE (used for parallel build).
This function is meant to be called from Emacs --batch mode."
  (unless noninteractive
    (error "capsule-batch-compile-single is only for batch mode"))

  (require 'bytecomp)

  (let ((pkg-dir (expand-file-name package capsule-drones-directory)))
    (unless (file-directory-p pkg-dir)
      (error "Package directory not found: %s" pkg-dir))

    ;; Setup load-path for dependencies
    (capsule--setup-load-path-all)

    ;; Compile the package
    (capsule--compile-package pkg-dir)))

(defun capsule-batch-build-single (package)
  "Build a single PACKAGE: generate autoloads and compile its files.
This function is meant to be called from Emacs --batch mode."
  (unless noninteractive
    (error "capsule-batch-build-single is only for batch mode"))

  (require 'bytecomp)
  (require 'loaddefs-gen)

  (let ((pkg-dir (expand-file-name package capsule-drones-directory)))
    (unless (file-directory-p pkg-dir)
      (error "Package directory not found: %s" pkg-dir))

    (message "\n--- [%s] ---\n" package)

    ;; Generate autoloads, setup load-path, and compile
    (capsule--generate-autoloads pkg-dir)
    (capsule--setup-load-path-all)
    (capsule--compile-package pkg-dir)))

;;; Interactive package management

;;;###autoload
(defun capsule-add-package (url &optional name)
  "Add a new package from git URL.
NAME is optional package name (defaults to git repo name).
Adds as git submodule, generates autoloads, and compiles."
  (interactive "sPackage git URL: ")
  (let* ((default-directory user-emacs-directory)
         (parsed-name (or name
                          (and (string-match "/\\([^/]+\\)\\.git\\'" url)
                               (match-string 1 url))
                          (and (string-match "/\\([^/]+\\)/?\\'" url)
                               (match-string 1 url))))
         (pkg-name (if parsed-name
                       (read-string (format "Package name (default %s): " parsed-name)
                                    nil nil parsed-name)
                     (read-string "Package name: ")))
         (pkg-path (format "lib/%s" pkg-name)))
    (unless (yes-or-no-p (format "Add package '%s' from %s? " pkg-name url))
      (user-error "Cancelled"))
    ;; Add submodule
    (message "Adding git submodule...")
    (shell-command (format "git submodule add --force %s %s" url pkg-path))
    (shell-command "git submodule update --init --recursive")
    ;; Build package
    (let ((pkg-dir (expand-file-name pkg-path user-emacs-directory)))
      (when (file-directory-p pkg-dir)
        (message "Generating autoloads...")
        (capsule--generate-autoloads pkg-dir)
        (message "Compiling package...")
        (capsule--setup-load-path-all)
        (capsule--compile-package pkg-dir)
        (message "Package '%s' added successfully!" pkg-name)
        (message "Don't forget to configure it in your init.el")))))

;;;###autoload
(defun capsule-remove-package (name)
  "Remove package NAME.
Removes git submodule and cleans up files."
  (interactive
   (list (completing-read "Remove package: "
                          (directory-files capsule-drones-directory nil "^[^.]"))))
  (let* ((default-directory user-emacs-directory)
         (pkg-path (format "lib/%s" name))
         (pkg-dir (expand-file-name pkg-path user-emacs-directory)))
    (unless (file-directory-p pkg-dir)
      (user-error "Package '%s' not found" name))
    (unless (yes-or-no-p (format "Remove package '%s'? " name))
      (user-error "Cancelled"))
    ;; Remove submodule
    (message "Removing git submodule...")
    (shell-command (format "git submodule deinit -f %s" pkg-path))
    (shell-command (format "git rm -f %s" pkg-path))
    (shell-command (format "rm -rf .git/modules/%s" pkg-path))
    (message "Package '%s' removed successfully!" name)
    (message "Don't forget to remove its configuration from init.el")))

(provide 'capsule)
;;; capsule.el ends here
