;;; batch-build-pkgs.el --- Minimal package manager -*- lexical-binding: t; -*-

;; A ultra-simplified replacement for borg.
;; Only handles: autoload generation, byte compilation, and package activation.

(require 'cl-lib)

(defvar batch-build-pkgs-drones-directory
  (expand-file-name "lib/" user-emacs-directory)
  "Directory containing package subdirectories.")

(defvar batch-build-pkgs-compile-directories '("elisp" "lisp" "extensions")
  "Whitelist of subdirectories to compile and load.")

;;; Runtime: Initialize packages

;;;###autoload
(defun batch-build-pkgs-initialize ()
  "Initialize all packages in lib/ directory.
Adds each package to load-path and loads each package's autoloads file."
  (let ((count 0)
        (new-load-path '()))
    ;; Collect all directories and load autoloads
    (dolist (pkg-dir (directory-files batch-build-pkgs-drones-directory t "^[^.]"))
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
          (dolist (subdir batch-build-pkgs-compile-directories)
            (let ((subdir-path (expand-file-name subdir pkg-dir)))
              (when (file-directory-p subdir-path)
                (push subdir-path new-load-path))))
          (cl-incf count))))
    ;; Add all collected directories to load-path at once (in reverse order)
    (setq load-path (nconc (nreverse new-load-path) load-path))
    (message "Batch-Build-Pkgs: Initialized %d packages" count)))

;;; Build-time: Batch functions (called from Makefile)

(defun batch-build-pkgs--should-compile-p (file)
  "Return non-nil if FILE should be compiled."
  (let ((filename (file-name-nondirectory file)))
    (and (string-suffix-p ".el" filename)
         (not (string-prefix-p "." filename))
         (not (string-suffix-p "-autoloads.el" filename))
         (not (string-suffix-p "-pkg.el" filename))
         (not (string-suffix-p "-test.el" filename))
         (not (string-suffix-p "-tests.el" filename)))))

(defun batch-build-pkgs--collect-el-files (dir)
  "Collect .el files from DIR and whitelisted subdirectories."
  (let ((files (directory-files dir t "^[^.].*\\.el\\'")))
    ;; Collect files in whitelisted subdirectories
    (dolist (subdir batch-build-pkgs-compile-directories)
      (let ((subdir-path (expand-file-name subdir dir)))
        (when (file-directory-p subdir-path)
          (setq files (append files
                             (directory-files-recursively subdir-path "^[^.].*\\.el\\'"))))))
    ;; Filter files
    (cl-remove-if-not #'batch-build-pkgs--should-compile-p files)))

(defun batch-build-pkgs--get-package-dirs (package)
  "Get scan directories for PACKAGE (root + whitelisted subdirs)."
  (let* ((pkg-dir (if (file-name-absolute-p package)
                      package
                    (expand-file-name package batch-build-pkgs-drones-directory)))
         (scan-dirs (list pkg-dir)))
    (dolist (subdir batch-build-pkgs-compile-directories)
      (let ((subdir-path (expand-file-name subdir pkg-dir)))
        (when (file-directory-p subdir-path)
          (push subdir-path scan-dirs))))
    (nreverse scan-dirs)))

(defun batch-build-pkgs--generate-autoloads (pkg-dir)
  "Generate autoloads for package at PKG-DIR."
  (let* ((pkg-name (file-name-nondirectory pkg-dir))
         (autoloads-file (expand-file-name (format "%s-autoloads.el" pkg-name) pkg-dir))
         (scan-dirs (batch-build-pkgs--get-package-dirs pkg-dir)))
    (message " Creating %s..." autoloads-file)
    (let ((default-directory pkg-dir))
      (loaddefs-generate
       scan-dirs
       autoloads-file
       nil  ; no excludes
       nil)))) ; no extra-data

(defvar batch-build-pkgs-use-native-compile nil
  "Whether to use native compilation instead of byte compilation.")

(defun batch-build-pkgs--compile-package (pkg-dir)
  "Compile all Elisp files in PKG-DIR."
  (let ((byte-compile-warnings '(not free-vars unresolved))
        (files (batch-build-pkgs--collect-el-files pkg-dir)))
    (if batch-build-pkgs-use-native-compile
        ;; Native compilation
        (progn
          (require 'comp)
          (dolist (file files)
            (native-compile file)))
      ;; Byte compilation
      (dolist (file files)
        (byte-compile-file file)))))

(defun batch-build-pkgs--setup-load-path-all ()
  "Add all packages and their whitelisted subdirectories to load-path."
  (let ((pkg-dirs (cl-remove-if-not #'file-directory-p
                                    (directory-files batch-build-pkgs-drones-directory t "^[^.]"))))
    (dolist (pkg-dir pkg-dirs)
      (dolist (dir (batch-build-pkgs--get-package-dirs pkg-dir))
        (add-to-list 'load-path dir)))))

(defun batch-build-pkgs-batch-autoloads ()
  "Generate autoloads for all packages.
This function is meant to be called from Emacs --batch mode."
  (unless noninteractive
    (error "batch-build-pkgs-batch-autoloads is only for batch mode"))

  (require 'loaddefs-gen)

  (let ((pkg-dirs (cl-remove-if-not #'file-directory-p
                                    (directory-files batch-build-pkgs-drones-directory t "^[^.]"))))
    (dolist (pkg-dir pkg-dirs)
      (message "\n--- [%s] ---\n" (file-name-nondirectory pkg-dir))
      (batch-build-pkgs--generate-autoloads pkg-dir))))

(defun batch-build-pkgs-batch-compile-single (package)
  "Compile a single PACKAGE (used for parallel build).
This function is meant to be called from Emacs --batch mode."
  (unless noninteractive
    (error "batch-build-pkgs-batch-compile-single is only for batch mode"))

  (require 'bytecomp)

  (let ((pkg-dir (expand-file-name package batch-build-pkgs-drones-directory)))
    (unless (file-directory-p pkg-dir)
      (error "Package directory not found: %s" pkg-dir))

    ;; Setup load-path for dependencies
    (batch-build-pkgs--setup-load-path-all)

    ;; Compile the package
    (batch-build-pkgs--compile-package pkg-dir)))

(defun batch-build-pkgs-batch-build-single (package)
  "Build a single PACKAGE: generate autoloads and compile its files.
This function is meant to be called from Emacs --batch mode."
  (unless noninteractive
    (error "batch-build-pkgs-batch-build-single is only for batch mode"))

  (require 'bytecomp)
  (require 'loaddefs-gen)

  (let ((pkg-dir (expand-file-name package batch-build-pkgs-drones-directory)))
    (unless (file-directory-p pkg-dir)
      (error "Package directory not found: %s" pkg-dir))

    (message "\n--- [%s] ---\n" package)

    ;; Generate autoloads, setup load-path, and compile
    (batch-build-pkgs--generate-autoloads pkg-dir)
    (batch-build-pkgs--setup-load-path-all)
    (batch-build-pkgs--compile-package pkg-dir)))

(provide 'batch-build-pkgs)
;;; batch-build-pkgs.el ends here
