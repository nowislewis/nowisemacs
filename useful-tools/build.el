;; Copyright (C) 2025  刘艺
;; Author: 刘艺 <lewisliu@183eefd97f30>
(require 'cl-lib)
(defun find-package-dirs (base-dir)
  "Find all first-level subdirs and src/lisp subdirs under BASE-DIR.
Returns a list of absolute paths."
  (let ((dirs '()))
    ;; First level directories
    (dolist (dir (directory-files base-dir t "^[^.]"))
      (when (file-directory-p dir)
        (push dir dirs)
        ;; Second level src/lisp directories
        (dolist (subdir '("src" "lisp" "elisp" "extensions"))
          (let ((subdir-path (expand-file-name subdir dir)))
            (when (file-directory-p subdir-path)
              (push subdir-path dirs))))))
    (reverse dirs)))
(defun remove-redundent-path()
  "Remove all lines in FILE that match REGEXP."
  (let ((file "autoloads.el")
        (regexp "lib/[a-z-]*/")
        )
    (with-temp-file file
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (replace-match "")))))
      (re-search-forward "lib/[a-z-]*/" nil t)

;; Add all package directories to load-path

(let ((package-dirs (find-package-dirs (expand-file-name "lib"))))
  (dolist (dir package-dirs)
    (add-to-list 'load-path dir)))
(setq generated-autoload-file "autoloads.el")
(loaddefs-generate (find-package-dirs (expand-file-name "lib")) "autoloads.el")

(remove-redundent-path)
