;;; gptel-dired-translate.el --- Batch translate files in dired  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; This package provides batch translation functionality for files marked in dired.
;; It uses gptel-request to translate file contents to English.

;;; Code:
(require 'gptel)
(require 'dired)

(defcustom gptel-dired-translate-suffix ".md"
  "Suffix to append to translated filenames."
  :type 'string
  :group 'gptel)

;;;###autoload
(defun gptel-dired-translate-marked-files ()
  "Translate marked files in dired to English using gptel.

For each marked file, reads its content, translates it using the
configured LLM backend, and saves the result to a new file with
the suffix defined in `gptel-dired-translate-suffix'."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "This command must be run in a dired buffer"))

  (let ((files (seq-filter #'file-regular-p (dired-get-marked-files))))
    (when (null files)
      (user-error "No regular files marked"))

    (dolist (file files)
      (let ((output-file (concat file gptel-dired-translate-suffix)))
        (with-temp-buffer
          (insert-file-contents file)
          (gptel-request
           (format "You are a highly skilled translation engine with expertise in ficttion literature
Your function is to translate texts into the English, capturing the inarrative
depth and emotional nuances of the original work. Maintain thhe original
storytelling elements and cultural references without adding any
explanations or annotations:\n\n%s" (buffer-string))
           :callback
           (lambda (response _info)
             (when (stringp response)
               (with-temp-file output-file
                 (insert response))
               (message "Saved translation: %s" output-file)))))))))

(provide 'gptel-dired-translate)
;;; gptel-dired-translate.el ends here
