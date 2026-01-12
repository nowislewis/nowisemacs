;;; gptel-overlay-translate.el --- Live paragraph translation overlay  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Live translation overlay that shows English translation above current paragraph.

;;; Code:
(require 'gptel)

;;; Customization

(defgroup gptel-overlay-translate nil
  "Live paragraph translation overlay."
  :group 'gptel)

(defcustom gptel-overlay-translate-delay 1.0
  "Idle time in seconds before translating current paragraph."
  :type 'number
  :group 'gptel-overlay-translate)

(defcustom gptel-overlay-translate-prompt "Translate to English:\n\n%s"
  "Prompt template for translation. %s will be replaced with paragraph text."
  :type 'string
  :group 'gptel-overlay-translate)

(defcustom gptel-overlay-translate-face '(:foreground "cyan" :background "#2d2d2d")
  "Face for translation overlay text."
  :type '(plist)
  :group 'gptel-overlay-translate)

;;; Internal variables

(defvar-local gptel-overlay-translate--overlay nil
  "Overlay displaying the translation.")

(defvar-local gptel-overlay-translate--last-bounds nil
  "Last translated paragraph bounds (START . END).")

(defvar gptel-overlay-translate--timer nil
  "Global idle timer.")

;;; Functions

(defun gptel-overlay-translate--paragraph-bounds ()
  "Return current paragraph bounds as (START . END)."
  (save-excursion
    (cons (progn (backward-paragraph) (point))
          (progn (forward-paragraph) (point)))))

(defun gptel-overlay-translate--clear ()
  "Remove translation overlay."
  (when (overlayp gptel-overlay-translate--overlay)
    (delete-overlay gptel-overlay-translate--overlay)
    (setq gptel-overlay-translate--overlay nil)))

(defun gptel-overlay-translate--show (text pos)
  "Show translation TEXT at position POS."
  (gptel-overlay-translate--clear)
  (setq gptel-overlay-translate--overlay (make-overlay pos pos))
  (overlay-put gptel-overlay-translate--overlay 'before-string
               (propertize (concat text "\n\n")
                           'face gptel-overlay-translate-face)))

(defun gptel-overlay-translate--translate ()
  "Translate current paragraph if it changed."
  (when (and (buffer-live-p (current-buffer))
             gptel-overlay-translate-mode)
    (let ((bounds (gptel-overlay-translate--paragraph-bounds)))
      (unless (equal bounds gptel-overlay-translate--last-bounds)
        (setq gptel-overlay-translate--last-bounds bounds)
        (let ((text (string-trim
                     (buffer-substring-no-properties (car bounds) (cdr bounds))))
              (buf (current-buffer))
              (pos (car bounds)))
          (when (> (length text) 0)
            (gptel-request
             (format gptel-overlay-translate-prompt text)
             :callback
             (lambda (response _info)
               (when (and (stringp response)
                          (buffer-live-p buf))
                 (with-current-buffer buf
                   (when gptel-overlay-translate-mode
                     (gptel-overlay-translate--show response pos))))))))))))

;;; Minor mode

;;;###autoload
(define-minor-mode gptel-overlay-translate-mode
  "Show live translation overlay for current paragraph."
  :lighter " Tr"
  :group 'gptel-overlay-translate
  (if gptel-overlay-translate-mode
      (unless gptel-overlay-translate--timer
        (setq gptel-overlay-translate--timer
              (run-with-idle-timer gptel-overlay-translate-delay t
                                   #'gptel-overlay-translate--translate)))
    (gptel-overlay-translate--clear)
    (setq gptel-overlay-translate--last-bounds nil)))

(provide 'gptel-overlay-translate)
;;; gptel-overlay-translate.el ends here
