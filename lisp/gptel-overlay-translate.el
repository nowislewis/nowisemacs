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

(defcustom gptel-overlay-translate-prompt "Translate into Chinese, answer with only results. if the paragraph is code snippet, then just write a brief explaination without origin codes\n\n%s"
  "Prompt template for translation. %s will be replaced with paragraph text."
  :type 'string
  :group 'gptel-overlay-translate)

(defcustom gptel-overlay-translate-face 'shadow
  "Face for translation overlay text."
  :type '(plist)
  :group 'gptel-overlay-translate)

;;; Internal variables

;; List of overlays for the current buffer
(defvar-local gptel-overlay-translate--overlays nil
  "List of all translation overlays in this buffer.")

(defvar-local gptel-overlay-translate--overlay nil
  "Overlay displaying the translation.")

(defvar-local gptel-overlay-translate--last-bounds nil
  "Last translated paragraph bounds (START . END).")

(defvar gptel-overlay-translate--timer nil
  "Global idle timer.")

;;; Functions

(defun gptel-overlay-translate--all-paragraph-bounds ()
  "Return a list of (start . end) cons for all paragraphs in buffer."
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((start (progn (skip-chars-forward "\n \t") (point)))
              (end (progn (forward-paragraph) (point))))
          (when (> end start)
            (push (cons start end) result))
          (forward-line 1)))
      (nreverse result))))

(defun gptel-overlay-translate--clear ()
  "Clear all translation overlays in the buffer."
  (when gptel-overlay-translate--overlays
    (mapc #'delete-overlay gptel-overlay-translate--overlays)
    (setq gptel-overlay-translate--overlays nil)))

(defun gptel-overlay-translate--show (text start end)
  "Show translation TEXT as overlay below paragraph from START to END."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'after-string
                 (propertize (concat "\n" text "\n")
                             'face gptel-overlay-translate-face
                             ;; Allow Emacs to visually wrap long/multi-line
                             ;; overlay text.
                             'line-prefix ""
                             'wrap-prefix ""))
    (push ov gptel-overlay-translate--overlays)))

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
              (start (car bounds))
              (end (cdr bounds)))
          (when (> (length text) 0)
            (gptel-request
             (format gptel-overlay-translate-prompt text)
             :callback
             (lambda (response _info)
               (when (and (stringp response)
                          (buffer-live-p buf))
                 (with-current-buffer buf
                   (when gptel-overlay-translate-mode
                     (gptel-overlay-translate--show response start end))))))))))))

;;; Main batch translation logic
(defun gptel-overlay-translate--translate-buffer ()
  "Translate all paragraphs in the buffer and show overlays."
  (gptel-overlay-translate--clear)
  (dolist (bounds (gptel-overlay-translate--all-paragraph-bounds))
    (let ((text (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds))))
          (start (car bounds))
          (end (cdr bounds)))
      (when (> (length text) 0)
        (gptel-request
         (format gptel-overlay-translate-prompt text)
         :callback
         (let ((start-marker (copy-marker start))
               (end-marker (copy-marker end)))
           (lambda (response _info)
             (when (and (stringp response)
                        (buffer-live-p (marker-buffer start-marker)))
               (with-current-buffer (marker-buffer start-marker)
                 (when gptel-overlay-translate-mode
                   (gptel-overlay-translate--show response start-marker end-marker)))))))))))

;;;###autoload
(define-minor-mode gptel-overlay-translate-mode
  "Automatically translate all paragraphs in this buffer as overlays.\n
Toggle to re-trigger translation."
  :lighter " GPTTr"
  :group 'gptel-overlay-translate
  (if gptel-overlay-translate-mode
      (gptel-overlay-translate--translate-buffer)
    (gptel-overlay-translate--clear)))

(provide 'gptel-overlay-translate)
;;; gptel-overlay-translate.el ends here
