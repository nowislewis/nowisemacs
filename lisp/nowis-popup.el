;;; nowis-popup.el --- One-key desktop popup menu via emacsclient -*- lexical-binding: t; -*-

;; Author: Lewis
;; Keywords: convenience, frames

;;; Commentary:
;;
;; A WM-key-triggered single-key menu built on `transient'.  Bind a key to:
;;   emacsclient -c -F '((name . "emacs-popup"))' -e '(nowis-popup-menu)'
;;
;; Float the frame in your WM by matching the title "emacs-popup".
;; Define the menu yourself as a native `transient-define-prefix' named
;; `nowis-popup--transient'.  In the popup buffer, `q' deletes the frame.

;;; Code:

(require 'transient)
(require 'cl-lib)

(defcustom nowis-popup-frame-title "emacs-popup"
  "Title given to popup frames; match it in your WM to float them."
  :type 'string :group 'convenience)

(declare-function nowis-popup--transient "nowis-popup")

(defvar nowis-popup-buffer-name "*nowis-popup*"
  "Name of the blank backdrop buffer shown in the popup frame.")

(defvar nowis-popup-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    ;; `q' closes the floating frame in any state: after a sync command, or
    ;; after finishing/aborting an org-capture launched from the menu.
    (keymap-set map "q" #'delete-frame)
    map)
  "Keymap for `nowis-popup-buffer-mode'.")

(define-derived-mode nowis-popup-buffer-mode special-mode "nowis-popup"
  "Major mode for the nowis-popup backdrop buffer.
Binds `q' to `delete-frame' so the floating popup frame is closed with a
single keypress.")

;;;###autoload
(defun nowis-popup-menu ()
  "Float the current frame and show the transient popup menu.
Define the menu yourself as a native `transient-define-prefix' named
`nowis-popup--transient'.  The popup buffer binds `q' to `delete-frame'."
  (interactive)
  (unless (fboundp 'nowis-popup--transient)
    (user-error "nowis-popup: define a transient named `nowis-popup--transient' first"))
  (let ((frame (selected-frame)))
    (set-frame-parameter frame 'nowis-popup t)
    (set-frame-parameter frame 'title nowis-popup-frame-title)
    (select-frame-set-input-focus frame)
    ;; Blank backdrop so the popup never reveals the current buffer.
    (switch-to-buffer (get-buffer-create nowis-popup-buffer-name))
    (unless (derived-mode-p 'nowis-popup-buffer-mode)
      (nowis-popup-buffer-mode))
    (delete-other-windows)
    (nowis-popup--transient)))

;;;; Built-in subcomponent: web bookmarks

(defcustom nowis-popup-bookmark-file
  (expand-file-name "bookmarks.org" user-emacs-directory)
  "Org file holding web bookmarks."
  :type 'file :group 'convenience)

(declare-function consult-org-heading "ext:consult")
(declare-function embark-open-externally "ext:embark")

;;;###autoload
(defun nowis-popup-bookmark-add (url title)
  "Append a bookmark (URL, TITLE) to `nowis-popup-bookmark-file'."
  (interactive (list (read-string "URL: ") (read-string "Title: ")))
  (with-current-buffer (find-file-noselect nowis-popup-bookmark-file)
    (goto-char (point-max))
    (insert (format "\n* %s\n[[%s]]\n  added: %s\n"
                    title url (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (save-buffer)))

;;;###autoload
(defun nowis-popup-bookmark-jump ()
  "Pick a bookmark with consult and open its URL externally."
  (interactive)
  (find-file nowis-popup-bookmark-file)
  (consult-org-heading)
  (when-let* ((url (save-excursion
                     (forward-line 1)
                     (when (looking-at "\\[\\[\\([^]]+\\)\\]\\]")
                       (match-string 1)))))
    (embark-open-externally url)))

(provide 'nowis-popup)
;;; nowis-popup.el ends here
