;;; nowis-popup.el --- One-key desktop popup menu via emacsclient -*- lexical-binding: t; -*-

;; Author: Lewis
;; Keywords: convenience, frames

;;; Commentary:
;;
;; A WM-key-triggered single-key menu built on `transient'.  Bind a key to:
;;   emacsclient -c -F '((name . "emacs-popup"))' -e '(nowis-popup-menu)'
;;
;; Float the frame in your WM by matching the title "emacs-popup".
;; Configure with `nowis-popup-define'; see that docstring for the format.

;;; Code:

(require 'transient)
(require 'cl-lib)

(defcustom nowis-popup-frame-title "emacs-popup"
  "Title given to popup frames; match it in your WM to float them."
  :type 'string :group 'convenience)

(declare-function nowis-popup--transient "nowis-popup")

(defun nowis-popup--run (command)
  "Run COMMAND; close the popup frame unless COMMAND switched buffer."
  (let ((frame (selected-frame))
        (buf (current-buffer)))
    (call-interactively command)
    (when (and (frame-live-p frame)
               (frame-parameter frame 'nowis-popup)
               (eq (current-buffer) buf))
      (delete-frame frame))))

(defun nowis-popup-define (groups)
  "Define the popup menu from GROUPS as a transient prefix.
Each group is (TITLE (KEY DESC COMMAND)...).  Each entry gets a wrapper
command so transient sees a real symbol, not a lambda."
  (let ((columns
         (mapcar
          (pcase-lambda (`(,title . ,items))
            (apply #'vector title
                   (mapcar
                    (pcase-lambda (`(,key ,desc ,command))
                      (let ((wrapper (intern (format "nowis-popup:%s" command))))
                        (defalias wrapper
                          (lambda () (interactive) (nowis-popup--run command))
                          (format "Run `%s' from the nowis-popup menu." command))
                        (list key desc wrapper)))
                    items)))
          groups)))
    (eval `(transient-define-prefix nowis-popup--transient ()
             "nowis-popup menu."
             ,@columns)
          t)))

;;;###autoload
(defun nowis-popup-menu ()
  "Float the current frame and show the transient popup menu."
  (interactive)
  (unless (fboundp 'nowis-popup--transient)
    (user-error "nowis-popup: call `nowis-popup-define' first"))
  (let ((frame (selected-frame)))
    (set-frame-parameter frame 'nowis-popup t)
    (set-frame-parameter frame 'title nowis-popup-frame-title)
    (select-frame-set-input-focus frame)
    ;; Blank backdrop so the popup never reveals the current buffer.
    (switch-to-buffer (get-scratch-buffer-create))
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
