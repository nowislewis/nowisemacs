;;; gptel-quick-with-prompt.el --- Quick gptel with custom prompt to org buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides a simple function to send selected text to gptel
;; with a custom prompt, and output the result to a temporary org-mode buffer.
;; If the output buffer already exists, it will be overwritten.

;;; Code:

(require 'gptel)

(defcustom gptel-quick-prompt-buffer-name "*gptel-output*"
  "Name of the output buffer for gptel-quick-with-prompt."
  :type 'string
  :group 'gptel)

(defcustom gptel-quick-default-prompt "Explain the following text:"
  "Default prompt for gptel-quick-with-prompt."
  :type 'string
  :group 'gptel)

(defun gptel-quick-prompt--callback (response info)
  "Callback function to handle RESPONSE from gptel.
INFO contains additional context information."
  (pcase response
    ('nil (message "gptel-quick request failed with error: %s" (plist-get info :status)))
    ((pred stringp)
     (with-current-buffer (get-buffer-create gptel-quick-prompt-buffer-name)
       (let ((inhibit-read-only t))
         (erase-buffer)
         ;; Convert markdown to org format
         (insert (gptel--convert-markdown->org response))
         (org-mode)
         (goto-char (point-min)))
       (display-buffer (current-buffer))))))

;;;###autoload
(defun gptel-quick-with-prompt (prompt)
  "Send selected region with PROMPT to gptel, output to org buffer.

If a region is active, use the region text.
Otherwise, use the entire buffer content.
The result will be displayed in an org-mode buffer named `gptel-quick-prompt-buffer-name'.
If that buffer already exists, it will be cleared and overwritten.

When called interactively, prompt for input with `gptel-quick-default-prompt' as default."
  (interactive
   (list (read-string "Prompt: " gptel-quick-default-prompt)))
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-substring-no-properties (point-min) (point-max))))
         (output-buffer (get-buffer-create gptel-quick-prompt-buffer-name))
         (gptel-use-curl t)           ; Disable curl, similar to gptel-quick
         (gptel-use-tools))         ; Disable tool usage
    
    ;; Initialize the output buffer
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "Waiting for response...\n")))
    
    ;; Show the output buffer
    (display-buffer output-buffer)
    
    ;; Send request
    (gptel-request text
      :system prompt
      :callback #'gptel-quick-prompt--callback)))

(provide 'gptel-quick-with-prompt)
;;; gptel-quick-with-prompt.el ends here
