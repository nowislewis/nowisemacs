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
                 (buffer-substring-no-properties (point-min) (point-max)))))
    
    ;; Prepare output buffer and send request
    (with-current-buffer (get-buffer-create gptel-quick-prompt-buffer-name)
      (erase-buffer)
      (org-mode)
      (display-buffer (current-buffer))
      
      ;; Use gptel's default callback for stream support
      (gptel-request text
        :buffer (current-buffer)
        :position (point-marker)
        :system prompt
        :stream gptel-stream))))

(provide 'gptel-quick-with-prompt)
;;; gptel-quick-with-prompt.el ends here
