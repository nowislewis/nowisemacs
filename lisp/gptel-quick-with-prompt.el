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
  "Default user prompt for gptel-quick-with-prompt.
This will be combined with the region or buffer content."
  :type 'string
  :group 'gptel)

;;;###autoload
(defun gptel-quick-with-prompt (user-prompt)
  "Send region/buffer with USER-PROMPT to gptel, output to org buffer.

If a region is active, use the region text.
Otherwise, use the entire buffer content.

The USER-PROMPT you provide will be combined with the extracted text
in a new org-mode buffer, then `gptel-send' is called to send the request.

The result will be displayed in an org-mode buffer named
`gptel-quick-prompt-buffer-name'. If that buffer already exists,
it will be cleared and overwritten.

When called interactively, prompt for input with
`gptel-quick-default-prompt' as default."
  (interactive
   (list (minibuffer-with-setup-hook
             (lambda ()
               ;; Enable @ preset completion in minibuffer
               (add-hook 'completion-at-point-functions #'gptel-preset-capf nil t))
           (read-string "Prompt: " gptel-quick-default-prompt))))
  (when (string-empty-p user-prompt)
    (setq user-prompt gptel-quick-default-prompt))
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-substring-no-properties (point-min) (point-max))))
         (output-buffer (get-buffer-create gptel-quick-prompt-buffer-name)))
    
    ;; Prepare output buffer
    (with-current-buffer output-buffer
      (erase-buffer)
      (org-mode)
      
      ;; Insert user's question first
      (insert "*** Question\n\n")
      (insert user-prompt "\n\n")
      (insert "#+begin_src text\n")
      (insert text "\n")
      (insert "#+end_src\n\n")
      (insert "*** Answer\n\n")
      
      ;; Enable gptel-mode after inserting content
      (gptel-mode 1)
      
      ;; Display the buffer
      (display-buffer (current-buffer))
      
      ;; Move point to the end (after "* Answer\n\n")
      (goto-char (point-max))
      
      ;; Call gptel-send to send the entire buffer content
      (gptel-send))))

(provide 'gptel-quick-with-prompt)
;;; gptel-quick-with-prompt.el ends here
