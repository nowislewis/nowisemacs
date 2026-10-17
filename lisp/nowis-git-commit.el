;;; nowis-git-commit.el --- Generate commit messages via gptel -*- lexical-binding: t; -*-

;;; Commentary:
;; Generate a Conventional Commit message in `COMMIT_EDITMSG' on demand.
;; Any draft notes already typed above the `#' comment line are passed to the
;; LLM as WHY context and automatically restored if the request fails.

;;; Code:

(require 'gptel)
(require 'subr-x)

(defgroup nowis-git-commit nil
  "Generate Git commit messages with gptel."
  :group 'gptel)

(defcustom nowis-git-commit-diff-max-chars 15000
  "Maximum number of staged diff characters sent to the LLM."
  :type 'natnum
  :group 'nowis-git-commit)

(defcustom nowis-git-commit-system-prompt
  "Write a concise git commit message following Conventional Commits.
Output ONLY the raw commit message text (no code fences, no explanations).
First line: <type>(<scope>): <summary> under 72 chars.
If the change is non-trivial or user notes are provided, add a blank line and a brief body explaining WHY."
  "System prompt used by `nowis-git-commit-generate'."
  :type 'string
  :group 'nowis-git-commit)

(defun nowis-git-commit--comment-pos ()
  "Return the start position of the Git comment block or `point-max'."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat "^" (regexp-quote (or comment-start "#"))) nil t)
        (match-beginning 0)
      (point-max))))

(defun nowis-git-commit--build-prompt (user-notes)
  "Build LLM prompt from staged Git changes and optional USER-NOTES."
  (let ((stat (shell-command-to-string "git diff --cached --stat")))
    (when (string-empty-p (string-trim stat))
      (user-error "No staged changes"))
    (concat
     (unless (string-empty-p user-notes)
       (format "User notes / intent (MUST incorporate this WHY):\n%s\n\n" user-notes))
     "Recent commits (style reference):\n"
     (shell-command-to-string "git log -n 5 --oneline") "\n"
     "Staged stat:\n" stat "\n"
     "Staged diff:\n"
     (truncate-string-to-width
      (shell-command-to-string "git diff --cached -U3")
      nowis-git-commit-diff-max-chars))))

;;;###autoload
(defun nowis-git-commit-generate ()
  "Generate a Conventional Commit message in `COMMIT_EDITMSG' via gptel.
Any text already typed above the comment block is passed as WHY context
and restored automatically if the request fails or is aborted."
  (interactive)
  (let* ((comment-pos (nowis-git-commit--comment-pos))
         (user-notes (string-trim (buffer-substring-no-properties (point-min) comment-pos)))
         (prompt (nowis-git-commit--build-prompt user-notes)))
    (delete-region (point-min) comment-pos)
    (goto-char (point-min))
    (insert "\n\n")
    (goto-char (point-min))
    (let ((marker (point-marker))
          (gptel-use-tools nil)
          (gptel-use-context nil))
      (set-marker-insertion-type marker t)
      (message "Generating commit message...")
      (gptel-request prompt
        :system nowis-git-commit-system-prompt
        :stream t
        :transforms nil
        :callback
        (lambda (response info)
          (cond
           ((stringp response)
            (with-current-buffer (plist-get info :buffer)
              (save-excursion (goto-char marker) (insert response))))
           ((eq response t)
            (set-marker marker nil)
            (message "Commit message generated."))
           ((or (null response) (eq response 'abort))
            (with-current-buffer (plist-get info :buffer)
              (unless (string-empty-p user-notes)
                (save-excursion (goto-char (point-min)) (insert user-notes))))
            (set-marker marker nil)
            (message "Commit message generation failed: %s"
                     (plist-get info :status)))))))))

(provide 'nowis-git-commit)
;;; nowis-git-commit.el ends here
