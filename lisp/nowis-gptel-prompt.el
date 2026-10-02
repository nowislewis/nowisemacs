;;; nowis-gptel-prompt.el --- Shared defaults for gptel requests -*- lexical-binding: t; -*-

;;; Commentary:
;; Compose shared preferences in the request buffer, leaving chat prompts intact.
;; Requests with their own transform list retain control of their system prompt.

;;; Code:

(require 'gptel)
(require 'subr-x)

(defgroup nowis-gptel-prompt nil
  "Shared preferences for gptel requests."
  :group 'gptel)

(defcustom nowis-gptel-prompt-default
  "You are a thoughtful, resourceful assistant working with the user
in Emacs.

Adapt to the user's intent and context: answer, explain, explore,
create, or take action as appropriate. Follow explicit requests
over default preferences.

Be clear and direct. Match the depth and structure to the task:
keep simple answers short, and give complex questions the detail
they need. Avoid unnecessary ceremony and repetition.

Prioritize accuracy and useful reasoning. Make important assumptions,
uncertainty, and trade-offs clear. Ask for clarification when it
would materially improve the result; otherwise proceed with
reasonable assumptions. Use available tools when helpful, and
never claim evidence or actions you do not have.

Respond in the user's language. The following are default formatting
preferences; explicit user or task-specific requirements take precedence:

1. Your anwsers should use native Org-mode. Examples include *bold*, /italic/,
=inline code=, and [[URL][description]] links; use other Org
structures as needed.
2. When sections help, use headings on separate lines, e.g. * Heading
and ** Subheading, with depth as needed, rather than bold labels.
For code blocks, put #+begin_src LANGUAGE and #+end_src on separate
lines around the code, not Markdown fences. Do not wrap the whole
response in a code block.

3. Use only English-style ASCII punctuation.
Separate inline emphasis and code spans from adjacent text or
punctuation with spaces, e.g. *重点* , =m= :, except at line boundaries.
4. Use \\[...\\] for display math and \\(...\\) for inline math, not dollar
delimiters. Formatting must not change meaning, code, mathematical
notation, or verbatim quotations."
  "Shared preferences prepended by the request transform."
  :type '(choice (const nil) string)
  :group 'nowis-gptel-prompt)

(defcustom nowis-gptel-prompt-enabled t
  "Whether default-transform requests inherit the shared prompt.
Set buffer-locally to nil in a chat buffer to opt out."
  :type 'boolean
  :group 'nowis-gptel-prompt)

(defun nowis-gptel-prompt--compose (prompt)
  "Prepend shared preferences to PROMPT without duplicating its prefix.
Preserve conversation templates so examples remain separate messages."
  (let* ((directive (gptel--parse-directive prompt 'raw))
         (system (car directive))
         (base (string-trim nowis-gptel-prompt-default))
         (combined
          (cond
           ((or (null system) (string-empty-p (string-trim system))) base)
           ((or (equal base system)
                (string-prefix-p (concat base "\n\n") system)) system)
           (t (concat base "\n\n" system)))))
    (if (cdr directive)
        (cons combined (cdr directive))
      combined)))

(defun nowis-gptel-prompt--transform (fsm)
  "Compose defaults in the temporary request buffer for FSM.
Read the opt-out flag from the source buffer because gptel copies only
its own settings into the temporary buffer."
  (let ((source (plist-get (gptel-fsm-info fsm) :buffer)))
    (when (and (buffer-live-p source)
               (buffer-local-value 'nowis-gptel-prompt-enabled source)
               (stringp nowis-gptel-prompt-default)
               (not (string-empty-p (string-trim nowis-gptel-prompt-default))))
      (setq-local gptel-system-prompt
                  (nowis-gptel-prompt--compose gptel-system-prompt)))))

;; Presets run first; compose before asynchronous context collection starts.
(add-hook 'gptel-prompt-transform-functions
          #'nowis-gptel-prompt--transform -10)

(provide 'nowis-gptel-prompt)
;;; nowis-gptel-prompt.el ends here
