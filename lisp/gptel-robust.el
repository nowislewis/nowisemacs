;;; gptel-robust.el --- Robustness fix for unknown tool calls -*- lexical-binding: t -*-
;;; Commentary:
;; gptel (karthink/gptel, still unfixed upstream as of 1230375) pushes a nil
;; tool spec into `gptel--display-tool-results' when the model calls an
;; unregistered tool (seen via LiteLLM + qwen3.8-flash: "Agent=subagent_type",
;; "Bash").  `gptel-tool-name' then signals `Wrong type argument: gptel-tool,
;; nil' in the process sentinel and kills the request.  Filter those entries
;; out before display; the "Tool 'X' is not available" result is still sent
;; back to the model via `gptel--parse-tool-results', which only uses
;; :id/:result, so the model can self-correct on the next turn.

;;; Code:
(require 'gptel)

(defun gptel-robust--known-tool-p (entry)
  "Return non-nil if ENTRY (tool args result) has a real gptel-tool."
  (and (car entry) (gptel-tool-p (car entry))))

(define-advice gptel--display-tool-results (:around (fn tool-results info) skip-unknown-tools)
  "Drop entries whose tool spec is nil (unknown tool called by model)."
  (funcall fn (cl-remove-if-not #'gptel-robust--known-tool-p tool-results) info))

(provide 'gptel-robust)
;;; gptel-robust.el ends here
