;;; nowis-review.el --- Unified declarative in-buffer code review -*- lexical-binding: t; -*-

;; Author: Lewis Liu
;; Keywords: convenience, tools

;;; Commentary:
;;
;; Zero-dependency, purely in-memory, one-way data flow review indicators.
;; - Single source of truth: `nowis-review--records' (a list of plists).
;; - Single render engine: `nowis-review-sync' (takes a plist list or JSON).
;; - Look: a continuous thin bar (│) in the left fringe, plus muted italic
;;   notes laid out in the right margin. Buffer text is never touched.
;; - Human command: `M-x nowis-review-mark' (add / edit / blank-to-delete).
;; - Revert-proof and ephemeral: `revert-buffer' redraws automatically, and
;;   killing the buffer (C-x k) frees everything.
;;
;; Note: the fringe bar uses a `before-string' overlay per line rather than the
;; `line-prefix' property, so it coexists with `org-indent' and friends.

;;; Code:

(require 'cl-lib)
(require 'json)

(defgroup nowis-review nil
  "In-buffer code review indicators."
  :group 'tools)

;; 2-pixel-wide continuous vertical bar, registered once per session.
(unless (fringe-bitmap-p 'nowis-review-fringe-bar)
  (define-fringe-bitmap 'nowis-review-fringe-bar [48] 1 8 '(top t)))

(defface nowis-review-bar
  '((t :inherit warning))
  "Face used to draw the fringe bar on annotated lines.
Inherits `warning' by default; customize it to recolor the bar."
  :group 'nowis-review)

(defvar-local nowis-review--records nil
  "The buffer's review records: the single source of truth.")

;; Records must outlive `revert-buffer': `revert-buffer--default' preserves only
;; `after-revert-hook', while everything else buffer-local is wiped by
;; `kill-all-local-variables'. Without permanent-local there would be nothing
;; left to redraw after a revert.
(put 'nowis-review--records 'permanent-local t)

(defconst nowis-review--tag 'nowis-review
  "Overlay property marking the overlays owned by this package.")

(defun nowis-review--make-overlay (beg end)
  "Create an overlay at BEG..END, tagged so `nowis-review--clear' can find it."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov nowis-review--tag t)
    ov))

(defun nowis-review--clear ()
  "Delete every overlay this package drew in the current buffer.
Cleanup is tag-based rather than list-based so that orphans left behind by a
failed draw or by an older version get reclaimed too."
  (remove-overlays nil nil nowis-review--tag t))

(defun nowis-review--resolve-buffer (target)
  "Resolve TARGET (buffer, buffer name, or file path) to a live buffer."
  (cond
   ((bufferp target) (and (buffer-live-p target) target))
   ((stringp target)
    (or (get-buffer target)
        (find-buffer-visiting target)
        (cl-loop for f in (list (expand-file-name target)
                                (expand-file-name target user-emacs-directory))
                 when (file-readable-p f) return (find-file-noselect f))))))

(defun nowis-review--draw-one (line comment &optional span)
  "Draw one note: a fringe bar over SPAN lines, COMMENT at the first line's end."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- (max 1 (or line 1))))
    (let* ((start-pos (point))
           ;; A span reaching past the buffer end would otherwise stack duplicate
           ;; bars at `point-max'; clamp it to the lines that actually exist.
           (n-lines (max 1 (min (or span 1) (count-lines start-pos (point-max))))))
      ;; 1. Left fringe: one bar per line, so the bar reads as continuous.
      (dotimes (_ n-lines)
        (let* ((lb (line-beginning-position))
               (ov (nowis-review--make-overlay lb lb)))
          (overlay-put ov 'before-string
                       (propertize " " 'display '(left-fringe nowis-review-fringe-bar nowis-review-bar)))
          (forward-line 1)))

      ;; 2. Right margin: right-align the note when the code line is short
      ;;    enough, else fall back to a fixed gap so it never overlaps code.
      (goto-char start-pos)
      (let* ((first-eol (line-end-position))
             (ov-eol (nowis-review--make-overlay first-eol first-eol))
             (line-len (- first-eol (line-beginning-position)))
             (win-w (window-width (or (get-buffer-window (current-buffer))
                                      (selected-window))))
             (note-width 48)                      ; comment budget, in columns
             (note-margin (+ note-width 4))        ; columns reserved at the right edge
             (align-threshold (+ note-margin 2))   ; margin needed before right-aligning
             (clean-comment (truncate-string-to-width (or comment "") note-width 0 nil "…"))
             (align-spec (if (and win-w (< line-len (- win-w align-threshold)))
                             `(space :align-to (- right ,note-margin))
                           '(space :width 4)))
             (align-str (concat (propertize " " 'display align-spec)
                                (propertize (format "💡 %s" clean-comment)
                                            'face '(:inherit shadow :slant italic)))))
        (overlay-put ov-eol 'before-string align-str)))))

(defun nowis-review--after-revert ()
  "Redraw from `nowis-review--records' after a buffer revert.
See the permanent-local note above for why the records are the only input here."
  (when nowis-review--records
    (nowis-review-sync (current-buffer) nowis-review--records)))

(defun nowis-review--valid-records-p (records)
  "Non-nil if RECORDS are all well-formed (:line/:span integers, :comment string).
This is what lets the draw phase assume it cannot fail midway and leak orphans."
  (cl-every (lambda (rec)
              (and (listp rec)
                   (integerp (plist-get rec :line))
                   (stringp (plist-get rec :comment))
                   (let ((span (plist-get rec :span)))
                     (or (null span) (integerp span)))))
            records))

;;;###autoload
(defun nowis-review-sync (file-or-buf records)
  "Fully sync and render all annotations for FILE-OR-BUF from RECORDS.
RECORDS is either a list of plists or a JSON string.

Parsing and validation run before any state change, so invalid input is a
no-op: it neither clears existing notes nor leaves half-drawn orphan overlays."
  (let ((buf (or (nowis-review--resolve-buffer file-or-buf)
                 (user-error "nowis-review: cannot resolve target buffer: %S" file-or-buf)))
        (parsed-records
         (cond
          ((null records) nil)
          ((stringp records)
           (condition-case err
               (json-parse-string records :object-type 'plist :array-type 'list)
             (error (user-error "nowis-review: JSON parse failed: %s"
                                (error-message-string err)))))
          ((listp records) records)
          (t (user-error "nowis-review: RECORDS must be a JSON string or a plist list, got %S"
                         records)))))
    (unless (nowis-review--valid-records-p parsed-records)
      (user-error "nowis-review: records need {line:int, comment:str, span:int}, got %S"
                  parsed-records))
    (with-current-buffer buf
      ;; 1. Drop the current rendering.
      (nowis-review--clear)
      ;; 2. Update the single source of truth.
      (setq nowis-review--records parsed-records)
      ;; 3. Redraw everything (validation guarantees this cannot fail midway).
      (dolist (rec (reverse parsed-records))
        (nowis-review--draw-one (plist-get rec :line)
                                (plist-get rec :comment)
                                (plist-get rec :span)))
      ;; 4. Records are permanent-local, so the revert hook redraws from them
      ;;    directly and no separate stash is needed.
      (add-hook 'after-revert-hook #'nowis-review--after-revert nil t)
      (format "Synced %s (%d annotations)" (buffer-name buf) (length parsed-records)))))

;;;###autoload
(defun nowis-review-list (&optional file-or-buf)
  "Return the current annotation list of FILE-OR-BUF (default: current buffer)."
  (let ((buf (if file-or-buf (nowis-review--resolve-buffer file-or-buf) (current-buffer))))
    (when (buffer-live-p buf)
      (buffer-local-value 'nowis-review--records buf))))

;;;###autoload
(defun nowis-review-add (file-or-buf line comment &optional span)
  "Append one annotation to FILE-OR-BUF and re-sync."
  (nowis-review-sync file-or-buf
                     (append (nowis-review-list file-or-buf)
                             (list (list :line (max 1 (or line 1))
                                         :comment comment
                                         :span (max 1 (or span 1)))))))

;;;###autoload
(defun nowis-review-mark (&optional comment)
  "Human-facing add / edit / delete command for one annotation.
- No note on the current line (or region): prompt for text and add one.
- Note already present: prefill it for editing.
- Blank input: delete that note."
  (interactive)
  (let* ((cur-line (line-number-at-pos (if (use-region-p) (region-beginning) (point))))
         (span (if (use-region-p)
                   (max 1 (count-lines (region-beginning) (region-end)))
                 1))
         ;; Does an existing record already cover the current line?
         (existing (cl-find-if (lambda (r)
                                 (let ((l (plist-get r :line))
                                       (s (max 1 (or (plist-get r :span) 1))))
                                   (and (>= cur-line l) (< cur-line (+ l s)))))
                               nowis-review--records))
         (prompt-init (and existing (plist-get existing :comment)))
         (prompt-label (if existing "Edit note (blank to delete): " "Note: "))
         (input (or comment (read-string prompt-label prompt-init))))
    (cond
     ;; Delete: an existing note with blank input.
     ((and existing (or (null input) (string-blank-p input)))
      (nowis-review-sync (current-buffer)
                         (cl-remove existing nowis-review--records :test #'equal))
      (message "Note deleted"))
     ;; Edit: keep the original line/span, replace only the text. Unchanged input
     ;; (e.g. RET on the prefilled value) is a no-op, not an update.
     ((and existing (equal input (plist-get existing :comment)))
      (message "No change"))
     (existing
      (nowis-review-sync (current-buffer)
                         (mapcar (lambda (r)
                                   (if (equal r existing)
                                       (plist-put (copy-sequence r) :comment input)
                                     r))
                                 nowis-review--records))
      (message "Note updated"))
     ;; Add.
     ((and input (not (string-blank-p input)))
      (nowis-review-add (current-buffer) cur-line input span))
     (t (message "No change")))))

(provide 'nowis-review)
;;; nowis-review.el ends here
