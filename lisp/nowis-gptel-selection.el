;;; nowis-gptel-selection.el --- Follow source selections in gptel -*- lexical-binding: t; -*-

;; Enable `nowis-gptel-selection-mode' globally.  In chats, M-p and C-c RET
;; materialize the displayed selection before sending; C-c C-x ignores it.
;; Nothing is uploaded until you send.  Only the latest source selection is kept.

(require 'gptel)

(defvar nowis-gptel-selection--pending nil
  "Latest selection plist: :source, :lines and :text.")
(defvar nowis-gptel-selection--last-signature nil
  "Last captured selection, retained after consumption to avoid duplicates.")
(defvar-local nowis-gptel-selection--overlay nil)
(defvar nowis-gptel-selection-mode nil)
(defvar nowis-gptel-selection--chat-mode nil)

(defun nowis-gptel-selection--capture ()
  "Snapshot a changed source region before switching can deactivate its mark."
  (when (and nowis-gptel-selection-mode
             (not gptel-mode) (not (minibufferp)) (use-region-p))
    (let* ((begin (region-beginning))
           (end (region-end))
           (signature (list (current-buffer) begin end
                            (buffer-chars-modified-tick))))
      (unless (equal signature nowis-gptel-selection--last-signature)
        (let* ((text (buffer-substring-no-properties begin end))
               (start-line (line-number-at-pos begin t))
               (end-line start-line)
               (offset 0))
          ;; Derive the end from the snapshot, avoiding a second full-buffer scan.
          ;; A trailing newline does not select any character of the next line.
          (while (and (setq offset (string-search "\n" text offset))
                      (< offset (1- (length text))))
            (setq end-line (1+ end-line)
                  offset (1+ offset)))
          (setq nowis-gptel-selection--last-signature signature
                nowis-gptel-selection--pending
                (list :source (or buffer-file-name (buffer-name))
                      :lines (cons start-line end-line)
                      :text text)))
        (nowis-gptel-selection--refresh)))))

(defun nowis-gptel-selection--delete-overlay ()
  "Remove this chat's hint without touching its draft."
  (when (overlayp nowis-gptel-selection--overlay)
    (delete-overlay nowis-gptel-selection--overlay))
  (setq nowis-gptel-selection--overlay nil))

(defun nowis-gptel-selection--label ()
  "Describe the pending snapshot, not a potentially changed source buffer."
  (let ((lines (plist-get nowis-gptel-selection--pending :lines)))
    (format "%s:%d–%d · %d 字符"
            (plist-get nowis-gptel-selection--pending :source)
            (car lines) (cdr lines)
            (length (plist-get nowis-gptel-selection--pending :text)))))

(defun nowis-gptel-selection--render ()
  "Show the candidate at the chat end without modifying buffer contents."
  (if (not (and nowis-gptel-selection-mode gptel-mode
                nowis-gptel-selection--pending))
      (nowis-gptel-selection--delete-overlay)
    (save-restriction
      (widen)
      (unless (overlayp nowis-gptel-selection--overlay)
        (setq nowis-gptel-selection--overlay
              (make-overlay (point-max) (point-max) nil t t)))
      (overlay-put nowis-gptel-selection--overlay 'after-string
                   (propertize
                    (format "\n[待发送选区：%s · C-c C-x 忽略]\n"
                            (nowis-gptel-selection--label))
                    'face 'shadow)))))

(defun nowis-gptel-selection--refresh (&rest _)
  "Refresh visible chats, including mouse-driven window switches."
  (when nowis-gptel-selection-mode
    (dolist (window (window-list-1 nil 'nomini 'all-frames))
      (with-current-buffer (window-buffer window)
        (when gptel-mode
          (unless nowis-gptel-selection--chat-mode
            (nowis-gptel-selection--chat-mode 1))
          (nowis-gptel-selection--render))))))

(defun nowis-gptel-selection--post-command ()
  "Capture changed regions and show a hint on first entering a chat."
  (nowis-gptel-selection--capture)
  ;; Insertion-advancing overlays follow typing and streamed output themselves.
  (when (and gptel-mode nowis-gptel-selection--pending
             (not nowis-gptel-selection--overlay))
    (nowis-gptel-selection--render)))

(defun nowis-gptel-selection-ignore ()
  "Ignore the latest selection everywhere until a different selection is made."
  (interactive)
  (setq nowis-gptel-selection--pending nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when nowis-gptel-selection--overlay
        (nowis-gptel-selection--delete-overlay)))))

(defun nowis-gptel-selection--quoted-text (text)
  "Quote TEXT as literal material in the chat's native format."
  ;; Fixed-width Org and Markdown block quotes need no escaping of code fences.
  (let ((prefix (if (derived-mode-p 'org-mode) ": " "> ")))
    (mapconcat (lambda (line) (concat prefix line))
               (split-string text "\n") "\n")))

(defun nowis-gptel-selection-send (&optional arg)
  "Materialize the pending snapshot at chat end, then call `gptel-send'.
Without a pending selection, preserve native send behavior.  Prefix
arguments retain gptel's menu and steering semantics.  A failed request
leaves the materialized text in the draft for ordinary retry."
  (interactive "P")
  (if (or arg (not nowis-gptel-selection-mode) (not gptel-mode)
          (not nowis-gptel-selection--pending))
      (gptel-send arg)
    (when (gptel--fsm-live-p)
      (user-error "This gptel chat is busy; selection retained"))
    (barf-if-buffer-read-only)
    (when (buffer-narrowed-p)
      (user-error "Widen the chat before sending"))
    (unless (= (point) (point-max))
      (user-error "Move to the chat end before sending"))
    (atomic-change-group
      (insert "\n\n引用：" (nowis-gptel-selection--label) "\n"
              (nowis-gptel-selection--quoted-text
               (plist-get nowis-gptel-selection--pending :text)) "\n"))
    (nowis-gptel-selection-ignore)
    ;; An active chat region would otherwise replace the entire conversation.
    (let ((mark-active nil))
      (gptel-send))))

(define-minor-mode nowis-gptel-selection--chat-mode
  "Install selection-aware send keys only in a gptel chat."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-p") #'nowis-gptel-selection-send)
            (define-key map (kbd "C-c RET") #'nowis-gptel-selection-send)
            (define-key map (kbd "C-c C-x") #'nowis-gptel-selection-ignore)
            map)
  (unless nowis-gptel-selection--chat-mode
    (nowis-gptel-selection--delete-overlay)))

(defun nowis-gptel-selection--sync-chat ()
  "Keep chat bindings in sync when `gptel-mode' is toggled."
  (nowis-gptel-selection--chat-mode
   (if (and nowis-gptel-selection-mode gptel-mode) 1 -1))
  (nowis-gptel-selection--render))

;;;###autoload
(define-minor-mode nowis-gptel-selection-mode
  "Globally preview the latest source region in gptel and attach it on send.
Switching buffers without a new selection retains the previous snapshot.
Consuming or ignoring it suppresses recapture of the unchanged region."
  :global t
  :group 'gptel
  (if nowis-gptel-selection-mode
      (progn
        (add-hook 'pre-command-hook #'nowis-gptel-selection--capture)
        (add-hook 'post-command-hook #'nowis-gptel-selection--post-command)
        (add-hook 'window-selection-change-functions #'nowis-gptel-selection--refresh)
        (add-hook 'window-buffer-change-functions #'nowis-gptel-selection--refresh)
        (add-hook 'gptel-mode-hook #'nowis-gptel-selection--sync-chat))
    (remove-hook 'pre-command-hook #'nowis-gptel-selection--capture)
    (remove-hook 'post-command-hook #'nowis-gptel-selection--post-command)
    (remove-hook 'window-selection-change-functions #'nowis-gptel-selection--refresh)
    (remove-hook 'window-buffer-change-functions #'nowis-gptel-selection--refresh)
    (remove-hook 'gptel-mode-hook #'nowis-gptel-selection--sync-chat)
    (nowis-gptel-selection-ignore)
    (setq nowis-gptel-selection--last-signature nil))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (or gptel-mode nowis-gptel-selection--chat-mode)
        (nowis-gptel-selection--sync-chat))))
  (when nowis-gptel-selection-mode
    (nowis-gptel-selection--post-command)))

(provide 'nowis-gptel-selection)
;;; nowis-gptel-selection.el ends here
