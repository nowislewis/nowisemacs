;;; nowis-ghostel.el --- Ghostel meow integration + compose buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal ghostel glue: meow <-> char/line-mode, a `M-'' compose buffer
;; (scratch pad -> bracketed paste), tramp-aware `ff', and window toggling.
;; Call `nowis-ghostel-setup' after `ghostel' is loaded.

;;; Code:

(require 'ghostel)

(declare-function meow-insert-exit "meow")
(defvar meow-insert-mode)
(defvar ghostel-char-mode-map)
(defvar ghostel-mode-map)
(defvar ghostel-eval-cmds)


;;;; meow <-> char/line-mode

(defun nowis-ghostel--to-line ()
  (when ghostel--term (ghostel-line-mode)))

(defun nowis-ghostel--meow-setup ()
  (add-hook 'meow-insert-enter-hook #'ghostel-char-mode nil t)
  (add-hook 'meow-insert-exit-hook  #'nowis-ghostel--to-line nil t))

(defun nowis-ghostel-char-escape ()
  "ESC leaves modal insert; outside it, return to line mode."
  (interactive)
  (if (bound-and-true-p meow-insert-mode) (meow-insert-exit)
    (nowis-ghostel--to-line)))

(defun nowis-ghostel-char-send-escape ()
  "Send a literal ESC to the terminal."
  (interactive)
  (ghostel-send-key "escape"))


;;;; Compose buffer (org-src style)

(defvar-local nowis-ghostel-compose--source nil
  "Ghostel buffer this compose buffer sends back to.")

(defvar-local nowis-ghostel-compose--adopted nil
  "Non-nil when we pulled in existing prompt input that must be cleared on send.")

(defvar-keymap nowis-ghostel-compose-mode-map
  "C-c C-c" #'nowis-ghostel-compose-send
  "C-c C-k" #'nowis-ghostel-compose-abort)

(define-derived-mode nowis-ghostel-compose-mode text-mode "Ghostel-Compose"
  "Compose multi-line input to send to a ghostel buffer."
  (setq-local header-line-format
              (substitute-command-keys
               "\\[nowis-ghostel-compose-send] send, \\[nowis-ghostel-compose-abort] abort")))

(defun nowis-ghostel-compose ()
  "Open a scratch buffer to compose input for the current ghostel buffer.
Text already typed at the prompt is pulled in for editing; it stays in
the terminal until you send (C-c C-c), so aborting (C-c C-k) loses nothing."
  (interactive)
  (unless (derived-mode-p 'ghostel-mode)
    (user-error "Not in a ghostel buffer"))
  (let* ((src (current-buffer))
         (start (ghostel-input-start-point))
         (initial (if (and start ghostel--cursor-char-pos)
                      (buffer-substring-no-properties start ghostel--cursor-char-pos)
                    ""))
         (buf (get-buffer-create (format "*ghostel compose: %s*" (buffer-name)))))
    (with-current-buffer buf
      (nowis-ghostel-compose-mode)
      (setq nowis-ghostel-compose--source src
            nowis-ghostel-compose--adopted (not (string-empty-p initial)))
      (erase-buffer)
      (insert initial))
    (pop-to-buffer buf)))

(defun nowis-ghostel-compose-send ()
  "Paste the composed text back (bracketed paste keeps multi-line verbatim).
Clears the adopted prompt input first (C-a C-k) so it is not duplicated."
  (interactive)
  (let ((text (buffer-string))
        (src nowis-ghostel-compose--source)
        (clear nowis-ghostel-compose--adopted))
    (unless (buffer-live-p src) (user-error "Source ghostel buffer is gone"))
    (with-current-buffer src
      (when clear (ghostel-send-string "\C-a\C-k"))
      (ghostel-paste-string text))
    (quit-restore-window (selected-window) 'kill)
    (pop-to-buffer src)))

(defun nowis-ghostel-compose-abort ()
  "Discard the compose buffer."
  (interactive)
  (quit-restore-window (selected-window) 'kill))


;;;; find-file-other-window (tramp-aware)

(defun nowis-ghostel-find-file-other-window (path)
  "Open PATH in another window, keeping the current remote host."
  (find-file-other-window
   (concat (or (file-remote-p default-directory) "") path)))


;;;; Window toggling  (match-buffers is MRU-ordered, so car = most recent)

(defun nowis-ghostel--buffers ()
  (match-buffers '(derived-mode . ghostel-mode)))

(defun nowis-ghostel--window ()
  (get-window-with-predicate
   (lambda (w) (buffer-match-p '(derived-mode . ghostel-mode) (window-buffer w)))))

(defun nowis-ghostel-toggle-window (&optional choose-buffer)
  "Toggle ghostel; \\[universal-argument] to pick or create a named buffer."
  (interactive "P")
  (cond
   (choose-buffer
    (let* ((names (mapcar #'buffer-name (nowis-ghostel--buffers)))
           (name (completing-read "Ghostel buffer: " names)))
      (if (member name names) (pop-to-buffer name)
        (let ((ghostel-buffer-name (format "*%s*" (string-trim name "*"))))
          (ghostel)))))
   ((nowis-ghostel--window) (delete-window (nowis-ghostel--window)))
   ((car (nowis-ghostel--buffers)) (pop-to-buffer (car (nowis-ghostel--buffers))))
   (t (ghostel))))


;;;; Setup

(defun nowis-ghostel-setup ()
  "Install ghostel keybindings, eval-cmd, and display rule."
  (with-eval-after-load 'meow
    (add-hook 'ghostel-mode-hook #'nowis-ghostel--meow-setup))
  (setf (alist-get "find-file-other-window" ghostel-eval-cmds nil nil #'equal)
        '(nowis-ghostel-find-file-other-window))
  ;; char-mode forwards nearly every key, so bind on char-mode-map explicitly;
  ;; line/semi-char inherit ghostel-mode-map.
  (define-key ghostel-char-mode-map (kbd "<escape>") #'nowis-ghostel-char-escape)
  (define-key ghostel-char-mode-map (kbd "M-q") #'nowis-ghostel-char-send-escape)
  (define-key ghostel-char-mode-map (kbd "M-`") #'nowis-ghostel-toggle-window)
  (define-key ghostel-char-mode-map (kbd "C-\\") #'toggle-input-method)
  (define-key ghostel-char-mode-map (kbd "M-'") #'nowis-ghostel-compose)
  (define-key ghostel-mode-map (kbd "M-'") #'nowis-ghostel-compose)
  (with-eval-after-load 'avy
    (advice-add 'avy-action-goto :after #'ghostel-maybe-leave-input))
  (add-to-list 'display-buffer-alist
               '((lambda (buf _) (with-current-buffer buf
                                   (derived-mode-p 'ghostel-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(provide 'nowis-ghostel)
;;; nowis-ghostel.el ends here
