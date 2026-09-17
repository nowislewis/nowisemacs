;;; nowis-ghostel.el --- Ghostel integration for Meow -*- lexical-binding: t; -*-

;;; Commentary:
;; Meow controls Ghostel's input mode: insert sends keys to the terminal,
;; normal mode provides Emacs-style line editing and navigation.  `M-`' creates
;; tiled shells; `C-u M-`' selects an existing shell with Consult.

;;; Code:

(require 'consult-ghostel)
(require 'ghostel-ime)
(require 'meow)

(defvar ghostel--term)
(defvar ghostel-char-mode-map)
(defvar ghostel-mode-map)
(defvar ghostel-eval-cmds)


;;;; Meow input modes

(defun nowis-ghostel--enter-line-mode ()
  "Return to Ghostel line mode when Meow leaves insert mode."
  (when ghostel--term
    (ghostel-line-mode)))

(defun nowis-ghostel--set-up-meow ()
  "Start in Meow normal mode and synchronize its insert transitions."
  (meow-normal-mode)
  (add-hook 'meow-insert-enter-hook #'ghostel-char-mode nil t)
  (add-hook 'meow-insert-exit-hook #'nowis-ghostel--enter-line-mode nil t))

(defun nowis-ghostel-char-escape ()
  "Leave Meow insert, or return to Ghostel line mode."
  (interactive)
  (if (bound-and-true-p meow-insert-mode)
      (meow-insert-exit)
    (nowis-ghostel--enter-line-mode)))

(defun nowis-ghostel-char-send-escape ()
  "Send a literal ESC to the terminal."
  (interactive)
  (ghostel-send-key "escape"))


;;;; Tiled terminals

(defun nowis-ghostel-new-window (&optional switch)
  "Create a fresh Ghostel shell in the largest window of this frame.
With a prefix argument SWITCH, select an existing shell with
`consult-ghostel' instead."
  (interactive "P")
  (if switch
      (consult-ghostel)
    (let* ((source (get-largest-window))
           (side (if (> (window-total-width source)
                        (window-total-height source))
                     'right
                   'below)))
      (select-window (split-window source nil side))
      (ghostel-create nil display-buffer--same-window-action))))


;;;; TRAMP-aware shell integration

(defun nowis-ghostel-find-file-other-window (path)
  "Open PATH in another window, preserving the current remote host."
  (find-file-other-window
   (concat (or (file-remote-p default-directory) "") path)))


;;;; Setup

(defun nowis-ghostel-setup ()
  "Install the minimal Ghostel integration."
  (add-hook 'ghostel-mode-hook #'nowis-ghostel--set-up-meow)
  (add-hook 'ghostel-mode-hook #'ghostel-ime-mode)
  (setf (alist-get "find-file-other-window" ghostel-eval-cmds nil nil #'equal)
        '(nowis-ghostel-find-file-other-window))
  (define-key ghostel-mode-map (kbd "M-`") #'nowis-ghostel-new-window)
  ;; Char mode does not inherit `ghostel-mode-map'.
  (define-key ghostel-char-mode-map (kbd "M-`") #'nowis-ghostel-new-window)
  (define-key ghostel-char-mode-map (kbd "<escape>") #'nowis-ghostel-char-escape)
  (define-key ghostel-char-mode-map (kbd "M-q") #'nowis-ghostel-char-send-escape)
  (define-key ghostel-char-mode-map (kbd "C-\\") #'toggle-input-method)
  (define-key ghostel-char-mode-map (kbd "C-y") #'ghostel-yank)
  (define-key ghostel-char-mode-map (kbd "M-w") #'ghostel-copy-all))

(provide 'nowis-ghostel)
;;; nowis-ghostel.el ends here
