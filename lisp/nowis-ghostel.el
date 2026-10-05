;;; nowis-ghostel.el --- Ghostel integration for Meow -*- lexical-binding: t; -*-

;;; Commentary:
;; Meow controls Ghostel's input mode: insert sends keys to the terminal,
;; normal mode provides read-only navigation and copying with live output.
;;
;; Project terminals use upstream `ghostel-project'; window management stays
;; with Emacs.  Use `consult-ghostel-project' explicitly to pick a terminal.

;;; Code:

(require 'consult-ghostel)
(require 'ghostel-ime)
(require 'meow)

(declare-function ace-window "ace-window" (&optional arg))
(defvar ghostel--term)
(defvar ghostel--input-mode)
(defvar ghostel--pending-initial-line-mode)
(defvar ghostel-char-mode-map)
(defvar ghostel-mode-map)
(defvar ghostel-eval-cmds)


;;;; Meow input modes

(defun nowis-ghostel--enter-emacs-mode ()
  "Enter live browsing without toggling out of an existing read-only mode."
  (when ghostel--term
    ;; Startup may still be waiting for a shell prompt to enter line mode.
    (setq ghostel--pending-initial-line-mode nil)
    (unless (memq ghostel--input-mode '(emacs copy))
      (ghostel-emacs-mode))))

(defun nowis-ghostel--sync-normal-mode ()
  "Repair normal-mode browsing after terminal initialization or mode drift."
  (when (bound-and-true-p meow-normal-mode)
    (nowis-ghostel--enter-emacs-mode)))

(defun nowis-ghostel--set-up-meow ()
  "Start in Meow normal mode and synchronize its insert transitions."
  ;; Meow owns leaving browsing; copying must not restore terminal input.
  (setq-local ghostel-readonly-fast-exit nil)
  (meow-normal-mode)
  (nowis-ghostel--sync-normal-mode)
  (add-hook 'meow-insert-enter-hook #'ghostel-char-mode nil t)
  (add-hook 'meow-insert-exit-hook #'nowis-ghostel--enter-emacs-mode nil t)
  ;; `ghostel-mode-hook' runs before the terminal exists; synchronize before
  ;; the first command, once initialization has installed its input keymap.
  (add-hook 'pre-command-hook #'nowis-ghostel--sync-normal-mode nil t))

(defun nowis-ghostel-char-escape ()
  "Leave Meow insert, or return to Ghostel read-only browsing."
  (interactive)
  (if (bound-and-true-p meow-insert-mode)
      (meow-insert-exit)
    (nowis-ghostel--enter-emacs-mode)))

(defun nowis-ghostel-char-send-escape ()
  "Send a literal ESC to the terminal."
  (interactive)
  (ghostel-send-key "escape"))


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
  (define-key ghostel-mode-map (kbd "M-`") #'ghostel-project)
  ;; Char mode does not inherit `ghostel-mode-map'.
  (define-key ghostel-char-mode-map (kbd "M-`") #'ghostel-project)
  (define-key ghostel-char-mode-map (kbd "M-o") #'ace-window)
  (define-key ghostel-char-mode-map (kbd "<escape>") #'nowis-ghostel-char-escape)
  (define-key ghostel-char-mode-map (kbd "M-q") #'nowis-ghostel-char-send-escape)
  (define-key ghostel-char-mode-map (kbd "C-\\") #'toggle-input-method)
  (define-key ghostel-char-mode-map (kbd "C-y") #'ghostel-yank)
  (define-key ghostel-char-mode-map (kbd "M-w") #'ghostel-copy-all))

(provide 'nowis-ghostel)
;;; nowis-ghostel.el ends here
