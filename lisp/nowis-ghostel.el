;;; nowis-ghostel.el --- Ghostel integration for Meow -*- lexical-binding: t; -*-

;;; Commentary:
;; Meow controls Ghostel's input mode: insert enters `ghostel-semi-char-mode'
;; (sending terminal keys while keeping Emacs `C-c' / `C-x' / `M-x' prefixes),
;; normal mode provides read-only navigation and copying with live output.
;;
;; Project terminals use upstream `ghostel-project'; window management stays
;; with Emacs.  Use `consult-ghostel-project' explicitly to pick a terminal.

;;; Code:

(require 'ghostel)
(require 'ghostel-ime)
(require 'meow)


;;;; Meow input modes & Alternate-screen paging

(defun nowis-ghostel--enter-insert-mode ()
  "Enter `ghostel-semi-char-mode' and restore Meow's insert cursor."
  (ghostel-semi-char-mode)
  (meow--update-cursor))

(defun nowis-ghostel--enter-emacs-mode ()
  "Enter live browsing without toggling out of an existing read-only mode."
  (when (and ghostel--term (not (memq ghostel--input-mode '(emacs copy))))
    (ghostel-emacs-mode)
    (meow--update-cursor)))

(defun nowis-ghostel--sync-normal-mode ()
  "Repair normal-mode browsing after terminal initialization or mode drift."
  (when (bound-and-true-p meow-normal-mode)
    (nowis-ghostel--enter-emacs-mode)))

(defun nowis-ghostel--set-up-meow ()
  "Start in Meow normal mode and synchronize its insert transitions."
  ;; Meow owns leaving browsing; copying must not restore terminal input.
  (setq-local ghostel-readonly-fast-exit nil)
  (meow-normal-mode)
  (add-hook 'meow-insert-enter-hook #'nowis-ghostel--enter-insert-mode nil t)
  (add-hook 'meow-insert-exit-hook #'nowis-ghostel--enter-emacs-mode nil t)
  ;; `ghostel-mode-hook' runs before `ghostel--term' exists; synchronize before
  ;; the first command, once initialization has installed its input keymap.
  (add-hook 'pre-command-hook #'nowis-ghostel--sync-normal-mode nil t)
  (message "%s"
           (substitute-command-keys
            "Ghostel: \\[meow-insert] input, \\<meow-insert-state-keymap>\\[meow-insert-exit] browse, \\<ghostel-semi-char-mode-map>\\[nowis-ghostel-send-escape] send literal ESC")))

(defun nowis-ghostel-send-escape ()
  "Send a literal ESC to the terminal."
  (interactive)
  (ghostel-send-key "escape"))

(defun nowis-ghostel-scroll-up (&optional arg)
  "Scroll down one page, or send PageDown on the alternate screen."
  (interactive "^P")
  (if (ghostel-alt-screen-p)
      (ghostel-send-key "next")
    (scroll-up-command arg)))

(defun nowis-ghostel-scroll-down (&optional arg)
  "Scroll up one page, or send PageUp on the alternate screen."
  (interactive "^P")
  (if (ghostel-alt-screen-p)
      (ghostel-send-key "prior")
    (scroll-down-command arg)))

(defun nowis-ghostel-beginning-of-buffer ()
  "Move to buffer start, or send C-Home on the alternate screen."
  (interactive "^")
  (if (ghostel-alt-screen-p)
      (ghostel-send-key "home" "ctrl")
    (goto-char (point-min))))

(defun nowis-ghostel-end-of-buffer ()
  "Move to buffer end, or send C-End on the alternate screen."
  (interactive)
  (if (ghostel-alt-screen-p)
      (ghostel-send-key "end" "ctrl")
    (ghostel-readonly-end-of-buffer)))

(defun nowis-ghostel--wheel (event fn)
  "Forward wheel EVENT via FN in input or alternate-screen mode."
  (with-current-buffer (window-buffer (ghostel--event-window event))
    (let ((ghostel--input-mode (if (ghostel-alt-screen-p)
                                   'semi-char
                                 ghostel--input-mode)))
      (funcall fn event))))

(defun nowis-ghostel-wheel-up (event)
  "Forward wheel-up EVENT to terminal in input or alternate-screen mode."
  (interactive "e")
  (nowis-ghostel--wheel event #'ghostel--scroll-intercept-up))

(defun nowis-ghostel-wheel-down (event)
  "Forward wheel-down EVENT to terminal in input or alternate-screen mode."
  (interactive "e")
  (nowis-ghostel--wheel event #'ghostel--scroll-intercept-down))


;;;; TRAMP-aware shell integration

(defun nowis-ghostel-find-file-other-window (path)
  "Open PATH in another window, preserving the current remote host."
  (find-file-other-window
   (concat (or (file-remote-p default-directory) "") path)))


;;;; Setup

(defun nowis-ghostel-setup ()
  "Install the minimal Ghostel integration."
  (setopt ghostel-keymap-exceptions
          (delete-dups (append ghostel-keymap-exceptions '("M-`" "M-o"))))
  (add-hook 'ghostel-mode-hook #'nowis-ghostel--set-up-meow)
  (add-hook 'ghostel-mode-hook #'ghostel-ime-mode)
  (setf (alist-get "find-file-other-window" ghostel-eval-cmds nil nil #'equal)
        '(nowis-ghostel-find-file-other-window))
  (keymap-set ghostel-semi-char-mode-map "M-q" #'nowis-ghostel-send-escape)
  (define-keymap :keymap ghostel-readonly-mode-map
    "<remap> <scroll-up-command>"              #'nowis-ghostel-scroll-up
    "<remap> <scroll-down-command>"            #'nowis-ghostel-scroll-down
    "<remap> <beginning-of-buffer>"            #'nowis-ghostel-beginning-of-buffer
    "<remap> <ghostel-readonly-end-of-buffer>" #'nowis-ghostel-end-of-buffer)
  (define-keymap :keymap ghostel--scroll-intercept-map
    "<mouse-4>"    #'nowis-ghostel-wheel-up
    "<wheel-up>"   #'nowis-ghostel-wheel-up
    "<mouse-5>"    #'nowis-ghostel-wheel-down
    "<wheel-down>" #'nowis-ghostel-wheel-down))

(provide 'nowis-ghostel)
;;; nowis-ghostel.el ends here
