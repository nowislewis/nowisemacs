;;; nowis-ghostel.el --- Ghostel integration for Meow -*- lexical-binding: t; -*-

;;; Commentary:
;; Meow controls Ghostel's input mode: insert sends keys to the terminal,
;; normal mode provides Emacs-style line editing and navigation.
;;
;; `M-`' is the single entry point for project terminals and stays silent in
;; the common case: no terminal yet for the project -> create one; exactly one
;; -> jump to it; only with two or more does `consult-ghostel' preview show up.
;; `C-u M-`' tiles a fresh terminal in the project's slot, so it names and
;; identifies the terminal exactly like `ghostel-project' (`C-x p t') does.
;; A numeric prefix goes straight to that instance, as upstream.
;;
;; The summon path deliberately avoids the picker: previewing another terminal
;; in place resizes its PTY (SIGWINCH repaints the shell and relayouts any TUI
;; running in it), which costs more attention than it saves when there is
;; nothing to choose between.

;;; Code:

(require 'consult-ghostel)
(require 'ghostel-ime)
(require 'meow)

(declare-function ghostel-project-buffer-list "ghostel")
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


;;;; Summoning project terminals

(defun nowis-ghostel--same-window-action ()
  "Return the `display-buffer' action ghostel's own commands pop with.
Mirrors `ghostel--start' and `consult-ghostel--display', including the
`comint' category so `display-buffer-alist' rules treat the two alike."
  (append display-buffer--same-window-action '((category . comint))))

(defun nowis-ghostel--tile (create)
  "Split the largest window and run CREATE in the new window.
Splitting before creating matters: `ghostel--create' sizes the terminal from
the window that will display it, so a shell tiled beside a tall source window
gets the columns it will actually have.  The new window inherits the source
window's `default-directory', which is what puts the terminal in the project
of the window you were editing in."
  (let* ((source (get-largest-window))
         (side (if (> (window-total-width source)
                      (window-total-height source))
                  'right
                'below)))
    (select-window (split-window source nil side))
    (funcall create)))

(defun nowis-ghostel (&optional arg)
  "Go to this project's Ghostel terminal, creating one when the project has none.
Silent wherever there is nothing to choose between: no terminal yet for the
project, or exactly one (then re-entering it from inside is a no-op).  Only
with two or more does it hand over to `consult-ghostel-project', which
previews candidates.

With prefix ARG, tile a fresh terminal in the largest window.  A numeric ARG
switches to that project instance without tiling.  Both follow the prefix
conventions of the upstream `consult-ghostel' commands.

The create paths go through `ghostel-project', so a terminal summoned here has
the identity and buffer name that \\[ghostel-project] (`C-x p t') finds and reuses
rather than a separate anonymous `*ghostel*' slot family."
  (interactive "P")
  (cond
   ((numberp arg) (ghostel-project arg))
   ((consp arg)
    ;; Confirm a project before splitting, so an unexpected prompt does not
    ;; land in a window the user did not ask for.
    (unless (project-current t)
      (user-error "No project to open a terminal for"))
    (nowis-ghostel--tile (lambda () (ghostel-project t))))
   (t
    ;; Check `project-current' without the prompt that
    ;; `ghostel-project-buffer-list' and `ghostel-project' would otherwise
    ;; issue when point sits outside any project.
    (unless (project-current)
      (user-error "No project here"))
    (let ((buffers (ghostel-project-buffer-list)))
      (cond
       ((null buffers) (ghostel-project))
       ((null (cdr buffers))
        (pop-to-buffer (car buffers) (nowis-ghostel--same-window-action)))
       (t (consult-ghostel-project)))))))


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
  (define-key ghostel-mode-map (kbd "M-`") #'nowis-ghostel)
  ;; Char mode does not inherit `ghostel-mode-map'.
  (define-key ghostel-char-mode-map (kbd "M-`") #'nowis-ghostel)
  (define-key ghostel-char-mode-map (kbd "<escape>") #'nowis-ghostel-char-escape)
  (define-key ghostel-char-mode-map (kbd "M-q") #'nowis-ghostel-char-send-escape)
  (define-key ghostel-char-mode-map (kbd "C-\\") #'toggle-input-method)
  (define-key ghostel-char-mode-map (kbd "C-y") #'ghostel-yank)
  (define-key ghostel-char-mode-map (kbd "M-w") #'ghostel-copy-all))

(provide 'nowis-ghostel)
;;; nowis-ghostel.el ends here
