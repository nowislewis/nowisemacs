;;; nowis-artist.el --- Meow drawing keys for Org Artist blocks -*- lexical-binding: t; -*-

(require 'artist)
(require 'meow)
(require 'org-src)

(defvar-local nowis-artist--saved-overriding-maps nil
  "Keymap overrides to restore when leaving text input in Artist.")

(defvar nowis-artist-draw-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    map)
  "One-key drawing tools for Org Artist source editing buffers.")

(meow-define-state artist-draw
  "Select Artist tools without leaving the Org source editing buffer."
  :lighter " [Draw]"
  :keymap nowis-artist-draw-keymap)
(setf (alist-get 'artist-draw meow-replace-state-name-list) "D")

(defun nowis-artist-enter-insert ()
  "Enter Meow Insert, letting Picture mode handle text instead of Artist."
  (interactive)
  (setq-local nowis-artist--saved-overriding-maps
              minor-mode-overriding-map-alist)
  (let ((insert-map (copy-keymap meow-insert-state-keymap)))
    (keymap-set insert-map "<escape>" #'nowis-artist-leave-insert)
    (setq-local minor-mode-overriding-map-alist
                `((meow-insert-mode . ,insert-map)
                  (artist-mode . ,(make-sparse-keymap))
                  ,@minor-mode-overriding-map-alist)))
  (meow-insert-mode 1))

(defun nowis-artist-leave-insert ()
  "Restore Artist keys and return to the Draw state."
  (interactive)
  (setq-local minor-mode-overriding-map-alist
              nowis-artist--saved-overriding-maps)
  (meow-artist-draw-mode 1))

(meow-define-keys 'artist-draw
  '("1" . artist-select-op-copy-rectangle)
  '("v" . artist-select-op-copy-rectangle)
  '("2" . artist-select-op-rectangle)
  '("r" . artist-select-op-rectangle)
  '("R" . artist-select-op-square)
  '("3" . artist-select-op-cut-rectangle)
  '("4" . artist-select-op-circle)
  '("o" . artist-select-op-circle)
  '("E" . artist-select-op-ellipse)
  '("5" . artist-select-op-line)
  '("6" . artist-select-op-line)
  '("l" . artist-select-op-line)
  '("7" . artist-select-op-pen-line)
  '("p" . artist-select-op-paste)
  '("P" . artist-select-op-poly-line)
  '("f" . artist-select-op-flood-fill)
  '("a" . artist-toggle-second-arrow)
  '("A" . artist-toggle-first-arrow)
  '("e" . artist-select-op-erase-rectangle)
  '("x" . artist-select-op-cut-rectangle)
  '("c" . artist-select-op-copy-rectangle)
  '("t" . artist-select-op-text-overwrite)
  '("i" . nowis-artist-enter-insert)
  '("RET" . artist-key-set-point)
  '("u" . meow-undo)
  '("U" . vundo)
  '("?" . artist-select-operation)
  '("<escape>" . meow-normal-mode)
  '("q" . org-edit-src-exit)
  '("C-c '" . org-edit-src-exit))

(defun nowis-artist--src-enable-draw ()
  "Enter Draw only when editing an Org source block in Artist mode."
  (when (and artist-mode
             (equal (car org-src--babel-info) "artist"))
    (let ((map (make-sparse-keymap)))
      (keymap-set map "C-c d" #'meow-artist-draw-mode)
      (use-local-map (make-composed-keymap map (current-local-map))))
    (meow-artist-draw-mode 1)))

(add-hook 'org-src-mode-hook #'nowis-artist--src-enable-draw)

(provide 'nowis-artist)
;;; nowis-artist.el ends here
