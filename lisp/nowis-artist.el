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
  '("p" . artist-select-op-pen-line)
  '("l" . artist-select-op-line)
  '("a" . artist-toggle-second-arrow)
  '("A" . artist-toggle-first-arrow)
  '("r" . artist-select-op-rectangle)
  '("e" . artist-select-op-erase-rectangle)
  '("x" . artist-select-op-cut-rectangle)
  '("c" . artist-select-op-copy-rectangle)
  '("v" . artist-select-op-paste)
  '("t" . artist-select-op-text-overwrite)
  '("i" . nowis-artist-enter-insert)
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
