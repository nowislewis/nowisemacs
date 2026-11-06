;;; nowis-completions-frame.el --- Minibuffer *Completions* in a child frame -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Show the native minibuffer *Completions* buffer in a fixed-size child frame
;; centered above the minibuffer, so opening it never resizes the parent's
;; windows and filtering never moves the frame's edges.
;;
;; Only the display action changes: the real minibuffer keeps input, point and
;; the input method, and every native completion command keeps working.

;;; Code:

(defconst nowis-completions-frame--border-width 2
  "Width in pixels of the border around the completions frame.")

(defconst nowis-completions-frame--width-ratio 0.6
  "Outer width of the completions frame as a fraction of its parent's.")

(defun nowis-completions-frame--geometry
    (parent-width available-height desired-height)
  "Return (LEFT TOP TEXT-WIDTH TEXT-HEIGHT) for a centered frame.
PARENT-WIDTH and AVAILABLE-HEIGHT, the space above the minibuffer, are in parent
pixels.  DESIRED-HEIGHT is the text height, clipped to stay inside that space."
  (let* ((chrome (* 2 nowis-completions-frame--border-width))
         (outer-width (floor (* parent-width
                                nowis-completions-frame--width-ratio)))
         (text-height (max 1 (min desired-height
                                  (- available-height chrome)))))
    (list (/ (- parent-width outer-width) 2)
          (max 0 (/ (- available-height text-height chrome) 2))
          (- outer-width chrome)
          text-height)))

(defun nowis-completions-frame--frames (&optional parent)
  "Return completions frames, only those of PARENT when it is non-nil."
  (filtered-frame-list
   (lambda (frame)
     (and (frame-parameter frame 'nowis-completions-frame)
          (or (null parent) (eq (frame-parent frame) parent))))))

(defun nowis-completions-frame--hide ()
  "Hide every visible completions frame.
Also serves as the frames' `auto-hide-function', which `bury-buffer' reaches
through `minibuffer-hide-completions'."
  (dolist (frame (nowis-completions-frame--frames))
    (when (frame-visible-p frame)
      (make-frame-invisible frame))))

(defun nowis-completions-frame--ensure (parent)
  "Return PARENT's completions frame, creating it hidden when missing.
The frame is reused because creating one on each keystroke is slow.  It is
found by its own parameter, not through `display-buffer-in-child-frame', whose
reuse does not work in Emacs 32 and could otherwise pick any child frame, such
as an input method's posframe."
  (or (car (nowis-completions-frame--frames parent))
      (let* ((frame
              (make-frame
               `((nowis-completions-frame . t)
                 (parent-frame . ,parent)
                 (visibility . nil)
                 (minibuffer . nil)
                 (no-accept-focus . t)
                 (no-focus-on-map . t)
                 (no-other-frame . t)
                 (undecorated . t)
                 (unsplittable . t)
                 (cursor-type . nil)
                 (tab-bar-lines . 0)
                 (left-fringe . 0)
                 (right-fringe . 0)
                 (vertical-scroll-bars . nil)
                 (horizontal-scroll-bars . nil)
                 (internal-border-width . 0)
                 (child-frame-border-width
                  . ,nowis-completions-frame--border-width)
                 (auto-hide-function . nowis-completions-frame--hide))))
             (window (frame-root-window frame)))
        (dolist (line '(mode-line-format header-line-format tab-line-format))
          (set-window-parameter window line 'none))
        frame)))

(defun nowis-completions-frame--sync-border-color (frame)
  "Give FRAME's border the current theme's `vertical-border' color.
The face is set for FRAME alone because other child frames share it, and only
when the color differs so that typing does not touch the face."
  (let ((color (face-foreground 'vertical-border nil t)))
    (unless (equal color (face-background 'child-frame-border frame))
      (set-face-background 'child-frame-border color frame))))

(defun nowis-completions-frame--place (frame)
  "Size and center FRAME over its parent, which may have been resized.
This runs before the buffer is filled, so width-sensitive annotations such as
Marginalia's align to the final width."
  (let* ((parent (frame-parent frame))
         (frame-resize-pixelwise t)
         (lines (or completions-max-height (frame-height parent))))
    (nowis-completions-frame--sync-border-color frame)
    (pcase-let ((`(,left ,top ,width ,height)
                 (nowis-completions-frame--geometry
                  (frame-native-width parent)
                  (nth 1 (window-pixel-edges (minibuffer-window parent)))
                  (* lines (frame-char-height frame)))))
      (set-frame-size frame width height t)
      (set-frame-position frame left top))))

(defun nowis-completions-frame--display (buffer alist)
  "Display BUFFER in the selected frame's completions frame.
This is a `display-buffer' action function; ALIST is its action alist.  The
window is undedicated first because `get-lru-window', used by
`display-buffer-use-some-frame', skips dedicated windows, and rededicated after
so burying BUFFER hides the frame instead of showing another buffer.  The frame
is shown last, once BUFFER is filled, so stale content never flashes."
  (let ((frame (nowis-completions-frame--ensure (selected-frame))))
    (nowis-completions-frame--place frame)
    (set-window-dedicated-p (frame-root-window frame) nil)
    (when-let* ((window
                 (display-buffer-use-some-frame
                  buffer
                  `((frame-predicate . ,(lambda (candidate)
                                          (eq candidate frame)))
                    (inhibit-switch-frame . t)
                    ,@alist))))
      (set-window-dedicated-p window 'soft)
      (make-frame-visible frame)
      window)))

(defun nowis-completions-frame--minibuffer-completions-p (buffer-name _action)
  "Return non-nil when BUFFER-NAME is *Completions* for the selected minibuffer.
This leaves `completion-help-at-point' in ordinary buffers on its usual window."
  (and (equal buffer-name "*Completions*")
       (window-minibuffer-p)))

(defconst nowis-completions-frame--display-rule
  '(nowis-completions-frame--minibuffer-completions-p
    (nowis-completions-frame--display))
  "The `display-buffer-alist' entry routing completions to the child frame.")

;;;###autoload
(define-minor-mode nowis-completions-frame-mode
  "Show minibuffer completions in a centered child frame."
  :global t
  :group 'minibuffer
  (if nowis-completions-frame-mode
      (progn
        (add-to-list 'display-buffer-alist
                     nowis-completions-frame--display-rule)
        ;; The default `read-minibuffer-restore-windows' restores only the
        ;; parent's windows on exit, which leaves a child frame on screen.
        (add-hook 'minibuffer-exit-hook #'nowis-completions-frame--hide))
    (setq display-buffer-alist
          (delete nowis-completions-frame--display-rule display-buffer-alist))
    (remove-hook 'minibuffer-exit-hook #'nowis-completions-frame--hide)
    (mapc #'delete-frame (nowis-completions-frame--frames))))

(provide 'nowis-completions-frame)
;;; nowis-completions-frame.el ends here
