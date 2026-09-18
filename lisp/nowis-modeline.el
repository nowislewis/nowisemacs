;;; nowis-modeline.el --- Minimal mode-line -*- lexical-binding: t -*-
;;; Code:

(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))

(setq mode-line-right-align-edge 'right-margin)

;;; Faces

(defface nowis-ml-dim  '((t :inherit shadow)) "" :group 'nowis-modeline)
(defface nowis-ml-file
  '((t :weight bold))
  "Face used for the buffer file name in the mode line."
  :group 'nowis-modeline)


;;; Buffer name

;; nil means "not built yet"; rebuilt by the next redisplay, so buffers that
;; never run `find-file-hook' (dired, magit, shells) also get a name.
(defvar-local nowis-ml--path-cache nil)
;; Risky, otherwise Emacs strips the `face' properties in the cached string.
(put 'nowis-ml--path-cache 'risky-local-variable t)

(defun nowis-ml--project-root ()
  "Project root of this buffer, or nil.  Remote dirs are skipped so that
redisplay never blocks on Tramp."
  (and (featurep 'project)
       (not (file-remote-p default-directory))
       (let* ((non-essential t)
              (pr (project-current nil)))
         (and pr (expand-file-name (project-root pr))))))

(defun nowis-ml--dir-prefix (dir root)
  "Dim path prefix for DIR: \"root/a/b/\" with components below ROOT shrunk
to one character.  Outside ROOT (or with ROOT nil) only DIR's own name."
  ;; `file-in-directory-p' resolves truenames (~150us), so consult it only
  ;; when the cheap string test fails; then derive the relative path from
  ;; truenames as well, or symlinks would leave ".." components in it.
  (let ((rel (cond ((null root) nil)
                   ((string-prefix-p root dir) (file-relative-name dir root))
                   ((file-in-directory-p dir root)
                    (file-relative-name (file-truename dir) (file-truename root))))))
    (concat (string-join
             (if rel
                 (cons (file-name-nondirectory (directory-file-name root))
                       (mapcar (lambda (s) (substring s 0 1))
                               (remove "." (split-string rel "/" t))))
               (list (file-name-nondirectory (directory-file-name dir))))
             "/")
            "/")))

(defun nowis-ml--path-string ()
  "Mode-line name: file buffers get an abbreviated directory, others their
buffer name prefixed by the project name."
  (let* ((own-file (buffer-file-name))
         ;; Indirect buffers (clone-indirect-buffer, org narrowing) have no
         ;; file of their own, but their base buffer does.
         (file (or own-file (buffer-file-name (buffer-base-buffer))))
         (non-essential t)
         (icon (and (fboundp 'nerd-icons-icon-for-buffer)
                    (ignore-errors (nerd-icons-icon-for-buffer))))
         (root (nowis-ml--project-root))
         ;; Keep the indirect buffer's own name; only the directory is borrowed.
         (name (if own-file (file-name-nondirectory own-file) (buffer-name)))
         (dir (cond (file (nowis-ml--dir-prefix (file-name-directory file) root))
                    ;; Dired at the project root already shows it as NAME.
                    (root (let ((prefix (nowis-ml--dir-prefix root nil)))
                            (unless (equal prefix (concat name "/")) prefix))))))
    (concat (and icon (concat icon " "))
            (and dir (propertize dir 'face 'nowis-ml-dim))
            (propertize name 'face 'nowis-ml-file))))

(defun nowis-ml--path-update (&rest _)
  "Rebuild and return the cached name."
  (setq nowis-ml--path-cache (nowis-ml--path-string)))

(defun nowis-ml--path-invalidate (&rest _)
  "Drop the cached name; next redisplay rebuilds it."
  (setq nowis-ml--path-cache nil))

(defconst nowis-ml--icon-modified
  (propertize "󰆓" 'face 'nerd-icons-red))
(defconst nowis-ml--icon-read-only
  (propertize "" 'face 'nerd-icons-lyellow))

(defun nowis-ml--modified ()
  (cond ((buffer-modified-p) nowis-ml--icon-modified)
        (buffer-read-only    nowis-ml--icon-read-only)))

(dolist (hook '(find-file-hook
                after-save-hook
                after-change-major-mode-hook ; icon/project depend on the mode
                dired-after-readin-hook))    ; dired reuses one buffer per dir
  (add-hook hook #'nowis-ml--path-invalidate))
(advice-add 'rename-buffer :after #'nowis-ml--path-invalidate)

(defun nowis-ml-refresh ()
  "Invalidate the cached name in all buffers and redraw."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (nowis-ml--path-invalidate)))
  (force-mode-line-update t))

(with-eval-after-load 'project
  (nowis-ml-refresh))

;;; mode-line-format
(setq-default mode-line-format
              '((:eval (and (fboundp 'meow-indicator) (meow-indicator)))
                " "
                (defining-kbd-macro "󰧟")
                " "
                (:eval (nowis-ml--modified))
                " "
                (:eval (or nowis-ml--path-cache (nowis-ml--path-update)))
                ;; Folding these separators into the neighbouring `:propertize'
                ;; was measured 5-6% SLOWER: a list-form :propertize costs more
                ;; than the bare element plus a literal space it would replace.
                "  "
                (:propertize "%p %l" face nowis-ml-dim)
                mode-line-format-right-align
                (dired-rsync-modeline-status dired-rsync-modeline-status)
                " "
                (:propertize ("" current-input-method-title) face font-lock-keyword-face)
                " "
                mode-line-misc-info
                " "
                (:propertize ("%I") face nowis-ml-dim)
                "  "
                (vc-mode vc-mode)
                " "
                (:propertize mode-name face bold)))

;;; Setup

(with-eval-after-load 'meow
  (advice-add 'meow-setup-mode-line :override #'ignore))

(set-face-attribute 'mode-line          nil :height 0.88 :box nil)
(set-face-attribute 'mode-line-active   nil :height 0.88 :box nil)
(set-face-attribute 'mode-line-inactive nil :height 0.88 :box nil)

(provide 'nowis-modeline)

;;; Benchmark

(defun nowis-ml-benchmark (&optional seconds)
  "逐项分析 `mode-line-format' 中每个元素的渲染耗时。
SECONDS 为每项采样时长（默认 1 秒）。在当前 buffer 的上下文中测量，
所以要量 dired/magit 的开销就在那个 buffer 里调用。"
  (interactive)
  (require 'benchmark)
  (let* ((secs (or seconds 1.0))
         (buffer (current-buffer))
         ;; 每项都必须包进一层 list：直接 splice `(:eval X)' 会把 :eval
         ;; 摊平成普通元素，渲染出 "*invalid*" 而量不到真实开销。
         (measure (lambda (format)
                    (benchmark-call
                     (lambda () (format-mode-line format nil nil buffer)) secs)))
         (items (append (mapcar (lambda (elem) (cons elem (list "" elem)))
                                mode-line-format)
                        (list (cons "[total]" mode-line-format)))))
    (with-output-to-temp-buffer "*nowis-ml-benchmark*"
      (princ (format "buffer: %s\n\n" (buffer-name buffer)))
      (pcase-dolist (`(,label . ,format) items)
        (pcase-let ((`(,reps ,time ,gcs) (funcall measure format)))
          (princ (format "%7.2fµs  GC×%-3d %s\n"
                         (* (/ time reps) 1e6) gcs
                         (truncate-string-to-width (format "%S" label) 58))))))))

;; (defmacro +measure-time(&rest body)
;;   `(let ((time (current-time)))
;;      ,@body
;;      (message "%.06fs" (float-time (time-since time)))
;;      )
;;   )
;; (+measure-time (format-mode-line mode-line-format))

;; nowis-modeline.el ends here
