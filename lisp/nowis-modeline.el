;;; nowis-modeline.el --- Minimal mode-line -*- lexical-binding: t -*-
;;; Code:

(require 'vc-hooks)
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))

(setq mode-line-right-align-edge 'right-margin
      mode-line-compact t)

;;; Faces

(defface nowis-ml-dim  '((t :inherit shadow)) "" :group 'nowis-modeline)
(defface nowis-ml-file
  '((t :weight bold))
  "Face used for the buffer file name in the mode line."
  :group 'nowis-modeline)
(defface nowis-ml-modified '((t :inherit warning :weight bold)) "" :group 'nowis-modeline)
(defface nowis-ml-read-only '((t :inherit nowis-ml-dim :weight bold)) "" :group 'nowis-modeline)

;;; shrink-path cache

(defvar-local nowis-ml--path-cache nil)
;; Mode-line symbols that may contain text properties must be marked risky,
;; otherwise Emacs strips/ignores properties such as `face'.
(put 'nowis-ml--path-cache 'risky-local-variable t)

(defun nowis-ml--path-update (&rest _)
  (setq nowis-ml--path-cache
        (let* ((file (buffer-file-name))
               (non-essential t)
               (icon (and (fboundp 'nerd-icons-icon-for-buffer)
                          (nerd-icons-icon-for-buffer)))
               (name (if file (file-name-nondirectory file) (buffer-name))))
          (if file
              (let* ((project (and (featurep 'project) (project-current nil)))
                     (root (and project (expand-file-name (project-root project))))
                     (in-project (and root (file-in-directory-p file root)))
                     (rel-dir (and in-project
                                   (file-name-directory (file-relative-name file root))))
                     (dir (if in-project
                              (concat (file-name-nondirectory (directory-file-name root)) "/"
                                      (and rel-dir
                                           (concat (mapconcat (lambda (s) (substring s 0 1))
                                                              (split-string rel-dir "/" t) "/")
                                                   "/")))
                            (concat (file-name-nondirectory
                                     (directory-file-name
                                      (file-name-parent-directory file)))
                                    "/"))))
                (concat (and icon (concat icon " "))
                        (propertize dir 'face 'nowis-ml-dim)
                        (propertize name 'face 'nowis-ml-file)))
            (propertize name 'face 'nowis-ml-file)))))

(defun nowis-ml--modified ()
  (cond ((buffer-modified-p) (propertize "●" 'face 'nowis-ml-modified))
        (buffer-read-only    (propertize "RO" 'face 'nowis-ml-read-only))))

(add-hook 'find-file-hook  #'nowis-ml--path-update)
(add-hook 'after-save-hook #'nowis-ml--path-update)
(advice-add 'rename-buffer :after #'nowis-ml--path-update)

(defun nowis-ml-refresh ()
  "Refresh cached mode-line path for all buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (nowis-ml--path-update)))
  (force-mode-line-update t))

(nowis-ml-refresh)
(with-eval-after-load 'project
  (nowis-ml-refresh))

;;; mode-line-format
(setq-default mode-line-format
              '((:eval (and (fboundp 'meow-indicator) (meow-indicator)))
                (defining-kbd-macro mode-line-defining-kbd-macro)
                " "
                (:eval nowis-ml--path-cache)
                "  "
                (:propertize ("%p %l") face font-lock-constant-face)
                mode-line-format-right-align
                (:propertize ("" current-input-method-title) face font-lock-keyword-face)
                " "
                mode-line-misc-info
                " "
                (:eval (nowis-ml--modified))
                " "
                (:propertize ("%I") face nowis-ml-dim)
                "  "
                (vc-mode vc-mode)
                " "
                (:propertize mode-name face bold)
                ))

;;; Setup

(with-eval-after-load 'meow
  (advice-add 'meow-setup-mode-line :override #'ignore))

(set-face-attribute 'mode-line          nil :height 0.88 :box nil)
(set-face-attribute 'mode-line-active   nil :height 0.88 :box nil)
(set-face-attribute 'mode-line-inactive nil :height 0.88 :box nil)

(line-number-mode   1)
(column-number-mode 1)
(size-indication-mode 1)

(provide 'nowis-modeline)

;;; Benchmark

(defun nowis-ml-benchmark (&optional seconds)
  "自动逐项分析 mode-line-format 中每个元素的渲染耗时。
SECONDS 为采样时长（默认 1 秒）。"
  (interactive)
  (require 'benchmark)
  (let* ((secs (or seconds 1.0))
         (items (cons (cons "[total]" mode-line-format)
                      (mapcar (lambda (elem) (cons (format "%S" elem) elem))
                              mode-line-format))))
    (with-output-to-temp-buffer "*nowis-ml-benchmark*"
      (dolist (item items)
        (let* ((expr (cdr item))
               (func (if (listp expr)
                         (lambda () (format-mode-line `("" ,@expr)))
                       (lambda () (format-mode-line expr))))
               (r        (benchmark-call func secs))
               (reps     (nth 0 r))
               (time     (nth 1 r))
               (gc-count (nth 2 r))
               (per-call (* (/ time reps) 1e6)))
          (princ (format "%-60s %6.2fµs  GC×%d\n"
                         (truncate-string-to-width (car item) 60)
                         per-call gc-count)))))))

;; (defmacro +measure-time(&rest body)
;;   `(let ((time (current-time)))
;;      ,@body
;;      (message "%.06fs" (float-time (time-since time)))
;;      )
;;   )
;; (+measure-time (format-mode-line mode-line-format))

;; nowis-modeline.el ends here
