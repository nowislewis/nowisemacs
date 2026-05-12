;;; nowis-modeline.el --- Minimal mode-line -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile (require 'subr-x))
(require 'vc-hooks)

(setq mode-line-right-align-edge 'right-margin
      mode-line-compact t)

;;; Faces

(defface nowis-ml-dim  '((t :inherit shadow))              "" :group 'nowis-modeline)
(defface nowis-ml-info '((t :inherit font-lock-type-face)) "" :group 'nowis-modeline)

;;; shrink-path cache

(defvar-local nowis-ml--path-cache nil)

(defun nowis-ml--path-update (&rest _)
  (setq nowis-ml--path-cache
        (let* ((file (buffer-file-name))
               (non-essential t)
               (root (and file
                          (fboundp 'project-current)
                          (fboundp 'project-root)
                          (when-let* ((pr (project-current nil)))
                            (expand-file-name (project-root pr)))))
               (icon (when (fboundp 'nerd-icons-icon-for-buffer)
                       (nerd-icons-icon-for-buffer))))
          (cond
           ((and root (string-prefix-p root file))
            (let* ((rel   (string-remove-prefix root file))
                   (dirs  (butlast (split-string (or (file-name-directory rel) "") "/" t)))
                   (abbr  (and dirs
                               (propertize
                                (concat (mapconcat (lambda (s) (substring s 0 1)) dirs "/") "/")
                                'face 'nowis-ml-dim)))
                   (pname (propertize (file-name-nondirectory (directory-file-name root))
                                      'face 'nowis-ml-info)))
              (concat icon (and icon " ") pname "/" abbr (file-name-nondirectory file))))
           (file
            (concat icon (and icon " ")
                    (propertize (concat (file-name-nondirectory
                                        (directory-file-name (file-name-directory file))) "/")
                                'face 'nowis-ml-dim)
                    (file-name-nondirectory file)))
           (t (propertize (buffer-name) 'face 'nowis-ml-dim))))))

(add-hook 'find-file-hook  #'nowis-ml--path-update)
(add-hook 'after-save-hook #'nowis-ml--path-update)
(advice-add 'rename-buffer :after #'nowis-ml--path-update)

(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when buffer-file-name (nowis-ml--path-update))))

;;; mode-line-format
(setq-default mode-line-format
              '((:eval (and (fboundp 'meow-indicator) (meow-indicator)))
                (defining-kbd-macro mode-line-defining-kbd-macro)
                " "
                nowis-ml--path-cache
                "  "
                (:propertize ("%l %p") face font-lock-constant-face)
                mode-line-format-right-align
                (:propertize ("" current-input-method-title) face font-lock-keyword-face)
                " "
                mode-line-misc-info
                " "
                mode-line-modified
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
