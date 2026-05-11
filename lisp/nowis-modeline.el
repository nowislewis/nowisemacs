;;; nowis-modeline.el --- Minimal mode-line -*- lexical-binding: t -*-
;;; Code:

(eval-when-compile (require 'subr-x))
(require 'vc-hooks)

;;; Faces

(defface nowis-ml-dim  '((t :inherit shadow))              "" :group 'nowis-modeline)
(defface nowis-ml-info '((t :inherit font-lock-type-face)) "" :group 'nowis-modeline)

;;; shrink-path

(defvar-local nowis-ml--path-cache nil
  "已格式化的路径字符串缓存。")

(defun nowis-ml--path-update (&rest _)
  "更新当前 buffer 的路径缓存。"
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

;; 对已有 buffer 初始化缓存
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when buffer-file-name
      (nowis-ml--path-update))))

;;; mode-line-format

(setq-default mode-line-format
              '((:eval (and (fboundp 'meow-indicator) (meow-indicator)))
                (defining-kbd-macro mode-line-defining-kbd-macro)  ; macro 录制
                " "
                nowis-ml--path-cache                               ; shrink-path
                mode-line-format-right-align
                mode-line-misc-info                                ; eglot/org-clock/…
                " "
                (flymake-mode flymake-mode-line-format)            ; flymake
                " "
                (:eval (when (and (fboundp 'profiler-running-p)    ; profiler
                                 (profiler-running-p))
                         (propertize "Prof" 'face 'warning)))
                " "
                #("%n" 0 2 (help-echo "Narrowing"))               ; narrow
                mode-line-mule-info
                "  "
                mode-line-modified
                "  "
                mode-line-position
                " "
                (:eval                                             ; branch + icon
                 (when vc-mode
                   (concat
                    (when (fboundp 'nerd-icons-octicon)
                      (propertize (nerd-icons-octicon "nf-oct-git_branch")
                                  'face `(:inherit shadow)))
                    (propertize vc-mode 'face 'shadow))))
                " "
                (:propertize mode-name face bold)
                ))

;;; Setup

(with-eval-after-load 'meow
  (advice-add 'meow-setup-mode-line :override #'ignore))

(set-face-attribute 'mode-line          nil :inherit 'unspecified :box 'unspecified)
(set-face-attribute 'mode-line-active   nil :inherit 'unspecified :box 'unspecified)
(set-face-attribute 'mode-line-inactive nil :box 'unspecified)

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
                      (mapcar (lambda (elem)
                                (cons (format "%S" elem) elem))
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
          (princ (format "%-60s %6.2fµs  GC×%d
"
                         (truncate-string-to-width (car item) 60)
                         per-call gc-count)))))))

;;; nowis-modeline.el ends here
