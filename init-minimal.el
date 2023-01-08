  (setq package-enable-at-startup nil)

  ;; Inhibit resizing frame
  (setq frame-inhibit-implied-resize t)

  ;; no title bar
  ;; (add-to-list 'default-frame-alist '(undecorated . t))
  ;; Faster to disable these here (before they've been initialized)
  (push '(alpha-background . 85) default-frame-alist)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  ;; (push '(undecorated . t) default-frame-alist)
  (setq initial-major-mode 'fundamental-mode ;; 默认用最简单的模式
        package--init-file-ensured t
        inhibit-startup-message t             ; 关闭启动 Emacs 时的欢迎界面
        )

(icomplete-vertical-mode)
(setq use-short-answers t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq package-enable-at-startup nil)
  (set-default-coding-systems 'utf-8)
  (setq make-backup-files nil)
  (setq auto-save-default nil)

(defun new-eshell(name)
  "create a new eshell with a given name"
  (interactive "sInput a name:")
  (let ((eshell-buffer-name name))
    (eshell)))
(load-theme 'modus-vivendi t)
(global-set-key
 [remap list-buffers]
 #'ibuffer)
