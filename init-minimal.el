;; -*- lexical-binding: t; -*-
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
