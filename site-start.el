;;; site-start.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Liu Yi
;;
;; Author: Liu Yi <https://github.com/liuyi>
;; Maintainer: Liu Yi <1226228799@qq.com>
;; Created: October 25, 2021
;; Modified: October 25, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/liuyi/site-start
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(tool-bar-mode -1)                      ;禁用工具栏
;; (menu-bar-mode -1)                      ;禁用菜单栏
(scroll-bar-mode -1)                    ;禁用滚动条
(defun add-subdirs-to-load-path (dir)
  "recursive add directories to 'load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)
    (dolist (path load-path)
      (when (or (string-match-p "/node_modules" path)
                (string-match-p "/dist" path))
        (setq load-path (delete path load-path))))))

(add-subdirs-to-load-path "~/nowisemacs/site-lisp/")


(require 'init)
;;; site-start.el ends here
