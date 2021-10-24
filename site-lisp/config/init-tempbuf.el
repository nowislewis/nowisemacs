;;; init-tempbuf.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Liu Yi
;;
;; Author: Liu Yi <https://github.com/liuyi>
;; Maintainer: Liu Yi <1226228799@qq.com>
;; Created: October 24, 2021
;; Modified: October 24, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/liuyi/init-tempbuf
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(setq tempbuf-kill-message nil) ;不在Mode-line显示删除临时buffer提示消息
(setq tempbuf-minimum-timeout 30)       ;删除 buffer 的最低期限
(dolist (hook (list
               'compilation-mode-hook     ;编译模式
               'comint-mode-hook          ;comint 模式
               'completion-list-mode-hook ;补全列表模式
               'help-mode-hook            ;帮助模式
               'Info-mode-hook            ;Info 模式
               'calc-mode-hook            ;计算器模式
               'gnus-article-mode-hook    ;Gnus 文章模式
               'gnus-kill-file-mode       ;Gnus 删除文件模糊
               ))
  (add-hook
   hook
   #'(lambda ()
       (require 'tempbuf)
       (turn-on-tempbuf-mode))))        ;加载自动清理临时buffer

(provide 'init-tempbuf)
;;; init-tempbuf.el ends here
