;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Liu Yi
;;
;; Author: Liu Yi <https://github.com/liuyi>
;; Maintainer: Liu Yi <1226228799@qq.com>
;; Created: October 24, 2021
;; Modified: October 24, 2021
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; 加速配置。
(require 'init-accelerate)

;; 字体设置
(require 'init-font)


(let (
      ;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))


  ;; 定义一些启动目录，方便下次迁移修改
  (defvar nowisemacs-root-dir (file-truename "~/nowisemacs/site-lisp"))
  (defvar nowisemacs-config-dir (concat nowisemacs-root-dir "/config"))
  (defvar nowisemacs-extension-dir (concat nowisemacs-root-dir "/extensions"))
  (with-temp-message ""                 ;抹掉插件启动的输出
    ;;(require 'benchmark-init-modes)
    ;;(require 'benchmark-init)
    ;;(benchmark-init/activate)
    ;;
    (require 'init-fullscreen)
    (require 'init-generic)
    (require 'init-theme)
    ;; (when (featurep 'cocoa)
    ;;   (require 'cache-path-from-shell))
    (require 'lazy-load)
    ;; (require 'one-key)
;;;; 可能不用－－－－－－－－－－－－－－－－
    (require 'awesome-pair)
    (require 'display-line-numbers)
    ;; (require 'basic-toolkit)
    ;; (require 'redo)
    ;; (require 'highlight-parentheses)
;;;; －－－－－－－－－－－－－－－－－－－－
    (when (eq system-type 'gnu/linux)
      (require 'init-tree-sitter))
    (require 'init-awesome-tray)
    (require 'init-awesome-tab)
    (require 'init-backup)
    (require 'init-line-number)
    (require 'init-auto-save)
    (require 'init-mode)
    (require 'init-awesome-pair)
    (require 'init-indent)
    ;; (require 'init-one-key)
    (require 'init-meow)
    (require 'init-key)
    (require 'init-which-key)
    (require 'init-isearch-mb)
    (require 'init-performance)
    ;; (require 'init-tempbuf)



    ;; 可以延后加载的
    (run-with-idle-timer 1 nil #'(lambda ()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                   (require 'init-company-mode)
                                   (require 'init-vertico)


                                   (require 'init-magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                   (require 'init-org)
                                   (require 'init-org-roam)))))

(provide 'init)
;;; init.el ends here
