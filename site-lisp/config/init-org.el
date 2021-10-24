;;; init-org.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Liu Yi
;;
;; Author: Liu Yi <https://github.com/liuyi>
;; Maintainer: Liu Yi <1226228799@qq.com>
;; Created: October 24, 2021
;; Modified: October 24, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/liuyi/init-org
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;;;
;;; 设置org mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)


;;; 导出相关的设置
(after! org
     (setq org-latex-pdf-process
     '("xelatex -interaction nonstopmode %f"
       "bibtex %b"
       "xelatex -interaction nonstopmode %f"
      "xelatex -interaction nonstopmode %f"))

     ;;设置粗体高亮颜色
     (require 'cl)
     (setq org-emphasis-alist
          (cons '("*" '(:emphasis t :foreground "pale violet red" :weight bold))
               (delete* "*" org-emphasis-alist :key 'car :test 'equal))
     )
     (setq org-emphasis-alist
          (cons '("/" '(:slant t :foreground "green3" :slant italic))
                (delete* "/" org-emphasis-alist :key 'car :test 'equal))
     )


;;; org TODO 设置

(after! org
(setq-default org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq-default org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))
)

;;; 美化
(setq org-hide-emphasis-markers t)
(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "ℱ")
                                       ("#+END_SRC" . "Ⅎ")
                                       ("#+begin_src" . "ℱ")
                                       ("#+end_src" . "Ⅎ")))
(add-hook 'org-mode-hook 'prettify-symbols-mode)


(provide 'init-org)
;;; init-org.el ends here
