;;; init-meow.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Liu Yi
;;
;; Author: Liu Yi <https://github.com/liuyi>
;; Maintainer: Liu Yi <1226228799@qq.com>
;; Created: October 24, 2021
;; Modified: October 24, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/liuyi/init-meow
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:



(require 'meow)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . meow-motion-origin-command)
   '("k" . meow-motion-origin-command)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("a" . meow-append)
   ;;'("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-clipboard-kill)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   ;; (cons "g" (concat doom-leader-alt-key " c"))
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-search)
   '("N" . meow-pop-search)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-line)
   '("S" . meow-kmacro-lines)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("u" . meow-undo)
   '("U" . undo-tree-redo)
   '("v" . meow-visit)
   '("V" . meow-kmacro-matches)
   '("w" . meow-block)
   '("W" . meow-block-expand)
   '("x" . meow-C-d)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("{" . meow-inner-of-thing)
   '("}" . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("." . repeat)
   '("," . meow-join)
   '("\\" . quoted-insert)
   '("<escape>" . meow-cancel)
   '("!" . meow-start-kmacro-or-insert-counter)
   '("@" . meow-end-or-call-kmacro)
   '("'" . meow-comment)
   '("/" . meow-last-buffer)
   ))
;; (setq doom-leader-alt-key "M-SPC")
  (meow-global-mode 1)

  ;; (custom-set-default meow-cursor-type-normal '(box 4))

  ;; meow-setup 用于自定义按键绑定，可以直接使用下文中的示例
  (meow-setup)
  ;; 如果你需要在 NORMAL 下使用相对行号（基于 display-line-numbers-mode）
  ;; (meow-setup-line-number)
  ;; 如果你需要自动的 mode-line 设置（如果需要自定义见下文对 `meow-indicator' 说明）
  (meow-setup-indicator)


  (setq which-key-show-transient-maps t)
  (setq meow-use-keypad-when-execute-kbd nil)
  (setq meow-expand-exclude-mode-list nil)
  (setq meow-use-clipboard t)
  (setq meow-cursor-type-normal '(bar . 5))
  (setq meow-cursor-type-insert '(bar . 1))
  (setq meow-replace-state-name-list
        '((normal . "N")
          (motion . "M")
          (keypad . "K")
          (insert . "I"))
        )

  ;; (meow-leader-define-key
  ;;  (cons "v"  one-key-menu-roam)
   ;; (cons "p"  (concat doom-leader-alt-key " p"))
   ;; (cons "f"  (concat doom-leader-alt-key " f"))
   ;; (cons "s"  (concat doom-leader-alt-key " s"))
   ;; (cons "o"  (concat doom-leader-alt-key " o"))
   ;; (cons "b"  (concat doom-leader-alt-key " b"))
   ;; (cons "w"  (concat doom-leader-alt-key " w"))
   ;; (cons "q"  (concat doom-leader-alt-key " q"))
   ;; (cons "n"  (concat doom-leader-alt-key " n"))
   ;; (cons "t"  (concat doom-leader-alt-key " t"))
   ;; (cons "i"  (concat doom-leader-alt-key " i"))
   ;; (cons "e"  (concat doom-leader-alt-key " e"))
  ;; )

(defun lewis/define-meow-local-key (key-alist &optional key-prefix)
  "the real order: meow-leader key-prefix key"
  (if key-prefix
      (setq key-prefix (concat key-prefix " "))
    (setq key-prefix ""))
  (dolist (element key-alist)
    (setq key (car element))
    (setq def (cdr element))
    (cond ((stringp key) (meow-leader-define-key
                          (cons (concat key-prefix key) def)))
          ((t (signal 'wrong-type-argument (list 'array key))))
  )))

(defun lewis/define-lazy-key (key-alist filename &optional key-prefix)
  (lewis/define-meow-local-key key-alist key-prefix)
  (dolist (element key-alist)
    (setq fun (cdr element))
    (autoload fun filename nil t))
  )


  ;; mode list
  (add-to-list 'meow-mode-state-list '(bibtex-mode . normal))
  (add-to-list 'meow-mode-state-list '(vterm-mode . normal))
  (add-to-list 'meow-mode-state-list '(comint-mode . normal))
  ;; (add-to-list 'meow-mode-state-list '(vterm-d . motion))
  ;; (add-to-list 'meow-mode-state-list '(bibtex-mode . motion))

(provide 'init-meow)
;;; init-meow.el ends here
