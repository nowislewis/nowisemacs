;; -*- lexical-binding: t; -*-
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

(fido-vertical-mode)
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
(global-set-key [remap list-buffers] #'ibuffer)

(add-to-list 'load-path (expand-file-name "lib/meow" user-emacs-directory))
(require 'meow)
(setq meow-use-keypad-when-execute-kbd nil
      meow-expand-exclude-mode-list nil
      meow-use-clipboard t
      ;; meow-cursor-type-normal 'box
      ;; meow-cursor-type-insert '(bar . 1)
      meow-replace-state-name-list '((normal . "N")
                                     (motion . "M")
                                     (keypad . "K")
                                     (insert . "I")
                                     (beacon . "B"))
      meow-use-enhanced-selection-effect t
      meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
      meow-keypad-start-keys '((?c . ?c)
                               (?x . ?x))
      meow-char-thing-table '((?\( . round)
                              (?\) . round)
                              (?\[ . square)
                              (?\] . square)
                              (?\{ . curly)
                              (?\} . curly)
                              (?\" . string)
                              (?w . symbol)
                              ;; (?w . window)
                              (?b . buffer)
                              (?p . paragraph)
                              (?l . line)
                              (?d . defun)
                              (?s . sentence))
      )
;; motion keys
(meow-motion-define-key '("j" . meow-next)
                        '("J" . meow-next-expand)
                        '("k" . meow-prev)
                        '("K" . meow-prev-expand)
                        '("h" . meow-left)
                        '("H" . meow-left-expand)
                        '("l" . meow-right)
                        '("L" . meow-right-expand)
                        '("v i" . meow-inner-of-thing)
                        '("v a" . meow-bounds-of-thing)
                        '("y" . meow-save)
                        '("<escape>" . ignore)
                        '("." . repeat)
                        '("!" . kmacro-start-macro-or-insert-counter)
                        '("@" . meow-end-or-call-kmacro)
                        )

;; normal keys
(setq wrap-keymap
      (let ((map (make-keymap)))
        (suppress-keymap map)
        (dolist (k '("(" "[" "{" "<" "\"" "*"))
          (define-key map k #'insert-pair))
        map
        ))

(meow-normal-define-key '("0" . meow-expand-0)
                        '("9" . meow-expand-9)
                        '("8" . meow-expand-8)
                        '("7" . meow-expand-7)
                        '("6" . meow-expand-6)
                        '("5" . meow-expand-5)
                        '("4" . meow-expand-4)
                        '("3" . meow-expand-3)
                        '("2" . meow-expand-2)
                        '("1" . meow-expand-1)

                        '("a" . meow-vim-append)
                        ;; '("A" . meow-append-vim)
                        '("b" . meow-back-word)
                        '("B" . meow-back-symbol)
                        '("c c" . meow-change)
                        '("d" . meow-kill)
                        '("e" . meow-next-word)
                        '("E" . meow-next-symbol)
                        '("f" . meow-find)
                        '("g" . g-extra-commands)
                        '("G" . meow-grab)
                        '("h" . meow-left)
                        '("H" . meow-left-expand)
                        '("i" . meow-insert)
                        ;; '("I" . meow-insert-vim)
                        '("j" . meow-next)
                        '("J" . meow-next-expand)
                        '("k" . meow-prev)
                        '("K" . meow-prev-expand)
                        '("l" . meow-right)
                        '("L" . meow-right-expand)
                        '("m" . consult-register-store)
                        '("M" . meow-block)
                        '("n" . meow-search)
                        '("N" . meow-pop-selection);;

                        '("o" . meow-open-below)
                        '("O" . meow-open-above)
                        '("p" . meow-yank)
                        '("P" . meow-yank-pop);;
                        '("q" . meow-quit)
                        '("Q" . consult-goto-line)
                        '("r" . meow-replace)
                        '("R" . meow-swap-grab)
                        '("s" . meow-line)
                        '("S" . meow-kmacro-lines) ;;
                        '("t" . meow-till)
                        '("u" . meow-undo)
                        '("U" . vundo)
                        '("v v" . meow-visit) ;;
                        '("V" . meow-kmacro-matches) ;;
                        '("w" . meow-mark-word)
                        '("W" . meow-mark-symbol)

                        '("x" . meow-delete)
                        '("X" . meow-backward-delete)
                        '("y" . meow-save)
                        ;; '("Y" . meow-sync-save)
                        '("z a" . hs-toggle-hiding)
                        '("z c" . hs-hide-block)
                        '("z o" . hs-show-block)
                        '("z m" . hs-hide-all)
                        '("z r" . hs-show-all)
                        '("z z" . recenter-top-bottom)

                        '("v i" . meow-inner-of-thing)
                        '("v a" . meow-bounds-of-thing)
                        '("v =" . insert-equation)

                        '("-" . negative-argument)
                        '("=" . indent-region)
                        '("(" . backward-sentence)
                        '(")" . forward-sentence)
                        '("{" . backward-paragraph)
                        '("}" . forward-paragraph)
                        '("]" . nowis-graphviz-symbol-with-label)
                        ;; '("]" . (lambda()
                        ;;           (interactive)
                        ;;           (meow-bounds-of-thing ?\")))
                        (cons "\\" wrap-keymap)
                        '(";" . meow-expand-1)
                        ;; '(":" . async-shell-command)
                        '("'" . consult-register-load)
                        '("\"" . consult-register)
                        '("," . meow-reverse)
                        '("." . repeat)

                        '("<escape>" . ignore)
                        '("!" . kmacro-start-macro-or-insert-counter)
                        '("@" . meow-end-or-call-kmacro)
                        '("#" . embark-toggle-highlight)
                        '("^" . meow-join)
                        '("*" . embark-next-symbol)
                        '("/" . isearch-forward))
(defun meow-vim-append ()
  "Like vim, move to the end of selection, switch to INSERT state."
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (if (not (region-active-p))
        (progn
          (when (and meow-use-cursor-position-hack
                     (< (point) (point-max)))
            (forward-char 1))
          (forward-char 1)
          )
      (meow--direction-forward)
      (meow--cancel-selection))
    (meow--switch-state 'insert)))
(meow-global-mode 1)

(defun toggle-between-meow-normal-motion()
  (interactive)
  (if meow-motion-mode (meow-normal-mode) (meow-motion-mode)))
(global-set-key (kbd "M-\\") #'toggle-between-meow-normal-motion)


;; which-key
(setq which-key-idle-delay 0.1)
(which-key-mode)
(electric-pair-mode)

;; isearch
(setq isearch-lazy-count t)
