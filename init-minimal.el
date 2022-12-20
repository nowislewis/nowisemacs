(add-to-list 'load-path "~/.emacs.d/lib/meow")
(add-to-list 'load-path "~/.emacs.d/lib/embark")
(add-to-list 'load-path "~/.emacs.d/lib/compat")
(add-to-list 'load-path "~/.emacs.d/lib/consult")
(add-to-list 'load-path "~/.emacs.d/lib/vertico")
(add-to-list 'load-path "~/.emacs.d/lib/vundo")
(add-to-list 'load-path "~/.emacs.d/lib/orderless")
(add-to-list 'load-path "~/.emacs.d/lib/symbol-overlay")


(require 'meow)
(setq meow-use-keypad-when-execute-kbd nil
      meow-expand-exclude-mode-list nil
      meow-use-clipboard t
      meow-cursor-type-normal '(bar . 5)
      meow-cursor-type-insert '(bar . 1)
      meow-replace-state-name-list '((normal . "N")
                                     (motion . "M")
                                     (keypad . "K")
                                     (insert . "I")
                                     (beacon . "B"))
      meow-use-enhanced-selection-effect t
      meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
      meow-keypad-start-keys '((?c . ?c)
                               (?x . ?x))
      )
;; motion keys
(meow-motion-overwrite-define-key '("j" . meow-next)
                                  '("k" . meow-prev)
                                  ;; '("h" . meow-left)
                                  ;; '("l" . meow-right)
                                  '("<escape>" . ignore)
                                  '("." . repeat))
;; normal keys
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
                        '("a" . meow-append)
                        '("A" . meow-append-vim)
                        '("b" . meow-back-word)
                        '("B" . meow-back-symbol)
                        '("c" . meow-change)
                        '("d" . meow-kill)
                        '("e" . meow-next-word)
                        '("E" . meow-next-symbol)
                        '("f" . meow-find)
			'("g d" . xref-find-definitions)
			'("g D" . xref-find-references)
			'("g j" . xref-find-apropos)
			'("g i" . eglot-find-implementation)
			'("g a" . eglot-code-actions)
			'("g r" . eglot-rename)
			'("g f" . format-all-region)
			'("g F" . format-all-buffer)
			'("g n" . flymake-goto-next-error)
			'("g l" . consult-flymake)
			'("g m" . consult-mark)
			'("g w" . clue-copy)
			'("g y" . clue-paste)
			'("g p" . citre-peek)
			'("g c" . citre-ace-peek)

			'("G" . meow-grab)
			'("h" . meow-left)
			'("H" . meow-left-expand)
			'("i" . meow-insert)
			'("I" . meow-insert-vim)
			'("j" . meow-next)
			'("J" . meow-next-expand)
			'("k" . meow-prev)
			'("K" . meow-prev-expand)
			'("l" . meow-right)
			'("L" . meow-right-expand)
			'("m" . consult-register-store)
			;; '("M" . meow-mark-symbol)
			'("M" . meow-block)
			'("n" . meow-search)
			'("N" . meow-pop-selection)

			'("o" . meow-open-below)
			'("O" . meow-open-above)
			'("p" . meow-yank)
			'("P" . meow-yank-pop)
			'("q" . meow-quit)
			'("Q" . goto-line)
			'("r" . meow-replace)
			'("R" . meow-swap-grab)
			'("s" . meow-line)
			'("S" . meow-kmacro-lines)
			'("t" . meow-till)
			'("u" . meow-undo)
			'("U" . vundo)
			'("v v" . meow-visit)
			'("V" . meow-kmacro-matches)
			'("w" . meow-mark-word)
			'("W" . meow-mark-symbol)

			'("x" . meow-delete)
			'("X" . meow-backward-delete)
			'("y" . meow-save)
			'("z a" . hs-toggle-hiding)
			'("z c" . hs-hide-block)
			'("z o" . hs-show-block)
			'("z m" . hs-hide-all)
			'("z r" . hs-show-all)
			'("v i" . meow-inner-of-thing)
			'("v a" . meow-bounds-of-thing)
			'("v \'" . insert-quotations)
			'("v \"" . insert-quotes)
			'("v \`" . insert-backquote)
			'("v *" . insert-star)
			'("v (" . insert-parentheses)
			'("v [" . insert-bracket)
			'("v {" . insert-curly)
			'("v =" . insert-equation)

			'("<escape>" . ignore)
			'("!" . meow-start-kmacro-or-insert-counter)
			'("@" . meow-end-or-call-kmacro)
			'("#" . symbol-overlay-put)
			'("^" . meow-join)
			'("*" . symbol-overlay-put)
			'("-" . negative-argument)
			;; '("=" . format-all-region)
			'("=" . indent-region)
			'("[" . meow-beginning-of-thing)
			'("]" . meow-end-of-thing)
			'("\\" . quoted-insert)
			'(";" . meow-expand-1)
			'(":" . async-shell-command)
			'("'" . consult-register-load)
			'("," . meow-reverse)
			'("." . repeat)
			'("/" . consult-line))

(meow-global-mode 1)

(defun meow-append-vim()
  (interactive)
  (progn (meow-line 1)
         (meow-append)))
(defun meow-insert-vim()
  (interactive)
  (progn (meow-join 1)
         (meow-append)))
(require 'transient)
(define-key transient-base-map "<escape>" #'transient-quit-one)


(transient-define-prefix leader-map-for-lewis()
  "Define leader-key map for special functions"
  [["Citre"
    ("ca" "ace-peek" citre-ace-peek)
    ("cj" "jump" citre-jump)
    ("cp" "peek" citre-peek)
    ("cJ" "jump-back" citre-jump-back)
    ("cu" "update-this-tags-file" citre-update-this-tags-file)
    ("cr" "peek-restore" citre-peek-restore)
    ("cs" "peek-save-session" citre-peek-save-session)
    ("cl" "peek-load-session" citre-peek-load-session)]
   ["Imenu"
    ("l" "Imenu list smart toggle" imenu-list-smart-toggle)]
   ;; ("L" "Boxy imenu" boxy-imenu)]
   ;; english help
   ["English helper"
    ("ht" "Toggle corfu english helper" toggle-corfu-english-helper)]
   ;; org-download
   ["Org download"
    ("d" "Screenshot" org-download-screenshot)]
   ["Youdao"
    ("yp" "Translate posframe" my-gts-translate-posframe)
    ("yi" "Translate input" gts-do-translate)]])

(transient-define-prefix leader-map-for-buffer()
  "Define leader-key map for buffer functions"
  [["Buffer"
    ("b" "consult-buffer" consult-buffer) ;; work with C-x b
    ("k" "kill-current-buffer" kill-current-buffer)
    ("l" "meow-last-buffer" meow-last-buffer)
    ("n" "next-buffer" next-buffer)
    ("p" "previous-buffer" previous-buffer)
    ("r" "revert-buffer" revert-buffer)]
   ["Bookmark"
    ("j" "bookmark-jump" bookmark-jump)
    ("m" "bookmark-set" bookmark-set)
    ("M" "bookmark-delete" bookmark-delete)]])

;; default
(meow-leader-define-key
 ;; SPC j/k will run the original command in MOTION state.
 '("j" . "H-j")
 '("k" . "H-k")
 ;; '("h" . "H-h")
 ;; '("l" . "H-l")
 '("." . "H-.")
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

;; buffer
(meow-leader-define-key
 ;; '("b b" . persp-switch-to-buffer)
 '("b" . leader-map-for-buffer))

;; lewisliu
(meow-leader-define-key
 '("e" . leader-map-for-lewis))

;; search
(meow-leader-define-key
 ;; '("s" . leader-map-for-search)
 '("s b" . consult-buffer)
 '("s d" . consult-ripgrep)
 '("s D" . lewis/ripgrep-search-other-dir)
 '("s f" . consult-find)
 '("s F" . lewis/find-file-other-dir)
 '("s g" . rg)
 '("s h" . consult-history)
 '("s i" . consult-imenu)
 '("s l" . consult-keep-lines)
 '("s s" . consult-line)
 '("s S" . consult-ripgrep-one-file)
 )
;; apps
(meow-leader-define-key
 '("a a" . org-agenda)
 '("a c" . org-capture)
 '("a g s" . gif-screencast-start-or-stop)
 '("a g p" . gif-screencast-toggle-pause)
 '("a l" . app-launcher-run-app)
 '("a f" . vterm-toggle-forward)
 '("a b" . vterm-toggle-backward)
 '("a i" . double-vterm-toggle-insert-cd)
 '("a o p" . oj-prepare)
 '("a o t" . oj-test)
 '("a o h" . oj-open-home-dir)
 '("a e" . org-excalidraw-create-drawing)
 '("a E" . org-excalidraw-initialize)
 '("a t" . org-pomodoro)
 )


(defun find-config-file()
  (interactive)
  (find-file nowisemacs-config-file))

;; file
(meow-leader-define-key
 '("f r" . consult-recent-file)
 '("f p" . find-config-file))

;; notes
(meow-leader-define-key
 '("n r f" . project-find-xnotes-dir-files)
 '("n r p" . project-find-papers-dir-files)
 '("n r o" . project-find-orgmode-dir-files)
 '("n r c" . consult-notes)
 '("n r g" . project-find-gtd-dir-files)

 '("n e" . org-noter)
 ;; org-remark
 '("n m m" . org-remark-mark))


(meow-leader-define-key
 ;;w workspace
 '("TAB". leader-map-for-tabspaces))

(require 'compat)

(require 'vertico)

(vertico-mode)
(setq vertico-cycle t)

(require 'consult)
(require 'vundo)

(require 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))


(autoload #'symbol-overlay-put "symbol-overlay" nil t)
(autoload #'eglot-find-implementation "eglot" nil t)

;;
