;;; init-tree-sitter.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Liu Yi
;;
;; Author: Liu Yi <https://github.com/liuyi>
;; Maintainer: Liu Yi <1226228799@qq.com>
;; Created: October 24, 2021
;; Modified: October 24, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/liuyi/init-tree-sitter
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tree-sitter-debug)
(require 'tree-sitter-query)

;;; Code:
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
