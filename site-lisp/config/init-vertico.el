;;; init-vertico.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Liu Yi
;;
;; Author: Liu Yi <https://github.com/liuyi>
;; Maintainer: Liu Yi <1226228799@qq.com>
;; Created: October 25, 2021
;; Modified: October 25, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/liuyi/init-vertico
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'vertico)
(add-hook 'after-init-hook vertico-mode)


  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t
        ;; completion-in-region-function
        ;; (lambda (&rest args)
        ;;   (apply (if vertico-mode
        ;;              #'consult-completion-in-region
        ;;            #'completion--in-region)
        ;;          args))))
                 )
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  ;; (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  ;; (map! :map vertico-map [backspace] #'vertico-directory-delete-char))

(provide 'init-vertico)
;;; init-vertico.el ends here
