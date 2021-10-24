;;; init-theme.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Liu Yi
;;
;; Author: Liu Yi <https://github.com/liuyi>
;; Maintainer: Liu Yi <1226228799@qq.com>
;; Created: October 24, 2021
;; Modified: October 24, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/liuyi/init-theme
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(require 'doom-themes)

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;
  ;; (doom-themes-neotree-config)
  ;;
  ;; or for treemacs users
  ;;
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;;
  ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config)

(provide 'init-theme)
;;; init-theme.el ends here
