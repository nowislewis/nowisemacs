;;; nowis-org-beautify.el --- Toggle mixed-pitch Org typography -*- lexical-binding: t; -*-

;;; Commentary:
;; `nowis-org-beautify-mode' gives the current Org buffer proportional prose,
;; larger headings, and fixed-pitch tables, code, and metadata.  It reuses the
;; configured `variable-pitch' and `fixed-pitch' fonts and the current theme.
;; Only display settings change; disabling removes this mode's face remappings
;; and restores the original line spacing.  Existing buffer-face and text-scale
;; modes are left alone, so their settings survive the toggle.

;;; Code:

(require 'cl-lib)
(require 'face-remap)
(require 'org)
(require 'org-indent)

(defgroup nowis-org-beautify nil
  "Buffer-local reading typography for Org."
  :group 'org-appearance)

(defcustom nowis-org-beautify-line-spacing 0.2
  "Extra line spacing while `nowis-org-beautify-mode' is enabled.
A float is a fraction of the default line height; an integer is pixels."
  :type '(choice (const :tag "No extra spacing" nil) integer float)
  :group 'nowis-org-beautify)

(defconst nowis-org-beautify--fixed-pitch-faces
  '(org-table org-table-header org-block org-block-begin-line org-block-end-line
    org-code org-verbatim org-meta-line org-document-info-keyword
    org-special-keyword org-property-value org-drawer org-checkbox org-formula
    org-indent org-hide)
  "Faces whose spacing must remain predictable in proportional prose.")

(defvar-local nowis-org-beautify--remappings nil
  "Cookies for face remappings owned by this mode.")

(defvar-local nowis-org-beautify--original-line-spacing nil
  "Original line spacing as (WAS-BUFFER-LOCAL . VALUE).")

(defun nowis-org-beautify--remap (face &rest specs)
  "Remap FACE to SPECS and track the cookie for reversible cleanup."
  (when (facep face)
    ;; A unique plist cookie avoids removing an existing symbol-only remap
    ;; (for example the one installed by variable-pitch-mode).
    (push (apply #'face-remap-add-relative face (copy-tree specs))
          nowis-org-beautify--remappings)))

(defun nowis-org-beautify--enable ()
  "Apply typography once, leaving unrelated display modes untouched."
  (unless nowis-org-beautify--remappings
    (setq nowis-org-beautify--original-line-spacing
          (cons (local-variable-p 'line-spacing) line-spacing))
    (setq-local line-spacing nowis-org-beautify-line-spacing)
    (nowis-org-beautify--remap 'default '(:inherit variable-pitch))
    (dolist (face nowis-org-beautify--fixed-pitch-faces)
      ;; Relative height keeps code and tables in step with text-scale-mode,
      ;; even when the configured fixed-pitch face has an absolute size.
      (nowis-org-beautify--remap face '(:inherit fixed-pitch :height 1.0)))
    (cl-loop for level from 1 to 8
             for height in '(1.3 1.2 1.1 1.0 1.0 1.0 1.0 1.0)
             do (nowis-org-beautify--remap
                 (intern (format "org-level-%d" level))
                 `(:inherit variable-pitch :height ,height :weight bold)))
    (dolist (face '(org-document-title org-document-info org-quote org-verse))
      ;; Some themes inherit org-block for quotes; prose should stay proportional.
      (nowis-org-beautify--remap face '(:inherit variable-pitch)))
    (add-hook 'change-major-mode-hook #'nowis-org-beautify--disable nil t)))

(defun nowis-org-beautify--disable ()
  "Remove only this mode's remappings and restore the saved line spacing."
  (when nowis-org-beautify--remappings
    (mapc #'face-remap-remove-relative nowis-org-beautify--remappings)
    (setq nowis-org-beautify--remappings nil)
    (if (car nowis-org-beautify--original-line-spacing)
        (setq-local line-spacing (cdr nowis-org-beautify--original-line-spacing))
      (kill-local-variable 'line-spacing))
    (setq nowis-org-beautify--original-line-spacing nil))
  (remove-hook 'change-major-mode-hook #'nowis-org-beautify--disable t))

;;;###autoload
(define-minor-mode nowis-org-beautify-mode
  "Toggle reading typography in the current Org buffer.
Prose uses `variable-pitch'; tables, code and metadata use `fixed-pitch'.
Headings are enlarged and line spacing is increased.  Disabling restores
previous typography without changing buffer text or other buffers."
  :lighter " OrgPretty"
  (if nowis-org-beautify-mode
      (if (derived-mode-p 'org-mode)
          (nowis-org-beautify--enable)
        (setq nowis-org-beautify-mode nil)
        (user-error "Org beautification is only available in Org buffers"))
    (nowis-org-beautify--disable)))

(provide 'nowis-org-beautify)
;;; nowis-org-beautify.el ends here
