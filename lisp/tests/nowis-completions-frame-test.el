;;; nowis-completions-frame-test.el --- Completions frame tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'nowis-completions-frame)

(ert-deftest nowis-completions-frame-geometry-is-centered ()
  "The outer frame is centered and takes the configured share of the width."
  (let ((nowis-completions-frame--border-width 1)
        (nowis-completions-frame--width-ratio 0.6))
    (should (equal (nowis-completions-frame--geometry 1000 700 200)
                   '(200 249 598 200)))))

(ert-deftest nowis-completions-frame-geometry-clips-to-parent ()
  "Content taller than the space above the minibuffer stays inside the parent."
  (let ((nowis-completions-frame--border-width 1)
        (nowis-completions-frame--width-ratio 0.6))
    (should (equal (nowis-completions-frame--geometry 500 100 900)
                   '(100 0 298 98)))))

(ert-deftest nowis-completions-frame-routes-only-minibuffer-completions ()
  "Only *Completions* requested from a selected minibuffer is routed."
  (cl-letf (((symbol-function 'window-minibuffer-p) (lambda (&rest _) t)))
    (should (nowis-completions-frame--minibuffer-completions-p
             "*Completions*" nil))
    (should-not (nowis-completions-frame--minibuffer-completions-p
                 "*Help*" nil)))
  (cl-letf (((symbol-function 'window-minibuffer-p) (lambda (&rest _) nil)))
    (should-not (nowis-completions-frame--minibuffer-completions-p
                 "*Completions*" nil))))

(ert-deftest nowis-completions-frame-mode-toggle-is-reversible ()
  "Disabling the mode removes its display rule and exit hook."
  (let ((display-buffer-alist '(("x" (display-buffer-same-window))))
        (minibuffer-exit-hook nil))
    (nowis-completions-frame-mode 1)
    (unwind-protect
        (progn
          (should (member nowis-completions-frame--display-rule
                          display-buffer-alist))
          (should (memq #'nowis-completions-frame--hide minibuffer-exit-hook)))
      (nowis-completions-frame-mode -1))
    (should (equal display-buffer-alist '(("x" (display-buffer-same-window)))))
    (should-not minibuffer-exit-hook)))
