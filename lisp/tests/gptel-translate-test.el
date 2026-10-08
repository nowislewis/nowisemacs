;;; gptel-translate-test.el --- Translation tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel-translate)

(defmacro gptel-translate-test--with-buffer (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (unwind-protect (progn ,@body)
       (gptel-translate-clear))))

(defun gptel-translate-test--flush ()
  "Send queued text now instead of waiting for the test's timer."
  (when (timerp gptel-translate--timer)
    (cancel-timer gptel-translate--timer))
  (gptel-translate--flush (current-buffer) gptel-translate--generation))

(ert-deftest gptel-translate-manual-paragraphs-without-log-mode ()
  (gptel-translate-test--with-buffer
    (insert "First line\ncontinued.\n\n \t\nSecond paragraph")
    (gptel-translate-region (point-min) (point-max))
    (should-not gptel-translate-log-mode)
    (should (equal (mapcar #'cdr gptel-translate--queue)
                   '("First line\ncontinued." "Second paragraph")))
    (should (= 2 (length gptel-translate--overlays)))
    (gptel-translate-region (point-min) (point-max))
    (should (= 2 (length gptel-translate--queue)))))

(ert-deftest gptel-translate-prefix-lines-and-exact-boundaries ()
  (gptel-translate-test--with-buffer
    (insert "prefix One\nTwo\nThree suffix")
    (gptel-translate-region 8 21 '(4))
    (should (equal (mapcar #'cdr gptel-translate--queue)
                   '("One" "Two" "Three")))
    (should-not gptel-translate-log-mode)))

(ert-deftest gptel-translate-log-complete-lines-and-manual-history ()
  (gptel-translate-test--with-buffer
    (insert "Old history\n")
    (gptel-translate-log-mode 1)
    (should-not gptel-translate--queue)
    (insert "The door")
    (should-not gptel-translate--queue)
    (insert " opens.\nSecond line\n")
    (should (equal (mapcar #'cdr gptel-translate--queue)
                   '("The door opens." "Second line")))
    (gptel-translate-region (point-min) (point-max))
    (should (= 3 (length gptel-translate--queue)))))

(ert-deftest gptel-translate-preserves-source-and-cleans-up ()
  (gptel-translate-test--with-buffer
    (insert "One\n\nTwo")
    (gptel-translate-region (point-min) (point-max))
    (let ((source (buffer-string)))
      (gptel-translate--finish gptel-translate--queue
                               "2\t二\n1\t一" nil)
      (should (equal source (buffer-string)))
      (should (equal '("\n  二" "\n  一")
                     (mapcar (lambda (ov) (overlay-get ov 'after-string))
                             gptel-translate--overlays)))
      (gptel-translate-clear)
      (should-not (overlays-in (point-min) (point-max)))
      (should (equal source (buffer-string))))))

(ert-deftest gptel-translate-rejects-duplicate-ids-and-allows-retry ()
  (gptel-translate-test--with-buffer
    (insert "One\nTwo\n")
    (gptel-translate-region (point-min) (point-max) t)
    (let ((batch (prog1 gptel-translate--queue
                   (setq gptel-translate--queue nil))))
      (gptel-translate--finish batch "1\t一\n1\t重复\n2\t二" nil))
    (should (= 1 (length gptel-translate--overlays)))
    (gptel-translate-region (point-min) (point-max) t)
    (should (equal '("One") (mapcar #'cdr gptel-translate--queue)))))

(ert-deftest gptel-translate-edit-invalidates-only-affected-entry ()
  (gptel-translate-test--with-buffer
    (insert "One\n\nTwo")
    (gptel-translate-region (point-min) (point-max))
    (let ((batch gptel-translate--queue))
      (goto-char 2)
      (insert "edited")
      (should (= 1 (length gptel-translate--overlays)))
      (should (equal '("Two") (mapcar #'cdr gptel-translate--queue)))
      (gptel-translate--finish batch "1\t一\n2\t二" nil)
      (should (equal "\n  二" (overlay-get
                               (car gptel-translate--overlays) 'after-string))))))

(ert-deftest gptel-translate-serial-requests-and-stale-callback ()
  (let (callbacks prompts)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (prompt &rest args)
                 (should-not gptel-use-tools)
                 (should-not gptel-use-context)
                 (push prompt prompts)
                 (push (plist-get args :callback) callbacks))))
      (gptel-translate-test--with-buffer
        (gptel-translate-log-mode 1)
        (insert "One\n")
        (gptel-translate-test--flush)
        (should gptel-translate--busy)
        (insert "Two\n")
        (should-not gptel-translate--timer)
        (funcall (car callbacks) "1\t一" nil)
        (should gptel-translate--timer)
        (gptel-translate-test--flush)
        (should (equal '("1\t\"Two\"" "1\t\"One\"") prompts))
        (let ((late (car callbacks)))
          (gptel-translate-clear)
          (funcall late "1\t二" nil)
          (should-not gptel-translate--overlays))))))

(ert-deftest gptel-translate-narrowed-region-and-read-only-source ()
  (gptel-translate-test--with-buffer
    (insert "Outside\n\nInside\ncontinued\n\nOutside")
    (narrow-to-region 10 26)
    (setq buffer-read-only t)
    (gptel-translate-region (point-min) (point-max))
    (should (equal '("Inside\ncontinued") (mapcar #'cdr gptel-translate--queue)))
    (gptel-translate--finish gptel-translate--queue "1\t内部" nil)
    (should (= 1 (length gptel-translate--overlays)))))

(ert-deftest gptel-translate-log-clear-continues-with-new-text ()
  (gptel-translate-test--with-buffer
    (gptel-translate-log-mode 1)
    (insert "One\n")
    (gptel-translate-clear)
    (insert "Two\n")
    (should (equal '("Two") (mapcar #'cdr gptel-translate--queue)))
    (gptel-translate-log-mode -1)
    (insert "Three\n")
    (should-not gptel-translate--queue)))

(ert-deftest gptel-translate-paragraph-wire-format-and-late-narrowing ()
  (let (callback prompt)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (text &rest args)
                 (setq prompt text callback (plist-get args :callback)))))
      (gptel-translate-test--with-buffer
        (insert "One\ncontinued\n\nOther")
        (gptel-translate-region 1 14)
        (gptel-translate-test--flush)
        (should (equal prompt "1\t\"One\\ncontinued\""))
        (narrow-to-region 16 (point-max))
        (funcall callback "1\t一段" nil)
        (should-not gptel-translate--busy)
        (should (equal "\n  一段" (overlay-get
                                 (car gptel-translate--overlays) 'after-string)))))))

(provide 'gptel-translate-test)
