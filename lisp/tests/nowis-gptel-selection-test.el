;;; nowis-gptel-selection-test.el --- Selection bridge tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'nowis-gptel-selection)

(defmacro nowis-gptel-selection-test--with-buffers (&rest body)
  (declare (indent 0))
  `(let ((source (generate-new-buffer "selection-source"))
         (chat (generate-new-buffer "selection-chat"))
         (transient-mark-mode t)
         (nowis-gptel-selection--pending nil)
         (nowis-gptel-selection--last-signature nil))
     (save-window-excursion
       (unwind-protect
           (progn
             (with-current-buffer source (insert "one\ntwo\nthree\n"))
             (with-current-buffer chat
               (text-mode)
               (setq-local gptel-mode t)
               (insert "Question?"))
             (nowis-gptel-selection-mode 1)
             ,@body)
         (nowis-gptel-selection-mode -1)
         (kill-buffer source)
         (kill-buffer chat)))))

(defun nowis-gptel-selection-test--select (buffer begin end)
  "Select and capture BUFFER's region as the command hooks would."
  (with-current-buffer buffer
    (goto-char end)
    (set-mark begin)
    (setq mark-active t)
    (nowis-gptel-selection--post-command)))

(ert-deftest nowis-gptel-selection-replaces-and-snapshots ()
  (nowis-gptel-selection-test--with-buffers
    (nowis-gptel-selection-test--select source 1 5)
    (should (equal (plist-get nowis-gptel-selection--pending :text) "one\n"))
    (should (equal (plist-get nowis-gptel-selection--pending :lines) '(1 . 1)))
    (nowis-gptel-selection-test--select source 5 8)
    (should (equal (plist-get nowis-gptel-selection--pending :text) "two"))
    (with-current-buffer source
      (setq mark-active nil)
      (goto-char 5)
      (insert "changed ")
      (nowis-gptel-selection--post-command))
    (should (equal (plist-get nowis-gptel-selection--pending :text) "two"))
    (with-temp-buffer
      (insert "other")
      (nowis-gptel-selection-test--select (current-buffer) 1 6))
    (should (equal (plist-get nowis-gptel-selection--pending :text) "other"))))

(ert-deftest nowis-gptel-selection-retains-after-switch-and-ignores-chat-region ()
  (nowis-gptel-selection-test--with-buffers
    (nowis-gptel-selection-test--select source 1 4)
    (with-current-buffer source
      (setq mark-active nil)
      (nowis-gptel-selection--capture))
    (switch-to-buffer chat)
    (nowis-gptel-selection--refresh)
    (should (overlayp nowis-gptel-selection--overlay))
    (should (= (overlay-start nowis-gptel-selection--overlay) (point-max)))
    (should (string-match-p "待发送选区" (overlay-get nowis-gptel-selection--overlay 'after-string)))
    (should (equal (buffer-string) "Question?"))
    (nowis-gptel-selection-test--select chat 1 3)
    (should (equal (plist-get nowis-gptel-selection--pending :text) "one"))
    (goto-char (point-max))
    (insert " More")
    (nowis-gptel-selection--post-command)
    (should (= (overlay-start nowis-gptel-selection--overlay) (point-max)))))

(ert-deftest nowis-gptel-selection-consumes-on-materialization-not-request-success ()
  (nowis-gptel-selection-test--with-buffers
    (nowis-gptel-selection-test--select source 1 4)
    (switch-to-buffer chat)
    (goto-char (point-max))
    (set-mark 1)
    (setq mark-active t)
    (cl-letf (((symbol-function 'gptel-send)
               (lambda (&optional _)
                 (should-not mark-active)
                 (should (= (point) (point-max)))
                 (error "Network failed"))))
      (should-error (nowis-gptel-selection-send)))
    (should-not nowis-gptel-selection--pending)
    (should-not nowis-gptel-selection--overlay)
    (should (string-match-p "Question?.*" (buffer-string)))
    (should (string-match-p "\n> one\n" (buffer-string)))
    (let ((body (buffer-string)))
      (cl-letf (((symbol-function 'gptel-send) (lambda (&optional _))))
        (nowis-gptel-selection-send))
      (should (equal body (buffer-string))))
    (nowis-gptel-selection-test--select source 1 4)
    (should-not nowis-gptel-selection--pending)))

(ert-deftest nowis-gptel-selection-ignore-suppresses-unmodified-region ()
  (nowis-gptel-selection-test--with-buffers
    (nowis-gptel-selection-test--select source 1 4)
    (nowis-gptel-selection-ignore)
    (nowis-gptel-selection-test--select source 1 4)
    (should-not nowis-gptel-selection--pending)
    (nowis-gptel-selection-test--select source 1 5)
    (should nowis-gptel-selection--pending)))

(ert-deftest nowis-gptel-selection-active-source-edit-updates-snapshot ()
  (nowis-gptel-selection-test--with-buffers
    (nowis-gptel-selection-test--select source 1 4)
    (with-current-buffer source
      (goto-char 2)
      (let ((inhibit-modification-hooks t))
        (subst-char-in-region 1 2 ?o ?O))
      (goto-char 4)
      (setq mark-active t)
      (nowis-gptel-selection--capture))
    (should (equal (plist-get nowis-gptel-selection--pending :text) "One"))))

(ert-deftest nowis-gptel-selection-guards-preserve-candidate-and-draft ()
  (nowis-gptel-selection-test--with-buffers
    (nowis-gptel-selection-test--select source 1 4)
    (with-current-buffer chat
      (goto-char (point-max))
      (let ((gptel--fsm-last (gptel-make-fsm :state 'WAIT)))
        (cl-letf (((symbol-function 'gptel-send)
                   (lambda (&optional _) (ert-fail "Must not send"))))
          (should-error (nowis-gptel-selection-send) :type 'user-error)))
      (let ((buffer-read-only t))
        (should-error (nowis-gptel-selection-send) :type 'buffer-read-only))
      (goto-char 1)
      (should-error (nowis-gptel-selection-send) :type 'user-error)
      (save-restriction
        (narrow-to-region 1 3)
        (goto-char (point-max))
        (should-error (nowis-gptel-selection-send) :type 'user-error))
      (should (equal (buffer-string) "Question?"))
      (should nowis-gptel-selection--pending))))

(ert-deftest nowis-gptel-selection-prefix-and-source-send-pass-through ()
  (nowis-gptel-selection-test--with-buffers
    (nowis-gptel-selection-test--select source 1 4)
    (let (args)
      (cl-letf (((symbol-function 'gptel-send) (lambda (&optional arg) (push arg args))))
        (with-current-buffer chat
          (nowis-gptel-selection-send '(4))
          (nowis-gptel-selection-send 0))
        (with-current-buffer source (nowis-gptel-selection-send)))
      (should (equal args '(nil 0 (4)))))
    (should nowis-gptel-selection--pending)
    (with-current-buffer chat (should (equal (buffer-string) "Question?")))))

(ert-deftest nowis-gptel-selection-org-quotes-literal-lines ()
  (with-temp-buffer
    (org-mode)
    (should (equal (nowis-gptel-selection--quoted-text "* heading\n#+end_src\n")
                   ": * heading\n: #+end_src\n: "))))

(ert-deftest nowis-gptel-selection-toggle-restores-bindings-and-cleans-hints ()
  (nowis-gptel-selection-test--with-buffers
    (nowis-gptel-selection-test--select source 1 4)
    (switch-to-buffer chat)
    (nowis-gptel-selection--refresh)
    (should (eq (key-binding (kbd "M-p")) #'nowis-gptel-selection-send))
    (should (eq (key-binding (kbd "C-c RET")) #'nowis-gptel-selection-send))
    (should (overlayp nowis-gptel-selection--overlay))
    (nowis-gptel-selection-mode -1)
    (should-not nowis-gptel-selection--chat-mode)
    (should-not nowis-gptel-selection--overlay)
    (should (eq (key-binding (kbd "C-c RET")) #'gptel-send))
    (should-not (memq #'nowis-gptel-selection--capture pre-command-hook))))

(ert-deftest nowis-gptel-selection-keyboard-buffer-switch ()
  (nowis-gptel-selection-test--with-buffers
    (switch-to-buffer source)
    (goto-char (point-min))
    (execute-kbd-macro (kbd "C-SPC C-n"))
    (should (equal (plist-get nowis-gptel-selection--pending :text) "one\n"))
    (let ((switch (lambda () (interactive) (switch-to-buffer chat))))
      (local-set-key (kbd "<f8>") switch)
      (execute-kbd-macro (kbd "<f8>")))
    (should (eq (current-buffer) chat))
    (should (overlayp nowis-gptel-selection--overlay))))

(ert-deftest nowis-gptel-selection-window-click-path ()
  (nowis-gptel-selection-test--with-buffers
    (switch-to-buffer source)
    (let ((chat-window (split-window-right)))
      (set-window-buffer chat-window chat)
      (goto-char (point-min))
      (execute-kbd-macro (kbd "C-SPC C-n"))
      ;; Mouse-set-point selects a window; redisplay invokes these callbacks.
      (select-window chat-window)
      (run-hook-with-args 'window-selection-change-functions (selected-frame))
      (should (eq (current-buffer) chat))
      (should (overlayp nowis-gptel-selection--overlay))
      (should (equal (plist-get nowis-gptel-selection--pending :text) "one\n")))))

(ert-deftest nowis-gptel-selection-hidden-chat-hint-cleared-on-consumption ()
  (nowis-gptel-selection-test--with-buffers
    (nowis-gptel-selection-test--select source 1 4)
    (with-current-buffer chat (nowis-gptel-selection--render))
    (nowis-gptel-selection-ignore)
    (with-current-buffer chat (should-not nowis-gptel-selection--overlay))))

(ert-deftest nowis-gptel-selection-materialization-is-one-undo-step ()
  (nowis-gptel-selection-test--with-buffers
    (nowis-gptel-selection-test--select source 1 4)
    (with-current-buffer chat
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (goto-char (point-max))
      (cl-letf (((symbol-function 'gptel-send) (lambda (&optional _))))
        (nowis-gptel-selection-send))
      (undo-boundary)
      (undo 1)
      (should (equal (buffer-string) "Question?")))))

(ert-deftest nowis-gptel-selection-native-request-keeps-history-and-attachment ()
  (nowis-gptel-selection-test--with-buffers
    (require 'gptel-openai)
    (nowis-gptel-selection-test--select source 1 4)
    (with-current-buffer chat
      (erase-buffer)
      (insert "Earlier question\n")
      (insert (propertize "Earlier answer\n" 'gptel 'response))
      (insert "Question?")
      (goto-char (point-max))
      (let ((gptel-backend (gptel-make-openai "selection-test"
                            :host "example.invalid" :key "unused"))
            (gptel-model 'gpt-4o-mini)
            (gptel-stream nil)
            (gptel-use-tools nil)
            (gptel-use-context nil)
            (gptel-prompt-transform-functions nil)
            (original-request (symbol-function 'gptel-request))
            fsm)
        ;; Keep native gptel-send and parsing; only suppress network dispatch.
        (cl-letf (((symbol-function 'gptel-request)
                   (lambda (prompt &rest options)
                     (setq fsm (apply original-request prompt
                                      :dry-run t options)))))
          (nowis-gptel-selection-send))
        (let* ((data (plist-get (gptel-fsm-info fsm) :data))
               (messages (plist-get data :messages))
               (printed (prin1-to-string messages)))
          (should (string-match-p "Earlier question" printed))
          (should (string-match-p "Earlier answer" printed))
          (should (string-match-p "Question?" printed))
          (should (string-match-p "> one" printed)))))))

(ert-deftest nowis-gptel-selection-unchanged-commands-do-not-refresh ()
  (nowis-gptel-selection-test--with-buffers
    (nowis-gptel-selection-test--select source 1 4)
    (let ((refreshes 0))
      (cl-letf (((symbol-function 'nowis-gptel-selection--refresh)
                 (lambda (&rest _) (setq refreshes (1+ refreshes)))))
        (with-current-buffer source
          (dotimes (_ 100)
            (nowis-gptel-selection--capture)
            (nowis-gptel-selection--post-command)))
        (should (= refreshes 0))
        (nowis-gptel-selection-test--select source 1 5)
        (should (= refreshes 1))))))

(ert-deftest nowis-gptel-selection-overlay-follows-external-edits-without-hooks ()
  (nowis-gptel-selection-test--with-buffers
    (nowis-gptel-selection-test--select source 1 4)
    (with-current-buffer chat
      (nowis-gptel-selection--render)
      (let ((overlay nowis-gptel-selection--overlay))
        ;; Model streaming appends text outside the command loop.
        (goto-char (point-max))
        (insert " streamed response")
        (should (= (overlay-start overlay) (point-max)))
        (should (= (overlay-end overlay) (point-max)))
        (goto-char (point-min))
        (insert "prefix ")
        (should (= (overlay-start overlay) (point-max)))
        (delete-region (- (point-max) 4) (point-max))
        (should (= (overlay-start overlay) (point-max)))
        (erase-buffer)
        (insert "New draft")
        (should (= (overlay-start overlay) (point-max)))
        (kill-buffer chat)
        (should-not (overlay-buffer overlay))))))

(ert-deftest nowis-gptel-selection-without-candidate-preserves-native-region-send ()
  (nowis-gptel-selection-test--with-buffers
    (with-current-buffer chat
      (goto-char 3)
      (set-mark 1)
      (setq mark-active t)
      (save-restriction
        (narrow-to-region 1 5)
        (let ((buffer-read-only t)
              (gptel--fsm-last (gptel-make-fsm :state 'WAIT))
              called)
          (cl-letf (((symbol-function 'gptel-send)
                     (lambda (&optional arg)
                       (setq called t)
                       (should-not arg)
                       (should mark-active)
                       (should (= (point) 3))
                       (should (buffer-narrowed-p)))))
            (nowis-gptel-selection-send))
          (should called)))
      (should (equal (buffer-string) "Question?")))))

(ert-deftest nowis-gptel-selection-line-ranges-match-native-counting ()
  (nowis-gptel-selection-test--with-buffers
    (with-current-buffer source
      (erase-buffer)
      (insert "首行\n\nthird\nlast")
      ;; Exhaust every nonempty region, including newline-only and EOF ranges.
      (let ((limit (point-max)))
        (dolist (narrowed '(nil t))
          (save-restriction
            (when narrowed (narrow-to-region 3 (1- limit)))
            (let ((begin (point-min)))
              (while (< begin (point-max))
                (let ((end (1+ begin)))
                  (while (<= end (point-max))
                    (let ((expected (cons (line-number-at-pos begin t)
                                          (line-number-at-pos (1- end) t))))
                      (nowis-gptel-selection-test--select source begin end)
                      (should (equal (plist-get nowis-gptel-selection--pending :lines)
                                     expected)))
                    (setq end (1+ end))))
                (setq begin (1+ begin))))))))))

(ert-deftest nowis-gptel-selection-capture-counts-absolute-lines-once ()
  (nowis-gptel-selection-test--with-buffers
    (let ((original (symbol-function 'line-number-at-pos))
          (calls 0))
      (cl-letf (((symbol-function 'line-number-at-pos)
                 (lambda (&rest args)
                   (setq calls (1+ calls))
                   (apply original args))))
        (nowis-gptel-selection-test--select source 1 9))
      (should (= calls 1))
      (should (equal (plist-get nowis-gptel-selection--pending :lines) '(1 . 2))))))

(provide 'nowis-gptel-selection-test)
;;; nowis-gptel-selection-test.el ends here
