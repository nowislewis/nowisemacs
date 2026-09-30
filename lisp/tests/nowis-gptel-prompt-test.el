;;; nowis-gptel-prompt-test.el --- Shared prompt tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nowis-gptel-prompt)
(require 'gptel-openai)

(ert-deftest nowis-gptel-prompt-compose-and-deduplicate ()
  (let ((nowis-gptel-prompt-default "Base"))
    (dolist (prompt '(nil "" "Base"))
      (should (equal (nowis-gptel-prompt--compose prompt) "Base")))
    (let ((combined (nowis-gptel-prompt--compose "Task")))
      (should (equal combined "Base\n\nTask"))
      (should (equal (nowis-gptel-prompt--compose combined) combined)))
    (should (equal (nowis-gptel-prompt--compose "Baseball")
                   "Base\n\nBaseball"))))

(ert-deftest nowis-gptel-prompt-preserve-template-and-function ()
  (let ((nowis-gptel-prompt-default "Base")
        (template '("Task" "Example question" "Example answer")))
    (should (equal (nowis-gptel-prompt--compose template)
                   '("Base\n\nTask" "Example question" "Example answer")))
    (should (equal template '("Task" "Example question" "Example answer")))
    (should (equal (nowis-gptel-prompt--compose (lambda () template))
                   '("Base\n\nTask" "Example question" "Example answer")))))

(ert-deftest nowis-gptel-prompt-request-integration ()
  (let ((nowis-gptel-prompt-default "Base")
        (gptel-backend (gptel-make-openai "prompt-test" :host "example.invalid" :key "unused"))
        (gptel-model 'gpt-4o-mini)
        (gptel-stream nil)
        (gptel-use-tools nil)
        (gptel-use-context nil)
        (gptel-prompt-transform-functions
         '(gptel--transform-apply-preset nowis-gptel-prompt--transform)))
    (with-temp-buffer
      (setq-local gptel-system-prompt "Task")
      (cl-labels
          ((sent-system
            (&optional transforms)
            (let* ((fsm (if transforms
                            (gptel-request "Question" :dry-run t :stream nil
                                           :transforms transforms)
                          (gptel-request "Question" :dry-run t :stream nil
                                         :transforms gptel-prompt-transform-functions)))
                   (data (plist-get (gptel-fsm-info fsm) :data)))
              (plist-get (aref (plist-get data :messages) 0) :content))))
        (should (equal (sent-system) "Base\n\nTask"))
        (should (equal (sent-system) "Base\n\nTask"))
        (should (equal gptel-system-prompt "Task"))
        (setq-local nowis-gptel-prompt-enabled nil)
        (should (equal (sent-system) "Task"))
        (setq-local nowis-gptel-prompt-enabled t)
        ;; Agent compaction supplies a dedicated transform list like this.
        (should (equal (sent-system '(gptel--transform-add-context)) "Task"))
        ;; Programmatic requests do not opt into transforms automatically.
        (let* ((fsm (gptel-request "Question" :dry-run t :stream nil))
               (data (plist-get (gptel-fsm-info fsm) :data)))
          (should (equal (plist-get (aref (plist-get data :messages) 0) :content)
                         "Task")))))))

(ert-deftest nowis-gptel-prompt-preset-before-composition ()
  (let ((nowis-gptel-prompt-default "Base")
        (gptel-backend (gptel-make-openai "preset-test" :host "example.invalid" :key "unused"))
        (gptel-model 'gpt-4o-mini)
        (gptel-stream nil)
        (gptel-use-tools nil)
        (gptel-use-context nil)
        (gptel-prompt-transform-functions
         '(gptel--transform-apply-preset nowis-gptel-prompt--transform))
        (gptel--known-presets (copy-tree gptel--known-presets)))
    (gptel-make-preset 'prompt-test :system "Agent task")
    (with-temp-buffer
      (setq-local gptel-system-prompt "Original")
      (let* ((fsm (gptel-request "@prompt-test Question" :dry-run t :stream nil
                                 :transforms gptel-prompt-transform-functions))
             (data (plist-get (gptel-fsm-info fsm) :data)))
        (should (equal (plist-get (aref (plist-get data :messages) 0) :content)
                       "Base\n\nAgent task"))
        (should (equal gptel-system-prompt "Original"))))))

;;; nowis-gptel-prompt-test.el ends here
