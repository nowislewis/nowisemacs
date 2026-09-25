;;; nowis-gtd.el --- GTD workflow navigation and time logging -*- lexical-binding: t; -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: org, gtd

;;; Commentary:
;; Navigate the existing GTD files without introducing another source of state.
;; Capture always goes to Inbox; classification happens during review.

;;; Code:

(require 'org)
(require 'transient)

(defun nowis/gtd--visit (relative-path)
  "Visit an existing workflow file at RELATIVE-PATH under `org-directory'."
  (let ((file (expand-file-name relative-path org-directory)))
    (unless (file-exists-p file)
      (user-error "GTD file not found: %s" file))
    (find-file file)
    (when (and (string-suffix-p ".org0" file)
               (not (derived-mode-p 'org-mode)))
      (org-mode))))

(defun nowis/gtd-capture-inbox ()
  "Capture a thought directly into the existing Inbox template."
  (interactive)
  (org-capture nil "i"))

(defun nowis/gtd-open-overview ()
  "Open the existing GTD agenda view."
  (interactive)
  (org-agenda nil "g"))

(transient-define-prefix nowis/gtd-workflow-menu ()
  "Navigate GTD by intent; capture first, classify only during review."
  [["Capture"
    ("c" "Capture to Inbox" nowis/gtd-capture-inbox)]
   ["Clarify"
    ("i" "Open Inbox" (lambda () (interactive) (nowis/gtd--visit "gtd/inbox.org")))
    ("o" "Open issues" (lambda () (interactive) (nowis/gtd--visit "node/20260818T204929--当前开放议题.org")))]]
  [["Do"
    ("a" "Next actions" (lambda () (interactive) (nowis/gtd--visit "gtd/action.org")))
    ("g" "GTD agenda" nowis/gtd-open-overview)
    ("t" "Agenda file" (lambda () (interactive) (nowis/gtd--visit "gtd/agenda.org")))]
   ["Reflect"
    ("j" "Today's journal" denote-journal-new-or-existing-entry)]]
  [["Later"
    ("m" "Reading queue" (lambda () (interactive) (nowis/gtd--visit "incremental/20260331T230205--incremental-reading-materials.org")))
    ("b" "Incubate" (lambda () (interactive) (nowis/gtd--visit "gtd/20251201T112101--incubate.org0")))]])

;;;; ---- 快速时间录入 --------------------------------------------------------
;;
;; DSL（minibuffer 输入）：「分钟 [@结束时间] [描述]」
;;   分钟      必填，这段 clock 的时长（start = 结束时间 − 分钟）
;;   @结束时间 可选，形如 @10:30；缺省 = 现在（即“做完就录”）
;;   描述      可选，有描述 → 新建 DONE 一次性事件；无描述 → 只追加 clock
;;
;; 四种组合：
;;   "25 @10:30 看编译器书"  新建 DONE，clock 10:05–10:30
;;   "25 @10:30"             当前 heading 追加 clock 10:05–10:30，不改状态
;;   "25 看编译器书"          新建 DONE，clock 结束=现在
;;   "25"                    当前 heading 追加 clock，结束=现在，不改状态
;;
;; 判定：有描述即视为新任务（CLOCK 行本身不带描述，故可据此区分）。

(defvar nowis/gtd-log-time-target nil
  "新建 DONE 条目的写入位置，在 init 中配置。
nil 表示插到当前光标处。否则为 (FILE . HEADING)：
FILE 为绝对路径（由用户拼好，库不假设目录），
HEADING 为一级标题文本，新条目插到其子树末尾并存盘。
例：(setq nowis/gtd-log-time-target
        (cons (expand-file-name \"action.org\" (nowis/gtd-dir)) \"inbox\"))
纯数字追加不受此影响，始终作用于当前光标 heading。")

(defun nowis/gtd--goto-target ()
  "根据 `nowis/gtd-log-time-target' 定位：切到目标 buffer、移 point 到插入点。"
  (when nowis/gtd-log-time-target
    (let ((file (car nowis/gtd-log-time-target))
          (head (cdr nowis/gtd-log-time-target)))
      (set-buffer (find-file-noselect file))
      (goto-char (point-min))
      (unless (re-search-forward (format "^\\* +%s" (regexp-quote head)) nil t)
        (user-error "在 %s 未找到一级标题「%s」" file head))
      (org-end-of-subtree t t))))

(defun nowis/gtd--new-done (task mins &optional end)
  "在光标处新建一条 DONE（含 CLOSED + CREATED + CLOCK）。
END 为结束时刻（Emacs time），nil 表示现在；CLOSED 也随之对齐。"
  (let* ((endt (or end (current-time)))
         (ts (format-time-string "[%Y-%m-%d %a %H:%M]" endt)))
    (insert (format (concat "** DONE %s\nCLOSED: %s\n"
                            ":PROPERTIES:\n:CREATED:  %s\n:END:\n"
                            ":LOGBOOK:\n%s\n:END:\n")
                    task ts ts (nowis/gtd--clock-line mins end)))))

(defun nowis/gtd--clock-line (mins &optional end)
  "生成一条 CLOCK 行：end=END（nil 则现在），start=end-MINS 分钟。"
  (let* ((now (or end (current-time)))
         (start (time-subtract now (seconds-to-time (* mins 60))))
         (f (lambda (tm) (format-time-string "[%Y-%m-%d %a %H:%M]" tm))))
    (format "CLOCK: %s--%s =>  %d:%02d"
            (funcall f start) (funcall f now) (/ mins 60) (% mins 60))))

(defun nowis/gtd--parse-end (str)
  "把 \"HH:MM\" 解析成今天该时刻的 Emacs time（str 已由上游正则保证格式）。"
  (let ((d (decode-time)))
    (string-match "\\`\\([0-9]+\\):\\([0-9]+\\)\\'" str)
    (encode-time 0 (string-to-number (match-string 2 str))
                 (string-to-number (match-string 1 str))
                 (nth 3 d) (nth 4 d) (nth 5 d))))

(defun nowis/gtd--append-clock (mins &optional end)
  "给光标所在 heading 的 LOGBOOK 顶部追加一条 CLOCK。"
  (save-excursion
    (org-back-to-heading t)
    (let ((clock (nowis/gtd--clock-line mins end))
          (bound (save-excursion (org-end-of-subtree t))))
      (if (re-search-forward "^[ \t]*:LOGBOOK:[ \t]*$" bound t)
          (progn (forward-line 1) (insert clock "\n"))
        (org-end-of-meta-data t)
        (insert ":LOGBOOK:\n" clock "\n:END:\n")))))

;;;###autoload
(defun nowis/gtd-log-time (input)
  "事后补记时间。INPUT 格式「分钟 [@HH:MM 结束时间] [描述]」，详见本节顶部 DSL 说明。"
  (interactive "s记时间（如「25 @10:30 看编译器书」或「25」）: ")
  (if (not (string-match
            "\\`[ \t]*\\([0-9]+\\)[ \t]*\\(?:@\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\)?[ \t]*\\(.*\\)\\'"
            input))
      (user-error "格式应为「分钟 [@结束时间] [描述]」，如「25 @10:30 看编译器书」")
    (let* ((mins (string-to-number (match-string 1 input)))
           (endstr (match-string 2 input))
           (task (string-trim (match-string 3 input)))
           (end (and endstr (nowis/gtd--parse-end endstr))))
      (cond
       ((> (length task) 0)
        (save-window-excursion
          (save-excursion
            (nowis/gtd--goto-target)
            (nowis/gtd--new-done task mins end)
            (when nowis/gtd-log-time-target (save-buffer))))
        (message "已新建 DONE：%s (%dmin%s)" task mins
                 (if end (concat " 结束 " endstr) "")))
       ((org-at-heading-p)
        (nowis/gtd--append-clock mins end)
        (message "已追加 %dmin%s 到当前任务" mins
                 (if end (concat "（结束 " endstr "）") "")))
       (t (user-error "光标不在 heading 上，纯数字无处追加；请补描述新建任务"))))))

(provide 'nowis-gtd)
;;; nowis-gtd.el ends here
