;;; nowis-gtd.el --- AI-assisted GTD analysis powered by gptel -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org "9.6") (gptel "0.9.0"))
;; Keywords: org, gtd, ai

;;; Commentary:
;;
;; Two commands sharing one preset (gtd-assistant):
;;
;;   `nowis/gtd-clarify-entry'
;;      Point on any inbox heading → focused analysis of that entry.
;;
;;   `nowis/gtd-weekly-review'
;;      Full GTD system health check across all files.
;;
;; LLM is analysis-only.  All modifications are done by the user in Emacs.

;;; Code:

(require 'org)
(require 'gptel)
(require 'gptel-context)

;;;; ---- Helpers -------------------------------------------------------------

(defun nowis/gtd--open-chat (buf-name content)
  "Open the gtd-assistant chat buffer, fill with CONTENT, and auto-send."
  (org-agenda-update-agenda-files)
  (let ((buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode) (org-mode))
      (unless gptel-mode (gptel-mode 1))
      (gptel--apply-preset 'gtd-assistant
                           (lambda (sym val)
                             (set (make-local-variable sym) val)))
      (setq-local gptel-context nil)
      (dolist (file org-agenda-files)
        (gptel-context-add-file file))
      (let ((inhibit-read-only t))        (erase-buffer)
           (insert content)))
    (display-buffer buf '(display-buffer-in-side-window
                          (side . right) (window-width . 0.45)))
    (with-current-buffer buf (gptel-send))))

(defun nowis/gtd-current-entry-text ()
  "Return full text of the org entry at point (heading + body, no children)."
  (save-excursion
    (org-back-to-heading t)
    (string-trim
     (buffer-substring-no-properties
      (point)
      (save-excursion (outline-next-heading) (point))))))

;;;; ---- Preset --------------------------------------------------------------

(defconst nowis/gtd--system-prompt
  "你是用户的 GTD 分析助手。用户的 GTD 文件见 context 附件，areas.org 包含完整的 Horizon 框架。

你只做分析和建议，不修改任何文件。所有修改由用户自己在 Emacs 中执行。

分析条目时：
1. 指出表述是否清晰（主语/动词/预期结果是否完整）
2. 给出更简明的改写建议（动词开头，一句话说清做什么、达到什么结果）
3. 判断归属（已有 project / 新建 project / someday / 暂留 inbox）
4. 对照 areas.org Horizon 2，建议合适的 Area tag

报告格式严格使用 org-mode（** 为顶层区块，*** 为子项），禁止 markdown 语法。
每个 *** 条目的分析建议之后加一行：
*用户决定：*
留空供用户填写，用户可继续对话追问细节。"
  "Shared system prompt for gtd-assistant commands.")

(gptel-make-preset 'gtd-assistant
  :description "GTD assistant: analysis only, no file modifications."
  :system nowis/gtd--system-prompt
  :org-convert-response nil)

;;;; ---- Entry points --------------------------------------------------------

;;;###autoload
(defun nowis/gtd-clarify-entry ()
  "Clarify the GTD inbox entry at point with AI assistance."
  (interactive)
  (when (org-before-first-heading-p)
    (user-error "Point is not inside an org heading"))
  (let ((entry (nowis/gtd-current-entry-text)))
    (nowis/gtd--open-chat
     "*gtd-clarify*"
     (concat "# 请分析以下 inbox 条目\n\n" entry "\n\n"))))

;;;###autoload
(defun nowis/gtd-weekly-review ()
  "Run a GTD weekly review with AI assistance."
  (interactive)
  (nowis/gtd--open-chat
   "*gtd-review*"
   (concat "请对我的 GTD 系统做一次完整的健康检查，"
           "输出结构化报告。报告区块：\n\n"
           "** Inbox 清理建议\n"
           "** Project 健康检查\n"
           "** Next Action 质量检查\n"
           "** Someday/Maybe 回顾\n"
           "** 高度检查（对照 areas.org 的 Horizon 框架）\n"
           "** 优先处理\n\n"
           "每个 *** 条目后附 *用户决定：* 留空。\n\n")))

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
