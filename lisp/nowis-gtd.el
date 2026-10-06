;;; nowis-gtd.el --- GTD workflow navigation and time logging -*- lexical-binding: t; -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: org, gtd

;;; Commentary:
;; Navigate the existing GTD files without introducing another source of state.
;; Capture always goes to Inbox; classification happens during review.

;;; Code:

(require 'org)
(require 'org-agenda)

(defvar nowis/gtd-extra-archive-files nil
  "Additional historical files to review, outside the current archive configuration.")
(declare-function denote-get-path-by-id "denote" (id))
(defvar nowis/gtd-dir)

;;;###autoload
(defun nowis/gtd-open-issues ()
  "Open the issues note by its stable Denote identifier, independent of its title."
  (interactive)
  (require 'denote)
  (let ((file (denote-get-path-by-id "20260818T204929")))
    (unless file
      (user-error "Issues note not found: Denote identifier 20260818T204929"))
    (find-file file)))

;;;###autoload
(defun nowis/gtd-open-agenda ()
  "Open the native Agenda dispatcher using the configured file list."
  (interactive)
  (call-interactively #'org-agenda))

(defun nowis/gtd-focus-task ()
  "Narrow to the selected Org task without changing its state or windows.
From Agenda, visit the original entry first.  Use `widen' to leave focus."
  (interactive)
  (when (derived-mode-p 'org-agenda-mode)
    (org-agenda-switch-to))
  (unless (derived-mode-p 'org-mode)
    (user-error "Select an Org task or an Agenda entry first"))
  (org-back-to-heading t)
  (org-narrow-to-subtree)
  (save-excursion (org-show-subtree))
  (message "Focused on this task; M-x widen to return"))

;;;###autoload
(defun nowis/gtd-resume-task ()
  "Show the original STARTED subtree; ask which one if several are active.
When none is active, show the grouped task Agenda so the user can choose."
  (interactive)
  (let (tasks)
    (org-map-entries
     (lambda ()
       (when (equal (org-get-todo-state) "STARTED")
         (push (cons (format "%s — %s:%d"
                             (org-get-heading t t t t)
                             (file-name-nondirectory (buffer-file-name))
                             (line-number-at-pos))
                     (point-marker))
               tasks)))
     nil 'agenda)
    (if (null tasks)
        (org-agenda nil "g")
      (let ((marker (if (cdr tasks)
                        (cdr (assoc (completing-read "继续哪件任务：" tasks nil t)
                                    tasks))
                      (cdar tasks))))
        (switch-to-buffer (marker-buffer marker))
        (widen)
        (goto-char marker)
        (nowis/gtd-focus-task)))))

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

;; ── 按完整 oldpath 层级浏览近期完成 ──
(defun nowis/review--entry ()
  "当前 DONE 条目 → (路径段列表 . 显示行)，不符合返回 nil。
跳转链接优先用精确的 file+行号+文本搜索，保证归档条目也能点回原处；
id 仅作兜底。"
  (when-let* ((title (org-get-heading t t t t)))
    (let* ((olp (org-entry-get nil "ARCHIVE_OLPATH"))
           (segs (if olp (split-string olp "/" t "[ \t]+") (org-get-outline-path)))
           (segs (or segs (list "其他")))
           (closed (org-entry-get nil "CLOSED"))
           (date (if closed (substring closed 1 11) "Undated"))
           (file (buffer-file-name (buffer-base-buffer)))
           (id (org-id-get))
           ;; 优先 file+标题搜索（datetree 归档也能命中），id 兵底
           (link (cond
                  (file (org-link-make-string
                         (format "file:%s::%s" file (org-link-heading-search-string title))
                         title))
                  (id   (org-link-make-string (concat "id:" id) title))
                  (t    title))))
      (cons segs (format "- [%s] %s" date link)))))

(defvar-local nowis/review-days nil "Date range retained for refreshing this report.")

(defun nowis/review-refresh ()
  "Rebuild the review with its previous date range."
  (interactive)
  (nowis/review-done-by-project nowis/review-days))

;;;###autoload
(defun nowis/review-done-by-project (days)
  "Show clock totals and DONE tasks for DAYS calendar days, including today.
DAYS can be `all' for all history, including undated DONE tasks.
Interactively, a prefix argument selects all history."
  (interactive (list (if current-prefix-arg 'all
                       (read-number "Review the last how many days: "))))
  (unless (or (eq days 'all) (and (integerp days) (> days 0)))
    (user-error "Days must be positive or `all'"))
  (require 'org-clock)
  (let* ((today (org-today))
         (start (unless (eq days 'all)
                  (format-time-string "%Y-%m-%d" (org-time-from-absolute (- today (1- days))))))
         (end (format-time-string "%Y-%m-%d" (org-time-from-absolute (1+ today))))
         (files (delete-dups
                 (append (org-add-archive-files (org-agenda-files t))
                         nowis/gtd-extra-archive-files)))
         (match (if (eq days 'all) "TODO=\"DONE\""
                  (format "TODO=\"DONE\"+CLOSED>=\"<%s>\"+CLOSED<\"<%s>\"" start end)))
         ;; The extensionless archive must be in Org mode for native queries.
         (_ (dolist (file files)
              (with-current-buffer (find-file-noselect file)
                (unless (derived-mode-p 'org-mode) (org-mode)))))
         (items (delq nil (org-map-entries #'nowis/review--entry match files)))
         ;; 按路径排序，使相同前缀相邻 → 能共享标题
         (items (sort items (lambda (a b)
                              (string< (mapconcat #'identity (car a) "/")
                                       (mapconcat #'identity (car b) "/")))))
         (last nil))
    (with-current-buffer (get-buffer-create "*Review by Project*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (setq-local nowis/review-days days)
        (local-set-key (kbd "C-c C-r") #'nowis/review-refresh)
        (insert (if (eq days 'all)
                    "#+title: Task Review — All History\n"
                  (format "#+title: Task Review — Last %d Days\nRange: %s to %s (end exclusive)\n" days start end)))
        (insert "Refresh: C-c C-r\n\n* Time Spent\n")
        (insert (format "#+begin: clocktable :scope %S%s :maxlevel 6 :link t :fileskip0 t\n#+end:\n"
                        files (if start (format " :tstart %S :tend %S" start end) "")))
        (goto-char (point-min))
        (search-forward "#+begin: clocktable")
        (beginning-of-line)
        (org-update-dblock)
        (goto-char (point-max))
        (insert "\n* Completed Tasks\n")
        (unless items (insert "No completed tasks in this range.\n"))
        (dolist (item items)
          (let* ((segs (car item))
                 (common 0))
            ;; 与上一条路径的公共前缀，只为新增层级插入标题
            (while (and (< common (length segs)) (< common (length last))
                        (equal (nth common segs) (nth common last)))
              (setq common (1+ common)))
            (cl-loop for d from common below (length segs) do
                     (insert (make-string (+ 2 d) ?*) " " (nth d segs) "\n"))
            (setq last segs))
          ;; 完成行缩进到路径深度下一级
          (insert (make-string (1+ (length (car item))) ?\s) (cdr item) "\n")))
      (goto-char (point-min))
      (org-fold-show-all)
      (setq buffer-read-only t)
      (pop-to-buffer (current-buffer)))))

(provide 'nowis-gtd)
;;; nowis-gtd.el ends here
