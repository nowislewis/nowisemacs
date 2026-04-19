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

(provide 'nowis-gtd)
;;; nowis-gtd.el ends here
