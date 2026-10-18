;;; nowis-pi.el --- Minimal Pi process fleet manager for Ghostel -*- lexical-binding: t; -*-

;; Author: Lewis Liu
;; Keywords: convenience, processes, tools

;;; Commentary:
;;
;; Minimalist, zero-worktree process fleet manager pairing `ghostel', `dtach',
;; and `magit' for Pi.
;;
;; Design:
;; - Single entry point: `M-x nowis-pi-fleet' (`C-c p').
;; - Agents run in-place in their target directory (local or TRAMP remote),
;;   persisted via `dtach' sockets (`~/.local/share/nowis-pi/sockets/').
;; - All conversation and `pi-subagents' interaction happens natively inside
;;   the Ghostel Pi terminal (`SIGWINCH' automatically replays full history on
;;   reattach via `~/.pi/agent/extensions/ghostel-inspector/index.ts').

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)

(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))
(declare-function ghostel-paste-string "ghostel" (string))
(declare-function ghostel-send-key "ghostel" (key-name &optional mods))
(declare-function magit-status-setup-buffer "magit-status" (&optional directory))

(defvar ghostel--process)
(defvar ghostel-exit-functions)
(defvar ghostel-identity)
(defvar ghostel-program-status)
(defvar ghostel-program-status-functions)
(defvar savehist-additional-variables)

(defgroup nowis-pi nil
  "Pi process fleet manager for Ghostel."
  :group 'tools)

(defcustom nowis-pi-command '("pi")
  "Command and default arguments used to launch Pi."
  :type '(repeat string)
  :group 'nowis-pi)

(defcustom nowis-pi-socket-directory "~/.local/share/nowis-pi/sockets"
  "Per-host directory storing `<name>.sock' and `<name>.cwd' for active agents."
  :type 'string
  :group 'nowis-pi)

(defcustom nowis-pi-hosts '(nil)
  "List of hosts watched by `*Pi Fleet*'.
`nil' represents the local machine; remote hosts are TRAMP prefixes such
as \"/rpc:cloud:\" or \"/ssh:dev:\"."
  :type '(repeat (choice (const :tag "Local host" nil)
                         (string :tag "TRAMP prefix (e.g. /rpc:cloud:)")))
  :group 'nowis-pi)

(defcustom nowis-pi-use-dtach t
  "When non-nil and `dtach' is installed, wrap Pi sessions in `dtach'."
  :type 'boolean
  :group 'nowis-pi)


;;;; Socket & Session Discovery (Zero Subprocesses)

(defun nowis-pi--slugify (text)
  "Convert TEXT into a clean session name."
  (let* ((lower (downcase (string-trim text)))
         (clean (replace-regexp-in-string "[^a-z0-9._-]+" "-" lower))
         (trimmed (replace-regexp-in-string "\\`[-.]+\\|[-.]+\\'" "" clean)))
    (if (string-empty-p trimmed)
        (format-time-string "pi-%H%M")
      (truncate-string-to-width trimmed 32 nil nil ""))))

(defun nowis-pi--with-remote (ref-dir path)
  "Attach REF-DIR's TRAMP prefix (if any) to PATH."
  (let ((remote (file-remote-p (or ref-dir default-directory))))
    (if (and remote (not (file-remote-p path)))
        (concat remote path)
      path)))

(defun nowis-pi--host-socket-dir (&optional remote-prefix)
  "Return the socket directory path on REMOTE-PREFIX (nil for local)."
  (let ((default-directory (or remote-prefix "~")))
    (file-name-as-directory
     (expand-file-name (concat (or remote-prefix "") nowis-pi-socket-directory)))))

(defun nowis-pi--socket-file (remote-prefix name)
  "Return the `.sock' path for session NAME on REMOTE-PREFIX."
  (let ((dir (nowis-pi--host-socket-dir remote-prefix)))
    (make-directory dir t)
    (expand-file-name (concat name ".sock") dir)))

(defun nowis-pi--cwd-file (remote-prefix name)
  "Return the `.cwd' metadata file path for session NAME on REMOTE-PREFIX."
  (expand-file-name (concat name ".cwd") (nowis-pi--host-socket-dir remote-prefix)))

(defun nowis-pi--write-cwd (remote-prefix name cwd)
  "Record CWD (as a host-local path) for session NAME on REMOTE-PREFIX."
  (let ((file (nowis-pi--cwd-file remote-prefix name)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (file-local-name (file-name-as-directory (expand-file-name cwd)))
              "\n"))))

(defun nowis-pi--read-cwd (remote-prefix name)
  "Read the recorded working directory for session NAME on REMOTE-PREFIX."
  (let ((file (nowis-pi--cwd-file remote-prefix name)))
    (if (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file nil 0 1024)
          (let ((raw (string-trim (buffer-string))))
            (if (string-empty-p raw)
                (concat (or remote-prefix "") "~/")
              (file-name-as-directory (nowis-pi--with-remote remote-prefix raw)))))
      (concat (or remote-prefix "") "~/"))))

(defun nowis-pi--host-connected-p (host)
  "Return non-nil if HOST (nil for local, or TRAMP prefix) is connected."
  (or (null host)
      (file-remote-p host nil t)))

(defun nowis-pi--all-known-sessions (&optional connect-remotes)
  "Return all active Pi session plists across `nowis-pi-hosts' and live buffers.
When CONNECT-REMOTES is nil, offline TRAMP hosts are skipped."
  (let ((seen (make-hash-table :test #'equal))
        (sessions nil))
    (dolist (buf (nowis-pi--all-task-buffers))
      (with-current-buffer buf
        (let* ((name (alist-get 'task ghostel-identity))
               (cwd (file-name-as-directory default-directory))
               (remote (file-remote-p cwd))
               (host (or remote "local"))
               (key (concat host "::" name))
               (sock (nowis-pi--socket-file remote name)))
          (puthash key t seen)
          (push (list :name name
                      :cwd cwd
                      :remote remote
                      :host host
                      :sock sock
                      :sock-exists (file-exists-p sock)
                      :buffer buf)
                sessions))))
    (dolist (remote nowis-pi-hosts)
      (when (or connect-remotes (nowis-pi--host-connected-p remote))
        (let ((dir (nowis-pi--host-socket-dir remote))
              (host (or remote "local")))
          (when (file-directory-p dir)
            (dolist (file (directory-files dir t directory-files-no-dot-files-regexp))
              (cond
               ((string-suffix-p ".sock" file)
                (let* ((name (file-name-sans-extension (file-name-nondirectory file)))
                       (key (concat host "::" name)))
                  (unless (gethash key seen)
                    (puthash key t seen)
                    (let ((cwd (nowis-pi--read-cwd remote name)))
                      (push (list :name name
                                  :cwd cwd
                                  :remote remote
                                  :host host
                                  :sock file
                                  :sock-exists t
                                  :buffer (nowis-pi--find-task-buffer remote name))
                            sessions)))))
               ((string-suffix-p ".cwd" file)
                (let* ((name (file-name-sans-extension (file-name-nondirectory file)))
                       (sock (expand-file-name (concat name ".sock") dir))
                       (key (concat host "::" name)))
                  (unless (or (gethash key seen) (file-exists-p sock))
                    (ignore-errors (delete-file file)))))))))))
    (sort (nreverse sessions)
          (lambda (a b)
            (string< (concat (plist-get a :host) "/" (plist-get a :name))
                     (concat (plist-get b :host) "/" (plist-get b :name)))))))


;;;; Ghostel Buffer Identity & OSC 7501 Telemetry Layer

(defun nowis-pi--task-identity (remote-prefix name)
  "Build the canonical `ghostel-identity' alist for session NAME on REMOTE-PREFIX."
  `((kind . pi-task)
    (host . ,(or remote-prefix "local"))
    (task . ,name)))

(defun nowis-pi--find-buffer-by-identity (identity)
  "Return the first live `ghostel-mode' buffer whose identity equals IDENTITY."
  (seq-find (lambda (b)
              (and (buffer-live-p b)
                   (with-current-buffer b
                     (and (derived-mode-p 'ghostel-mode)
                          (equal (bound-and-true-p ghostel-identity) identity)))))
            (buffer-list)))

(defun nowis-pi--find-task-buffer (remote-prefix name)
  "Find the live Ghostel buffer for session NAME on REMOTE-PREFIX, if any."
  (nowis-pi--find-buffer-by-identity (nowis-pi--task-identity remote-prefix name)))

(defun nowis-pi--all-task-buffers ()
  "Return all live Ghostel buffers running a `pi-task'."
  (cl-loop for buf in (buffer-list)
           when (and (buffer-live-p buf)
                     (with-current-buffer buf
                       (and (derived-mode-p 'ghostel-mode)
                            (eq (alist-get 'kind ghostel-identity) 'pi-task))))
           collect buf))

(defun nowis-pi--decode-activity (act)
  "Convert an immune `pi-activity' record ACT into a canonical status plist."
  (when act
    (let* ((phase-str (plist-get act :title))
           (phase (and phase-str (intern-soft phase-str)))
           (msg (plist-get act :msg)))
      (when (memq phase '(working blocked done idle))
        (list :state phase
              :app "pi"
              :kind (and (eq phase 'blocked) 'question)
              :msg (and (not (member msg '("Idle" "idle" "working" "done")))
                        msg))))))

(defun nowis-pi--buffer-records (buf)
  "Return `(:root RECORD :subagents ((ID . RECORD) ...) :prompt STR :stats STR)'."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((act (nowis-pi--decode-activity
                   (cdr (assoc "pi-activity" ghostel-program-status))))
             (telem (cdr (assoc "pi-telemetry" ghostel-program-status)))
             (osc-root (cdr (assoc "" ghostel-program-status)))
             (root (cond
                    ((and act (memq (plist-get act :state) '(working blocked)))
                     act)
                    (osc-root osc-root)
                    (act act)
                    ((process-live-p ghostel--process)
                     '(:state idle :app "pi"))
                    (t nil)))
             (subs (cl-loop for (id . rec) in ghostel-program-status
                            when (string-prefix-p "subagents/" id)
                            collect (cons id rec))))
        (list :root root
              :subagents (nreverse subs)
              :prompt (plist-get telem :title)
              :stats (plist-get telem :msg))))))

(defun nowis-pi--state-priority (state)
  "Return numeric urgency priority for OSC 7501 STATE symbol."
  (pcase state
    ('blocked 6)
    ('error 5)
    ('done 4)
    ('working 3)
    ('detached 2)
    ('idle 1)
    (_ 0)))

(defun nowis-pi--effective-status (buf &optional sock-exists)
  "Return the highest-priority status plist for BUF (or SOCK-EXISTS on disk)."
  (cond
   ((not (buffer-live-p buf))
    (if sock-exists '(:state detached) '(:state stopped)))
   ((not (process-live-p (buffer-local-value 'ghostel--process buf)))
    (if sock-exists '(:state detached) '(:state exited)))
   (t
    (let* ((recs (nowis-pi--buffer-records buf))
           (root (plist-get recs :root))
           (subs (mapcar #'cdr (plist-get recs :subagents)))
           (all (if root (cons root subs) subs))
           (best (car (sort (copy-sequence all)
                            (lambda (a b)
                              (> (nowis-pi--state-priority (plist-get a :state))
                                 (nowis-pi--state-priority (plist-get b :state))))))))
      (or best '(:state idle))))))

(defun nowis-pi--state-badge (state &optional kind)
  "Format a colored badge string for STATE and optional KIND."
  (pcase state
    ('blocked  (propertize (format "❓ %s" (or kind "blocked")) 'face 'warning))
    ('error    (propertize "✖ error" 'face 'error))
    ('done     (propertize "✓ done" 'face 'success))
    ('working  (propertize "⚙ working" 'face 'font-lock-constant-face))
    ('detached (propertize "⚡ detached" 'face 'font-lock-type-face))
    ('exited   (propertize "○ exited" 'face 'shadow))
    ('stopped  (propertize "○ stopped" 'face 'shadow))
    (_         (propertize "● idle" 'face 'shadow))))

(defvar nowis-pi--refresh-timer nil
  "Debounce timer coalescing bursty OSC 7501 updates for `*Pi Fleet*'.")

(defun nowis-pi--on-status-change (_id _old _new)
  "Coalesce `*Pi Fleet*' redraws across bursty OSC 7501 events."
  (when (and (get-buffer "*Pi Fleet*") (not (timerp nowis-pi--refresh-timer)))
    (setq nowis-pi--refresh-timer
          (run-with-timer
           0.05 nil
           (lambda ()
             (setq nowis-pi--refresh-timer nil)
             (when-let* ((fleet-buf (get-buffer "*Pi Fleet*")))
               (with-current-buffer fleet-buf
                 (nowis-pi-fleet--refresh-in-place))))))))


;;;; Spawning, Reattaching & Exit Cleanup

(defun nowis-pi--dtach-pids (sock)
  "Return all local system PIDs (master `dtach' and children) owning SOCK."
  (let ((masters nil)
        (children nil))
    (dolist (pid (list-system-processes))
      (let* ((attrs (process-attributes pid))
             (args (alist-get 'args attrs)))
        (when (and args (string-search sock args))
          (push pid masters))))
    (when masters
      (dolist (pid (list-system-processes))
        (let ((ppid (alist-get 'ppid (process-attributes pid))))
          (when (memq ppid masters)
            (push pid children)))))
    (append children masters)))

(defconst nowis-pi--outer-script
  (concat
   "script=\"$1\"; shift; "
   "if command -v zsh >/dev/null 2>&1; then "
   "  exec zsh -ic \"$script\" zsh \"$@\"; "
   "elif command -v bash >/dev/null 2>&1; then "
   "  ZSH_VERSION=1 exec bash -ic \"$script\" bash \"$@\"; "
   "else "
   "  exec sh -c \"$script\" sh \"$@\"; "
   "fi")
  "Outer `sh -c' wrapper that loads interactive `zsh' or `bash' environment.")

(defconst nowis-pi--inner-script
  (concat
   "use_dtach=\"$1\"; sock=\"$2\"; cwd=\"$3\"; shift 3; "
   "cd \"$cwd\" || exit 1; "
   "export PI_PROGRAM_STATUS=1 INSIDE_EMACS=ghostel; "
   "if [ \"$use_dtach\" = 1 ] && command -v dtach >/dev/null 2>&1; then "
   "  if [ -S \"$sock\" ] && ! pgrep -f \"$sock\" >/dev/null 2>&1; then "
   "    rm -f \"$sock\"; "
   "    set -- \"$@\" --continue; "
   "  fi; "
   "  dtach -A \"$sock\" -e \"$(printf '\\035')\" -r winch \"$@\"; "
   "  rc=$?; "
   "  [ ! -e \"$sock\" ] && rm -f \"${sock%.sock}.cwd\"; "
   "  exit $rc; "
   "else "
   "  exec \"$@\"; "
   "fi")
  "Inner script that `cd's into CWD, handles stale sockets, and runs `dtach'.")

(defun nowis-pi--on-buffer-exit (buf _event)
  "Clean up renamed `.sock' / `.cwd' if the underlying `dtach' session exited."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (eq (alist-get 'kind ghostel-identity) 'pi-task)
        (let* ((name (alist-get 'task ghostel-identity))
               (host (alist-get 'host ghostel-identity))
               (remote (unless (equal host "local") host))
               (sock (nowis-pi--socket-file remote name))
               (cwd-file (nowis-pi--cwd-file remote name)))
          (when (and (not remote)
                     (null (nowis-pi--dtach-pids (file-local-name sock))))
            (when (file-exists-p sock) (ignore-errors (delete-file sock)))
            (when (file-exists-p cwd-file) (ignore-errors (delete-file cwd-file)))))
        (nowis-pi--on-status-change "" nil nil)))))

(defun nowis-pi--open-session (name cwd &optional initial-prompt display-mode)
  "Open or spawn Ghostel Pi session NAME in directory CWD.
When INITIAL-PROMPT is non-nil and a new session is created, pass it to Pi.
DISPLAY-MODE is `switch' (default) or `background'."
  (require 'ghostel)
  (nowis-pi-setup)
  (let* ((mode (or display-mode 'switch))
         (full-cwd (file-name-as-directory (expand-file-name cwd)))
         (remote (file-remote-p full-cwd))
         (existing (nowis-pi--find-task-buffer remote name))
         (live-p (and existing
                      (process-live-p (buffer-local-value 'ghostel--process existing))))
         (buf (or existing (get-buffer-create (format "*pi:%s*" name)))))
    (when remote
      (cl-pushnew remote nowis-pi-hosts :test #'equal))
    (unless live-p
      (nowis-pi--write-cwd remote name full-cwd)
      (let* ((identity (nowis-pi--task-identity remote name))
             (sock (nowis-pi--socket-file remote name))
             (local-sock (file-local-name sock))
             (local-cwd (file-local-name full-cwd))
             (pi-argv (append nowis-pi-command
                              (list "--name" name)
                              (and initial-prompt
                                   (not (string-blank-p initial-prompt))
                                   (list initial-prompt))))
             (args (append (list "-c"
                                 nowis-pi--outer-script
                                 "sh"
                                 nowis-pi--inner-script
                                 (if nowis-pi-use-dtach "1" "0")
                                 local-sock
                                 local-cwd)
                           pi-argv)))
        (with-current-buffer buf
          (setq-local default-directory full-cwd))
        (when (eq mode 'switch)
          (pop-to-buffer buf))
        (ghostel-exec buf "sh" args identity)
        (with-current-buffer buf
          (setq-local default-directory full-cwd))))
    (when (eq mode 'switch)
      (pop-to-buffer buf))
    buf))


;;;; Contextual Session Resolution & Lifecycle Commands

(defun nowis-pi--current-session ()
  "Resolve the session plist at point in `*Pi Fleet*', or for current buffer."
  (or (get-text-property (point) 'nowis-pi-session)
      (let* ((sessions (nowis-pi--all-known-sessions))
             (exact-buf
              (and (derived-mode-p 'ghostel-mode)
                   (eq (alist-get 'kind (bound-and-true-p ghostel-identity)) 'pi-task)
                   (let ((name (alist-get 'task ghostel-identity))
                         (host (alist-get 'host ghostel-identity)))
                     (seq-find (lambda (s)
                                 (and (equal (plist-get s :name) name)
                                      (equal (plist-get s :host) host)))
                               sessions))))
             (dir-matches
              (unless exact-buf
                (cl-remove-if-not
                 (lambda (s)
                   (file-in-directory-p default-directory (plist-get s :cwd)))
                 sessions))))
        (cond
         (exact-buf exact-buf)
         ((= (length dir-matches) 1) (car dir-matches))
         ((> (length dir-matches) 1)
          (let* ((names (mapcar (lambda (s) (cons (plist-get s :name) s)) dir-matches))
                 (choice (completing-read "Pi agent in this directory: " names nil t)))
            (alist-get choice names nil nil #'equal)))
         (t (user-error "No Pi session on this line or directory"))))))

(defun nowis-pi--initial-prompt-default ()
  "Extract a default prompt from the active region if present."
  (when (use-region-p)
    (let ((text (string-trim (buffer-substring-no-properties
                              (region-beginning) (region-end)))))
      (if buffer-file-name
          (format "In `%s` (lines %d-%d):\n```\n%s\n```\n"
                  (file-name-nondirectory buffer-file-name)
                  (line-number-at-pos (region-beginning))
                  (line-number-at-pos (region-end))
                  text)
        text))))

(defun nowis-pi--next-free-name (remote base-slug)
  "Return BASE-SLUG if unused on REMOTE, else `BASE-SLUG-2', `BASE-SLUG-3', etc."
  (let ((candidate base-slug)
        (n 2))
    (while (or (nowis-pi--find-task-buffer remote candidate)
               (file-exists-p (nowis-pi--socket-file remote candidate)))
      (setq candidate (format "%s-%d" base-slug n)
            n (1+ n)))
    candidate))

(defun nowis-pi--ssh-host-candidates ()
  "Return TRAMP prefix candidates from `~/.ssh/config'."
  (let ((config (expand-file-name "~/.ssh/config"))
        (hosts nil))
    (when (file-readable-p config)
      (with-temp-buffer
        (insert-file-contents config)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*Host[ \t]+\\(.+\\)$" nil t)
          (dolist (h (split-string (match-string 1) "[ \t]+" t))
            (unless (string-match-p "[*?]" h)
              (cl-pushnew h hosts :test #'equal))))))
    (cl-loop for h in (nreverse hosts)
             append (list (format "/rpc:%s:" h)
                          (format "/ssh:%s:" h)))))

;;;###autoload
(defun nowis-pi-new (&optional remote-host)
  "Prompt for a directory and name, then launch a `dtach + pi' agent session.
With prefix argument REMOTE-HOST, first pick a remote TRAMP host from
`~/.ssh/config'; when invoked on a session row in `*Pi Fleet*', default to
that session's directory (including its remote TRAMP host)."
  (interactive "P")
  (let* ((at-pt (get-text-property (point) 'nowis-pi-session))
         (host-prefix (when remote-host
                        (completing-read "Remote TRAMP host: "
                                         (nowis-pi--ssh-host-candidates) nil nil)))
         (base-dir (cond
                    ((and host-prefix (not (string-empty-p host-prefix)))
                     (let ((h (if (file-remote-p host-prefix)
                                  host-prefix
                                (format "/rpc:%s:" (string-remove-suffix ":" host-prefix)))))
                       (concat h "~/")))
                    (at-pt (plist-get at-pt :cwd))
                    (t (if-let* ((proj (project-current nil default-directory)))
                           (project-root proj)
                         default-directory))))
         (dir (read-directory-name "Run Pi in directory: " base-dir nil t))
         (remote (file-remote-p dir))
         (base-slug (nowis-pi--slugify
                     (string-remove-prefix
                      "." (file-name-nondirectory (directory-file-name dir)))))
         (default-name (nowis-pi--next-free-name remote base-slug))
         (raw-name (read-string (format "Agent name (default %s): " default-name)
                                nil nil default-name))
         (name (nowis-pi--slugify raw-name))
         (prompt (nowis-pi--initial-prompt-default)))
    (nowis-pi--open-session name dir prompt 'switch)))

;;;###autoload
(defun nowis-pi-rename (&optional session new-name)
  "Rename SESSION to NEW-NAME across its socket, buffer, and Pi `/name'."
  (interactive)
  (let* ((s (or session (nowis-pi--current-session)))
         (old-name (plist-get s :name))
         (remote (plist-get s :remote))
         (cwd (plist-get s :cwd))
         (raw-new (or new-name
                      (read-string (format "Rename `%s' to: " old-name) old-name)))
         (clean-new (nowis-pi--slugify raw-new)))
    (when (or (string-empty-p clean-new) (equal clean-new old-name))
      (user-error "Name unchanged"))
    (let ((old-sock (nowis-pi--socket-file remote old-name))
          (new-sock (nowis-pi--socket-file remote clean-new))
          (old-cwd  (nowis-pi--cwd-file remote old-name))
          (new-cwd  (nowis-pi--cwd-file remote clean-new))
          (buf      (nowis-pi--find-task-buffer remote old-name)))
      (when (file-exists-p new-sock)
        (user-error "An agent named `%s' already exists" clean-new))
      (when (file-exists-p old-sock)
        (rename-file old-sock new-sock))
      (if (file-exists-p old-cwd)
          (rename-file old-cwd new-cwd)
        (nowis-pi--write-cwd remote clean-new cwd))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq ghostel-identity (nowis-pi--task-identity remote clean-new))
          (rename-buffer (format "*pi:%s*" clean-new) t)
          (force-mode-line-update t)
          (let ((st (plist-get (nowis-pi--effective-status buf t) :state)))
            (when (memq st '(idle done))
              (ghostel-paste-string (format "/name %s" clean-new))
              (ghostel-send-key "return")))))
      (nowis-pi--on-status-change "" nil nil)
      (message "Renamed agent `%s' -> `%s'" old-name clean-new))))

;;;###autoload
(defun nowis-pi-next-waiting ()
  "Jump to the next Pi agent buffer waiting on user attention."
  (interactive)
  (let* ((candidates
          (cl-loop for buf in (nowis-pi--all-task-buffers)
                   for st = (nowis-pi--effective-status buf t)
                   for prio = (nowis-pi--state-priority (plist-get st :state))
                   when (and (not (eq buf (current-buffer)))
                             (>= prio 4))
                   collect (cons prio buf)))
         (sorted (sort candidates (lambda (a b) (> (car a) (car b)))))
         (target (cdar sorted)))
    (if target
        (pop-to-buffer target)
      (message "No Pi agents currently waiting for attention"))))

;;;###autoload
(defun nowis-pi-review (&optional session)
  "Open Magit status in SESSION's directory."
  (interactive)
  (let ((s (or session (nowis-pi--current-session))))
    (magit-status-setup-buffer (plist-get s :cwd))))

;;;###autoload
(defun nowis-pi-dired (&optional session)
  "Open Dired in SESSION's working directory."
  (interactive)
  (let ((s (or session (nowis-pi--current-session))))
    (dired (plist-get s :cwd))))

;;;###autoload
(defun nowis-pi-kill (&optional session)
  "Terminate SESSION's Pi process, remove its socket, and kill its buffer."
  (interactive)
  (let* ((s (or session (nowis-pi--current-session)))
         (name (plist-get s :name))
         (remote (plist-get s :remote))
         (sock (plist-get s :sock))
         (cwd-file (nowis-pi--cwd-file remote name))
         (buf (nowis-pi--find-task-buffer remote name)))
    (when (yes-or-no-p (format "Kill Pi agent session `%s'? " name))
      (when (buffer-live-p buf)
        (let ((kill-buffer-query-functions nil))
          (when-let* ((proc (buffer-local-value 'ghostel--process buf)))
            (ignore-errors (delete-process proc)))
          (kill-buffer buf)))
      (if remote
          (let ((default-directory (nowis-pi--host-socket-dir remote)))
            (ignore-errors
              (process-file "sh" nil nil nil "-c"
                            (format "pkill -TERM -f %s 2>/dev/null || true"
                                    (shell-quote-argument (file-local-name sock))))))
        (dolist (pid (nowis-pi--dtach-pids (file-local-name sock)))
          (ignore-errors (signal-process (- pid) 'SIGTERM))
          (ignore-errors (signal-process pid 'SIGTERM))))
      (when (file-exists-p sock) (ignore-errors (delete-file sock)))
      (when (file-exists-p cwd-file) (ignore-errors (delete-file cwd-file)))
      (when (derived-mode-p 'nowis-pi-fleet-mode)
        (nowis-pi-fleet--refresh-in-place))
      (message "Killed Pi agent `%s'" name))))


;;;; Fleet Dashboard (`*Pi Fleet*')

(defvar-local nowis-pi-fleet--collapsed nil
  "List of session keys (`host::name') currently folded in `*Pi Fleet*'.")
(put 'nowis-pi-fleet--collapsed 'permanent-local t)

;; Note: Avoid `h/H', `j/J', `k/K', `l/L', `y', `.', `!', `@', and `SPC',
;; which are bound in `meow-motion-state-keymap'.
(defvar-keymap nowis-pi-fleet-mode-map
  :doc "Keymap for `nowis-pi-fleet-mode'."
  :parent special-mode-map
  "RET" #'nowis-pi-fleet-open-at-point
  "TAB" #'nowis-pi-fleet-toggle-section
  "n"   #'nowis-pi-fleet-next-item
  "p"   #'nowis-pi-fleet-prev-item
  "w"   #'nowis-pi-next-waiting
  "a"   #'nowis-pi-fleet-attach-at-point
  "A"   #'nowis-pi-fleet-attach-all
  "c"   #'nowis-pi-new
  "r"   #'nowis-pi-rename
  "R"   #'nowis-pi-fleet-remote
  "d"   #'nowis-pi-dired
  "G"   #'nowis-pi-review
  "D"   #'nowis-pi-kill
  "g"   #'revert-buffer)

(define-derived-mode nowis-pi-fleet-mode special-mode "Pi-Fleet"
  "Major mode for monitoring Pi agent processes."
  :group 'nowis-pi
  (setq-local revert-buffer-function #'nowis-pi-fleet--revert
              truncate-lines t
              header-line-format
              (substitute-command-keys
               "\\<nowis-pi-fleet-mode-map> Pi Fleet: \\[nowis-pi-fleet-open-at-point] open  \\[nowis-pi-fleet-attach-at-point]/\\[nowis-pi-fleet-attach-all] attach/all  \\[nowis-pi-new] new  \\[nowis-pi-rename] rename  \\[nowis-pi-fleet-remote] remote  \\[nowis-pi-dired] dired  \\[nowis-pi-review] magit  \\[nowis-pi-kill] delete  \\[revert-buffer] refresh")))

(defun nowis-pi-fleet--session-at-point ()
  "Return the session plist on the current dashboard line, or signal an error."
  (or (get-text-property (point) 'nowis-pi-session)
      (user-error "No Pi session on this line")))

(defun nowis-pi-fleet-next-item ()
  "Move to the next session row in `*Pi Fleet*'."
  (interactive)
  (let ((pos (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (get-text-property (point) 'nowis-pi-item)))
      (forward-line 1))
    (if (get-text-property (point) 'nowis-pi-item)
        (beginning-of-line)
      (goto-char pos))))

(defun nowis-pi-fleet-prev-item ()
  "Move to the previous session row in `*Pi Fleet*'."
  (interactive)
  (let ((pos (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (get-text-property (point) 'nowis-pi-item)))
      (forward-line -1))
    (if (get-text-property (point) 'nowis-pi-item)
        (beginning-of-line)
      (goto-char pos))))

(defun nowis-pi-fleet-toggle-section ()
  "Fold or unfold the session at point in `*Pi Fleet*'."
  (interactive)
  (let* ((s (nowis-pi-fleet--session-at-point))
         (key (concat (plist-get s :host) "::" (plist-get s :name))))
    (if (member key nowis-pi-fleet--collapsed)
        (setq nowis-pi-fleet--collapsed (delete key nowis-pi-fleet--collapsed))
      (push key nowis-pi-fleet--collapsed))
    (nowis-pi-fleet--refresh-in-place)))

(defun nowis-pi-fleet--render-content (&optional connect-remotes)
  "Render the `*Pi Fleet*' dashboard content into a propertized string."
  (let ((collapsed-set nowis-pi-fleet--collapsed))
    (with-temp-buffer
      (let ((sessions (nowis-pi--all-known-sessions connect-remotes))
            (disconnected (cl-loop for h in nowis-pi-hosts
                                   when (and h (not (nowis-pi--host-connected-p h)))
                                   collect h)))
        (when disconnected
          (dolist (h disconnected)
            (insert (propertize
                     (format "☁ %s (offline — press `R' to connect & scan agents)\n" h)
                     'face 'shadow)))
          (insert "\n"))
        (if (null sessions)
            (insert (propertize "  No active Pi agents. Press `c' to start one, or `R' to connect a remote host.\n"
                                'face 'shadow))
          (dolist (group (seq-group-by (lambda (s) (plist-get s :host)) sessions))
            (pcase-let ((`(,host . ,host-sessions) group))
              (insert (propertize (format "── Host: %s ──\n" host)
                                  'face 'font-lock-keyword-face))
              (dolist (s host-sessions)
                (let* ((name (plist-get s :name))
                       (cwd (plist-get s :cwd))
                       (key (concat host "::" name))
                       (collapsed (member key collapsed-set))
                       (buf (plist-get s :buffer))
                       (eff (nowis-pi--effective-status buf (plist-get s :sock-exists)))
                       (badge (nowis-pi--state-badge (plist-get eff :state)
                                                     (plist-get eff :kind)))
                       (records (nowis-pi--buffer-records buf))
                       (root (plist-get records :root))
                       (subs (plist-get records :subagents))
                       (prompt (plist-get records :prompt))
                       (stats (plist-get records :stats))
                       (r-msg (and root (plist-get root :msg)))
                       (start (point))
                       (head-end nil))
                  (insert (format "%s %-18s %-14s %s%s\n"
                                  (if collapsed "▶" "▼")
                                  (propertize name 'face 'bold)
                                  badge
                                  (propertize (abbreviate-file-name cwd) 'face 'shadow)
                                  (if (and r-msg (not (equal r-msg name)))
                                      (propertize (concat "  — " r-msg) 'face 'font-lock-doc-face)
                                    "")))
                  (setq head-end (point))
                  (put-text-property start head-end 'nowis-pi-item 'session)
                  (put-text-property start head-end 'nowis-pi-key key)
                  (unless collapsed
                    (when (and prompt (not (string-empty-p prompt)))
                      (insert (propertize (format "  │ 💬 %s\n"
                                                  (truncate-string-to-width prompt 84 nil nil "…"))
                                          'face 'font-lock-doc-face)))
                    (when stats
                      (insert (propertize (format "  │ %s\n" stats) 'face 'shadow)))
                    (cl-loop for (sub . rest) on subs
                             for prefix = (if rest "  ├─ " "  └─ ")
                             for id = (car sub)
                             for rec = (cdr sub)
                             do (insert (format "%s%-15s %s — %s\n"
                                                prefix
                                                (propertize (or (plist-get rec :title) id)
                                                            'face 'font-lock-variable-name-face)
                                                (nowis-pi--state-badge (plist-get rec :state)
                                                                       (plist-get rec :kind))
                                                (or (plist-get rec :msg) "")))))
                  (put-text-property start (point) 'nowis-pi-session s)))
              (insert "\n")))))
      (buffer-string))))

(defun nowis-pi-fleet--refresh-in-place (&optional connect-remotes)
  "Redraw `*Pi Fleet*' only when content changed, preserving cursor position."
  (let ((new-content (nowis-pi-fleet--render-content connect-remotes)))
    (unless (equal-including-properties new-content (buffer-string))
      (let* ((inhibit-read-only t)
             (line (line-number-at-pos))
             (col (current-column)))
        (erase-buffer)
        (insert new-content)
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column col)
        (let ((new-pt (point)))
          (dolist (win (get-buffer-window-list (current-buffer) nil t))
            (set-window-point win new-pt)))))))

(defun nowis-pi-fleet--revert (_ignore-auto _noconfirm)
  "Revert handler for `nowis-pi-fleet-mode'."
  (nowis-pi-fleet--refresh-in-place))

;;;###autoload
(defun nowis-pi-fleet ()
  "Open the `*Pi Fleet*' process dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*Pi Fleet*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'nowis-pi-fleet-mode)
        (nowis-pi-fleet-mode))
      (nowis-pi-fleet--refresh-in-place))
    (pop-to-buffer buf)))

(defun nowis-pi-fleet-open-at-point ()
  "Open the Pi terminal at point."
  (interactive)
  (let ((s (nowis-pi-fleet--session-at-point)))
    (nowis-pi--open-session (plist-get s :name)
                            (plist-get s :cwd)
                            nil 'switch)))

(defun nowis-pi-fleet-attach-at-point ()
  "Attach the `⚡ detached' session at point in the background."
  (interactive)
  (let* ((s (nowis-pi-fleet--session-at-point))
         (name (plist-get s :name))
         (buf (plist-get s :buffer)))
    (if (and buf (process-live-p (buffer-local-value 'ghostel--process buf)))
        (message "Agent `%s' is already attached" name)
      (nowis-pi--open-session name (plist-get s :cwd) nil 'background)
      (nowis-pi-fleet--refresh-in-place)
      (message "Attached Pi agent `%s'" name))))

(defun nowis-pi-fleet-attach-all ()
  "Reattach all `⚡ detached' sessions so their live telemetry & status resume."
  (interactive)
  (let ((sessions (nowis-pi--all-known-sessions))
        (count 0))
    (dolist (s sessions)
      (let ((buf (plist-get s :buffer)))
        (when (and (plist-get s :sock-exists)
                   (not (and buf (process-live-p (buffer-local-value 'ghostel--process buf)))))
          (nowis-pi--open-session (plist-get s :name)
                                  (plist-get s :cwd)
                                  nil 'background)
          (cl-incf count))))
    (nowis-pi-fleet--refresh-in-place)
    (message "Reattached %d background Pi agent(s)" count)))

(defun nowis-pi-fleet-remote (tramp-prefix)
  "Connect or add remote TRAMP-PREFIX (from `~/.ssh/config') and scan its agents."
  (interactive
   (let* ((offline (cl-loop for h in nowis-pi-hosts
                            when (and h (not (nowis-pi--host-connected-p h)))
                            collect h))
          (candidates (delete-dups (append offline (nowis-pi--ssh-host-candidates)))))
     (list (completing-read "Connect remote TRAMP host (e.g. /rpc:cloud:): "
                            candidates nil nil (car offline)))))
  (let* ((trimmed (string-trim tramp-prefix))
         (clean (cond
                 ((string-empty-p trimmed) "")
                 ((file-remote-p trimmed) trimmed)
                 (t (format "/rpc:%s:" (string-remove-suffix ":" trimmed))))))
    (unless (and (not (string-empty-p clean)) (file-remote-p clean))
      (user-error "Invalid TRAMP prefix: %s (expected e.g. /rpc:host:)" clean))
    (cl-pushnew clean nowis-pi-hosts :test #'equal)
    (message "Scanning remote host %s..." clean)
    (nowis-pi-fleet--refresh-in-place t)
    (message "Connected %s" clean)))

;;;###autoload
(defun nowis-pi-setup ()
  "Register Ghostel hooks and `savehist' persistence for `nowis-pi'."
  (with-eval-after-load 'savehist
    (cl-pushnew 'nowis-pi-hosts savehist-additional-variables))
  (with-eval-after-load 'ghostel
    (add-hook 'ghostel-program-status-functions #'nowis-pi--on-status-change)
    (add-hook 'ghostel-exit-functions #'nowis-pi--on-buffer-exit)))

(provide 'nowis-pi)
;;; nowis-pi.el ends here
