;;; nowis-pi.el --- Minimal Pi process fleet manager for Ghostel -*- lexical-binding: t; -*-

;; Author: Lewis Liu
;; Keywords: convenience, processes, tools

;;; Commentary:
;;
;; Minimalist, zero-worktree process fleet manager pairing `ghostel', `dtach',
;; and `magit' for Pi (`SPC a p' -> `nowis-pi-fleet').
;;
;; Architecture & Non-Obvious Invariants:
;; 1. Filesystem Socket Truth (`~/.local/share/nowis-pi/sockets/'):
;;    Each session on a host (local or TRAMP remote) is a `<name>.sock' (`dtach'
;;    Unix socket, detach key `C-]' / 0x1d) plus a `<name>.cwd' metadata file
;;    (cwd, initial socket path, and Pi session UUID).  Stale sockets from power
;;    loss are auto-detected (`! dtach -p <sock> </dev/null') and respawned with
;;    `pi --session <uuid>' (or `pi --continue' fallback).
;; 2. Interactive Shell Bootstrap (`nowis-pi--outer-script'):
;;    Spawns via `zsh -ic' (or `ZSH_VERSION=1 bash -ic') on the execution host
;;    so user shell rc exports (`*_KEY', `VIRTUAL_ENV', `PATH') are loaded both
;;    locally and over TRAMP before `dtach' starts `pi'.
;; 3. OSC 7501 Telemetry & OSC 133;A Immunity (`ghostel-inspector/index.ts'):
;;    Pi's `TuiMainScreen' emits `\e]133;A\e\\' on every user message, which
;;    triggers Ghostel's `ghostel-status--drop-running' (wiping `:state working'
;;    records).  The companion Pi extension therefore publishes `pi-activity'
;;    and `pi-telemetry' with OSC `state=idle' (encoding the true phase in
;;    `:title'), and hooks `process.on("SIGWINCH")' to nudge `stdout.columns'
;;    so `dtach -r winch' reattach deterministically replays full history.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)

(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))
(declare-function magit-status-setup-buffer "magit-status" (&optional directory))

(defvar ghostel--process)
(defvar ghostel-exit-functions)
(defvar ghostel-identity)
(defvar ghostel-program-status)
(defvar ghostel-program-status-functions)
(defvar consult-buffer-sources)
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


;;;; Socket & Session Discovery (Zero Subprocesses)

(defun nowis-pi--slugify (text)
  "Convert TEXT into a clean session name."
  (let* ((lower (downcase (string-trim text)))
         (clean (replace-regexp-in-string "[^a-z0-9._-]+" "-" lower))
         (trimmed (replace-regexp-in-string "\\`[-.]+\\|[-.]+\\'" "" clean)))
    (if (string-empty-p trimmed)
        (format-time-string "pi-%H%M")
      (truncate-string-to-width trimmed 32 nil nil ""))))

(defun nowis-pi--normalize-remote (host)
  "Normalize HOST into a TRAMP prefix (defaulting bare hosts to `/rpc:HOST:')."
  (let ((trimmed (string-trim (or host ""))))
    (cond
     ((string-empty-p trimmed) nil)
     ((file-remote-p trimmed) trimmed)
     (t (format "/rpc:%s:" (string-remove-suffix ":" trimmed))))))

(defun nowis-pi--with-remote (ref-dir path)
  "Attach REF-DIR's TRAMP prefix (if any) to PATH."
  (let ((remote (file-remote-p (or ref-dir default-directory))))
    (if (and remote (not (file-remote-p path)))
        (concat remote path)
      path)))

(defun nowis-pi--host-connected-p (host)
  "Return non-nil if HOST (nil for local, or TRAMP prefix) is connected."
  (or (null host)
      (let ((non-essential t))
        (ignore-errors (file-remote-p host nil t)))))

(defun nowis-pi--host-socket-dir (&optional remote-prefix)
  "Return the socket directory path on REMOTE-PREFIX (nil for local).
Never initiates a TRAMP connection when REMOTE-PREFIX is offline."
  (let ((raw (concat (or remote-prefix "") nowis-pi-socket-directory)))
    (file-name-as-directory
     (if (and remote-prefix (not (nowis-pi--host-connected-p remote-prefix)))
         raw
       (let ((non-essential t)
             (default-directory (or remote-prefix "~")))
         (or (ignore-errors (expand-file-name raw)) raw))))))

(defun nowis-pi--socket-file (remote-prefix name)
  "Return the `.sock' path for session NAME on REMOTE-PREFIX."
  (concat (nowis-pi--host-socket-dir remote-prefix) name ".sock"))

(defun nowis-pi--cwd-file (remote-prefix name)
  "Return the `.cwd' metadata file path for session NAME on REMOTE-PREFIX."
  (concat (nowis-pi--host-socket-dir remote-prefix) name ".cwd"))

(defun nowis-pi--read-meta (remote-prefix name)
  "Return `(:cwd CWD :argv-sock SOCK :session-id SID :cmd CMD)' for NAME."
  (let ((file (nowis-pi--cwd-file remote-prefix name))
        (fallback-cwd (concat (or remote-prefix "") "~/"))
        (fallback-sock (file-local-name (nowis-pi--socket-file remote-prefix name)))
        (lines nil))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file nil 0 1024)
        (setq lines (split-string (buffer-string) "\n" nil "[ \t\r]+"))))
    (let ((raw-cwd (car lines))
          (raw-sock (cadr lines))
          (raw-sid (nth 2 lines))
          (raw-cmd (nth 3 lines)))
      (list :cwd (if (or (null raw-cwd) (string-empty-p raw-cwd))
                     fallback-cwd
                   (file-name-as-directory (nowis-pi--with-remote remote-prefix raw-cwd)))
            :argv-sock (if (or (null raw-sock) (string-empty-p raw-sock))
                           fallback-sock
                         raw-sock)
            :session-id (and raw-sid (not (string-empty-p raw-sid)) raw-sid)
            :cmd (and raw-cmd (not (string-empty-p raw-cmd)) raw-cmd)))))

(defvar nowis-pi--disk-cache (make-hash-table :test #'equal)
  "Cached disk session plists per host key (`\"local\"' or TRAMP prefix).")

(defun nowis-pi--cache-put (remote name cwd &optional sock cmd)
  "Record session NAME in `nowis-pi--disk-cache' for REMOTE (nil for local)."
  (let* ((host (or remote "local"))
         (prev (seq-find (lambda (s) (equal (plist-get s :name) name))
                         (gethash host nowis-pi--disk-cache)))
         (entry (list :name name
                      :cwd (file-name-as-directory cwd)
                      :remote remote
                      :host host
                      :sock (or sock (nowis-pi--socket-file remote name))
                      :cmd (or cmd (plist-get prev :cmd))
                      :sock-exists t))
         (cur (cl-remove-if (lambda (s) (equal (plist-get s :name) name))
                            (gethash host nowis-pi--disk-cache))))
    (puthash host (cons entry cur) nowis-pi--disk-cache)))

(defun nowis-pi--cache-remove (remote name)
  "Remove session NAME from `nowis-pi--disk-cache' for REMOTE (nil for local)."
  (let ((host (or remote "local")))
    (puthash host
             (cl-remove-if (lambda (s) (equal (plist-get s :name) name))
                           (gethash host nowis-pi--disk-cache))
             nowis-pi--disk-cache)))

(defun nowis-pi--scan-host-disk (remote)
  "Scan socket directory on REMOTE (nil for local) and cache session plists.
Uses `non-essential' so scanning never triggers a TRAMP reconnect."
  (when (nowis-pi--host-connected-p remote)
    (let* ((non-essential t)
           (dir (nowis-pi--host-socket-dir remote))
           (host (or remote "local"))
           (prev (gethash host nowis-pi--disk-cache))
           (entries nil))
      (condition-case nil
          (progn
            (when (file-directory-p dir)
              (let* ((files (directory-files dir t directory-files-no-dot-files-regexp))
                     (socks (cl-remove-if-not (lambda (f) (string-suffix-p ".sock" f))
                                              files)))
                (dolist (file socks)
                  (let* ((name (file-name-sans-extension (file-name-nondirectory file)))
                         (live-buf (nowis-pi--find-task-buffer remote name))
                         (cached (seq-find (lambda (s) (equal (plist-get s :name) name))
                                           prev))
                         (meta (unless (and (buffer-live-p live-buf) cached)
                                 (nowis-pi--read-meta remote name)))
                         (cwd (cond
                               ((buffer-live-p live-buf)
                                (file-name-as-directory
                                 (buffer-local-value 'default-directory live-buf)))
                               (cached (plist-get cached :cwd))
                               (t (plist-get meta :cwd))))
                         (cmd (or (and (buffer-live-p live-buf)
                                       (alist-get 'cmd (buffer-local-value
                                                        'ghostel-identity live-buf)))
                                  (and cached (plist-get cached :cmd))
                                  (plist-get meta :cmd))))
                    (push (list :name name
                                :cwd cwd
                                :remote remote
                                :host host
                                :sock file
                                :cmd cmd
                                :sock-exists t)
                          entries)))
                (unless remote
                  (dolist (file files)
                    (when (string-suffix-p ".cwd" file)
                      (let* ((name (file-name-sans-extension (file-name-nondirectory file)))
                             (sock (concat dir name ".sock")))
                        (unless (or (member sock socks)
                                    (nowis-pi--find-task-buffer nil name))
                          (ignore-errors (delete-file file)))))))))
            (setq entries (nreverse entries))
            (puthash host entries nowis-pi--disk-cache)
            entries)
        (error prev)))))

(defun nowis-pi--all-known-sessions (&optional scan-mode)
  "Return all active Pi session plists across `nowis-pi-hosts' and live buffers.
SCAN-MODE controls filesystem/network probing:
- nil: use `nowis-pi--disk-cache' (zero disk/TRAMP I/O once local is primed)
- `local': rescan local socket directory, use cache for remote hosts
- t: rescan both local and connected remote hosts."
  (let ((seen (make-hash-table :test #'equal))
        (sessions nil))
    (when (or (memq scan-mode '(local t))
              (null (gethash "local" nowis-pi--disk-cache)))
      (nowis-pi--scan-host-disk nil))
    (when (eq scan-mode t)
      (dolist (remote nowis-pi-hosts)
        (when (and remote (nowis-pi--host-connected-p remote))
          (nowis-pi--scan-host-disk remote))))
    (dolist (buf (nowis-pi--all-task-buffers))
      (with-current-buffer buf
        (let* ((name (alist-get 'task ghostel-identity))
               (cmd (alist-get 'cmd ghostel-identity))
               (cwd (file-name-as-directory default-directory))
               (remote (file-remote-p cwd))
               (host (or remote "local"))
               (key (concat host "::" name))
               (sock (nowis-pi--socket-file remote name))
               (cached (seq-find (lambda (s) (equal (plist-get s :name) name))
                                 (gethash host nowis-pi--disk-cache)))
               (sock-exists
                (cond
                 ((nowis-pi--buffer-attached-p buf) t)
                 ((assoc "pi-exit" ghostel-program-status) nil)
                 (t (and cached t)))))
          (puthash key t seen)
          (push (list :name name
                      :cwd cwd
                      :remote remote
                      :host host
                      :sock sock
                      :cmd (or cmd (plist-get cached :cmd))
                      :sock-exists sock-exists
                      :buffer buf)
                sessions))))
    (dolist (remote nowis-pi-hosts)
      (when (nowis-pi--host-connected-p remote)
        (let* ((host (or remote "local"))
               (disk-entries (gethash host nowis-pi--disk-cache)))
          (dolist (entry disk-entries)
            (let* ((name (plist-get entry :name))
                   (key (concat host "::" name)))
              (unless (gethash key seen)
                (puthash key t seen)
                (push (append entry
                              (list :buffer (nowis-pi--find-task-buffer remote name)))
                      sessions)))))))
    (sort (nreverse sessions)
          (lambda (a b)
            (string< (concat (plist-get a :host) "::"
                             (plist-get a :cwd) "::"
                             (plist-get a :name))
                     (concat (plist-get b :host) "::"
                             (plist-get b :cwd) "::"
                             (plist-get b :name)))))))


;;;; Ghostel Buffer Identity & OSC 7501 Telemetry Layer

(defun nowis-pi--task-identity (remote-prefix name &optional cmd)
  "Build the canonical `ghostel-identity' alist for session NAME on REMOTE-PREFIX."
  `((kind . pi-task)
    (host . ,(or remote-prefix "local"))
    (task . ,name)
    ,@(and cmd `((cmd . ,cmd)))))

(defun nowis-pi--find-task-buffer (remote-prefix name)
  "Find the live Ghostel buffer for session NAME on REMOTE-PREFIX, if any."
  (let ((host (or remote-prefix "local")))
    (seq-find (lambda (b)
                (and (buffer-live-p b)
                     (with-current-buffer b
                       (and (derived-mode-p 'ghostel-mode)
                            (eq (alist-get 'kind ghostel-identity) 'pi-task)
                            (equal (alist-get 'host ghostel-identity) host)
                            (equal (alist-get 'task ghostel-identity) name)))))
              (buffer-list))))

(defun nowis-pi--buffer-attached-p (buf)
  "Return non-nil if BUF is a live Ghostel buffer with a running process."
  (and (buffer-live-p buf)
       (process-live-p (buffer-local-value 'ghostel--process buf))))

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
    (let* ((raw-title (or (plist-get act :title) ""))
           (parts (split-string raw-title "|" t))
           (phase (and (car parts) (intern-soft (car parts))))
           (tag (cadr parts))
           (msg (plist-get act :msg)))
      (when (memq phase '(working blocked done idle))
        (list :state phase
              :app "pi"
              :tag tag
              :kind (and (eq phase 'blocked) 'question)
              :msg (and (not (member msg '("Idle" "idle" "working" "done"
                                           "Finished" "Interrupted")))
                        msg))))))

(defun nowis-pi--buffer-records (buf)
  "Return status and telemetry plist for live Ghostel buffer BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((act (nowis-pi--decode-activity
                   (cdr (assoc "pi-activity" ghostel-program-status))))
             (telem (cdr (assoc "pi-telemetry" ghostel-program-status)))
             (sess (cdr (assoc "pi-session" ghostel-program-status)))
             (osc-root (cdr (assoc "" ghostel-program-status)))
             (root (cond
                    ((and osc-root (eq (plist-get osc-root :state) 'error))
                     osc-root)
                    (act act)
                    (osc-root osc-root)
                    ((process-live-p ghostel--process)
                     '(:state idle :app "pi"))
                    (t nil)))
             (subs (cl-loop for (id . rec) in ghostel-program-status
                            when (string-prefix-p "subagents/" id)
                            collect (cons id rec))))
        (list :root root
              :subagents (nreverse subs)
              :session-id (plist-get sess :title)
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
   ((not (nowis-pi--buffer-attached-p buf))
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

(defun nowis-pi--state-badge (state &optional kind tag)
  "Format a colored badge string for STATE, optional KIND, and turn TAG."
  (let ((base (pcase state
                ('blocked  (propertize (format "❓ %s" (or kind "blocked")) 'face 'warning))
                ('error    (propertize "✖ error" 'face 'error))
                ('done     (propertize "✓ done" 'face 'success))
                ('working  (propertize "⚙ working" 'face 'font-lock-constant-face))
                ('detached (propertize "⚡ detached" 'face 'font-lock-type-face))
                ('exited   (propertize "○ exited" 'face 'shadow))
                ('stopped  (propertize "○ stopped" 'face 'shadow))
                (_         (propertize "● idle" 'face 'default)))))
    (if (and tag (not (string-empty-p tag)))
        (concat base " " (propertize tag 'face (get-text-property 0 'face base)))
      base)))

(defvar nowis-pi--refresh-timer nil
  "Debounce timer coalescing bursty OSC 7501 updates for `*Pi Fleet*'.")

(defun nowis-pi--on-status-change (id _old _new)
  "Update disk cache on exit and coalesce visible `*Pi Fleet*' redraws."
  (when (eq (alist-get 'kind (bound-and-true-p ghostel-identity)) 'pi-task)
    (let ((name (alist-get 'task ghostel-identity))
          (remote (file-remote-p default-directory)))
      (when (and (equal id "pi-exit") name)
        (nowis-pi--cache-remove remote name))))
  (when-let* ((fleet-buf (get-buffer "*Pi Fleet*"))
              ((get-buffer-window fleet-buf t))
              ((not (timerp nowis-pi--refresh-timer))))
    (setq nowis-pi--refresh-timer
          (run-with-timer
           0.1 nil
           (lambda ()
             (unwind-protect
                 (when-let* ((buf (get-buffer "*Pi Fleet*"))
                             ((get-buffer-window buf t)))
                   (with-current-buffer buf
                     (nowis-pi-fleet--refresh-in-place nil)))
               (setq nowis-pi--refresh-timer nil)))))))


;;;; Spawning, Reattaching & Exit Cleanup

(defconst nowis-pi--outer-script
  (concat
   "s=\"$1\";shift;[ -f ~/.profile ]&&. ~/.profile >/dev/null 2>&1;"
   "if command -v zsh >/dev/null 2>&1;then exec zsh -ic \"$s\" zsh \"$@\";"
   "elif command -v bash >/dev/null 2>&1;then ZSH_VERSION=1 exec bash -ic \"$s\" bash \"$@\";"
   "else exec sh -c \"$s\" sh \"$@\";fi")
  "Outer `sh -c' wrapper that loads login/interactive shell environment.")

(defconst nowis-pi--inner-script
  (concat
   "s=\"$1\";d=\"$2\";c=\"$3\";shift 3;cd \"$d\"||exit 1;"
   "m=\"${s%.sock}.cwd\";mkdir -p \"${s%/*}\";"
   "[ -f \"$m\" ]||printf '%s\\n%s\\n\\n%s\\n' \"$d\" \"$s\" \"$c\">\"$m\";"
   "export PI_PROGRAM_STATUS=1 INSIDE_EMACS=ghostel NOWIS_PI_META=\"$m\";"
   "if [ -S \"$s\" ]&&! dtach -p \"$s\"</dev/null >/dev/null 2>&1;then "
   "rm -f \"$s\";"
   "if [ -z \"$c\" ];then u=$(sed -n 3p \"$m\" 2>/dev/null);"
   "if [ -n \"$u\" ];then set -- \"$@\" --session \"$u\";"
   "else set -- \"$@\" --continue;fi;fi;fi;"
   "dtach -A \"$s\" -e \"$(printf '\\035')\" -r winch \"$@\";r=$?;"
   "if ! dtach -p \"$s\"</dev/null >/dev/null 2>&1;then "
   "rm -f \"$s\" \"$m\";"
   "for f in \"${s%/*}\"/*.cwd;do "
   "[ -f \"$f\" ]&&[ \"$(sed -n 2p \"$f\")\" = \"$s\" ]&&rm -f \"$f\" \"${f%.cwd}.sock\";"
   "done;"
   "printf '\\033]7501;state=idle:id=pi-exit:app=pi\\033\\\\';"
   "fi;exit $r")
  "Inner script that `cd's into CWD, handles stale sockets, and runs `dtach'.")

(defun nowis-pi--on-buffer-exit (buf _event)
  "Refresh `*Pi Fleet*' when a `pi-task' Ghostel buffer exits."
  (when (and (buffer-live-p buf)
             (eq (alist-get 'kind (buffer-local-value 'ghostel-identity buf))
                 'pi-task))
    (nowis-pi--on-status-change "" nil nil)))

(defun nowis-pi--open-session (name cwd &optional initial-prompt display-mode extra-args cmd)
  "Open or spawn Ghostel session NAME in directory CWD.
When CMD is non-nil, run CMD inside `dtach' instead of `nowis-pi-command'.
When INITIAL-PROMPT is non-nil and a new Pi session is created, pass it to Pi.
DISPLAY-MODE is `switch' (default) or `background'; EXTRA-ARGS are additional
CLI flags appended to `nowis-pi-command'."
  (require 'ghostel)
  (let* ((mode (or display-mode 'switch))
         (full-cwd (file-name-as-directory (expand-file-name cwd)))
         (remote (file-remote-p full-cwd))
         (host (or remote "local"))
         (existing (nowis-pi--find-task-buffer remote name))
         (live-p (nowis-pi--buffer-attached-p existing))
         (cached (seq-find (lambda (s) (equal (plist-get s :name) name))
                           (gethash host nowis-pi--disk-cache)))
         (resolved-cmd (or cmd
                           (and (buffer-live-p existing)
                                (alist-get 'cmd (buffer-local-value
                                                 'ghostel-identity existing)))
                           (and cached (plist-get cached :cmd))
                           (and (not remote)
                                (plist-get (nowis-pi--read-meta nil name) :cmd))))
         (buf nil))
    (unless (or live-p remote)
      (unless (executable-find "dtach")
        (user-error "`dtach' is not installed on local host")))
    (setq buf (or existing (get-buffer-create (format "*pi:%s*" name))))
    (when remote
      (cl-pushnew remote nowis-pi-hosts :test #'equal))
    (nowis-pi--cache-put remote name full-cwd nil resolved-cmd)
    (unless live-p
      (let* ((identity (nowis-pi--task-identity remote name resolved-cmd))
             (sock (nowis-pi--socket-file remote name))
             (local-sock (file-local-name sock))
             (local-cwd (file-local-name full-cwd))
             (exec-argv (if resolved-cmd
                            (list "sh" "-c" (concat "exec " resolved-cmd))
                          (append nowis-pi-command
                                  (list "--name" name)
                                  extra-args
                                  (and initial-prompt
                                       (not (string-blank-p initial-prompt))
                                       (list initial-prompt)))))
             (args (append (list "-c"
                                 nowis-pi--outer-script
                                 "sh"
                                 nowis-pi--inner-script
                                 local-sock
                                 local-cwd
                                 (or resolved-cmd ""))
                           exec-argv)))
        (with-current-buffer buf
          (setq-local default-directory full-cwd))
        (ghostel-exec buf "sh" args identity)
        (with-current-buffer buf
          (setq-local default-directory full-cwd))))
    (when (eq mode 'switch)
      (pop-to-buffer buf))
    buf))


;;;; Contextual Session Resolution & Lifecycle Commands

(defun nowis-pi--dir-in-session-p (dir session)
  "Return non-nil if DIR is inside SESSION's `:cwd' without remote TRAMP I/O."
  (let ((remote (file-remote-p dir))
        (sess-remote (plist-get session :remote))
        (sess-cwd (plist-get session :cwd)))
    (and (equal remote sess-remote)
         (if remote
             (let ((non-essential t))
               (string-prefix-p (file-name-as-directory sess-cwd)
                                (file-name-as-directory dir)))
           (file-in-directory-p dir sess-cwd)))))

(defun nowis-pi--current-session ()
  "Resolve the session plist at point in `*Pi Fleet*', or for current buffer."
  (or (get-text-property (point) 'nowis-pi-session)
      (let* ((sessions (nowis-pi--all-known-sessions 'local))
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
                   (nowis-pi--dir-in-session-p default-directory s))
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
        (n 2)
        (cached (gethash (or remote "local") nowis-pi--disk-cache)))
    (while (or (nowis-pi--find-task-buffer remote candidate)
               (if remote
                   (seq-some (lambda (s) (equal (plist-get s :name) candidate))
                             cached)
                 (file-exists-p (nowis-pi--socket-file nil candidate))))
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

(defun nowis-pi--default-shell ()
  "Return the default shell command for command sessions."
  "bash")

(defun nowis-pi--parse-new-input (raw-input default-name)
  "Parse RAW-INPUT into `(:name NAME :args ARGS :cmd CMD)' with DEFAULT-NAME.
Leading `-flag' tokens keep DEFAULT-NAME and treat all tokens as ARGS; a `!'
delimiter (`[name] ! [cmd]') treats the remainder as a shell/CLI command
\(defaulting empty `cmd' to `nowis-pi--default-shell')."
  (let ((trimmed (string-trim (or raw-input ""))))
    (if (string-match
         "\\`\\(?:\\([^ \t!-][^ \t]*\\)[ \t]+\\)?![ \t]*\\(.*\\)\\'"
         trimmed)
        (let ((raw-name (match-string 1 trimmed))
              (raw-cmd (string-trim (match-string 2 trimmed))))
          (list :name (nowis-pi--slugify (or raw-name default-name))
                :args nil
                :cmd (if (string-empty-p raw-cmd)
                         (nowis-pi--default-shell)
                       raw-cmd)))
      (let* ((tokens (ignore-errors (split-string-shell-command trimmed)))
             (flags-only (and tokens (string-prefix-p "-" (car tokens))))
             (name (nowis-pi--slugify (if (or (null tokens) flags-only)
                                          default-name
                                        (car tokens))))
             (extra-args (if flags-only tokens (cdr tokens))))
        (list :name name :args extra-args :cmd nil)))))

(defun nowis-pi--read-new-spec (default-name &optional initial-input)
  "Read `[name] [-args | ! cmd]' in the minibuffer with live preview."
  (let ((raw
         (minibuffer-with-setup-hook
             (lambda ()
               (let* ((pos (- (minibuffer-prompt-end) 2))
                      (ov (make-overlay pos pos))
                      (update
                       (lambda (&rest _)
                         (let* ((spec (nowis-pi--parse-new-input
                                       (minibuffer-contents-no-properties)
                                       default-name))
                                (name (plist-get spec :name))
                                (args (plist-get spec :args))
                                (cmd (plist-get spec :cmd)))
                           (overlay-put
                            ov 'before-string
                            (cond
                             (cmd (format " (name: %s, cmd: %s)" name cmd))
                             (args (format " (name: %s, args: %s)"
                                           name (string-join args " ")))
                             (t (format " (name: %s)" name))))))))
                 (funcall update)
                 (add-hook 'after-change-functions update nil t)))
           (read-string "Session [name] [-args | ! cmd]: "
                        initial-input nil default-name))))
    (nowis-pi--parse-new-input raw default-name)))

(defun nowis-pi--prompt-target-dir (remote-host prompt)
  "Read target directory with PROMPT, honoring REMOTE-HOST prefix or point."
  (let* ((at-pt (get-text-property (point) 'nowis-pi-session))
         (host-at-pt (get-text-property (point) 'nowis-pi-host))
         (host-prefix (cond
                       (remote-host
                        (nowis-pi--normalize-remote
                         (completing-read "Remote TRAMP host: "
                                          (nowis-pi--ssh-host-candidates) nil nil)))
                       ((and (null at-pt) host-at-pt)
                        host-at-pt)))
         (base-dir (cond
                    (host-prefix (concat host-prefix "~/"))
                    (at-pt (plist-get at-pt :cwd))
                    (t (if-let* ((proj (project-current nil default-directory)))
                           (project-root proj)
                         default-directory)))))
    (read-directory-name prompt base-dir nil t)))

;;;###autoload
(defun nowis-pi-new (&optional remote-host initial-input)
  "Prompt for a directory and spec, then launch a `dtach' Pi or command session.
With prefix argument REMOTE-HOST, first pick a remote TRAMP host from
`~/.ssh/config'; when invoked on a session row in `*Pi Fleet*', default to
that session's directory.  Prefix input with `! [cmd]' to run a shell or
arbitrary command instead of Pi (or use `\\[nowis-pi-run]')."
  (interactive "P")
  (let* ((dir (nowis-pi--prompt-target-dir remote-host "Run in directory: "))
         (remote (file-remote-p dir))
         (base-slug (nowis-pi--slugify
                     (string-remove-prefix
                      "." (file-name-nondirectory (directory-file-name dir)))))
         (default-name (nowis-pi--next-free-name remote base-slug))
         (spec (nowis-pi--read-new-spec default-name initial-input))
         (prompt (nowis-pi--initial-prompt-default))
         (new-cmd (plist-get spec :cmd))
         (new-name (plist-get spec :name)))
    (when (and new-cmd (not (plist-get spec :args)))
      (nowis-pi--cache-put remote new-name dir nil new-cmd))
    (nowis-pi--open-session new-name dir prompt 'switch
                            (plist-get spec :args) new-cmd)))

;;;###autoload
(defun nowis-pi-run (&optional remote-host)
  "Launch an interactive shell (`zsh'/`bash') or arbitrary CLI command in `dtach'."
  (interactive "P")
  (nowis-pi-new remote-host "! "))

;;;###autoload
(defun nowis-pi-fork (&optional session)
  "Fork SESSION into a new independent `dtach + pi' agent session."
  (interactive)
  (let* ((s (or session (nowis-pi--current-session)))
         (parent-name (plist-get s :name))
         (remote (plist-get s :remote))
         (cwd (plist-get s :cwd))
         (buf (nowis-pi--find-task-buffer remote parent-name))
         (session-id (or (and buf (plist-get (nowis-pi--buffer-records buf) :session-id))
                         (plist-get (nowis-pi--read-meta remote parent-name) :session-id))))
    (unless (and session-id (not (string-empty-p session-id)))
      (user-error "Agent `%s' has no saved conversation to fork yet" parent-name))
    (let* ((default-name (nowis-pi--next-free-name remote (concat parent-name "-fork")))
           (spec (nowis-pi--read-new-spec default-name))
           (prompt (nowis-pi--initial-prompt-default)))
      (nowis-pi--open-session (plist-get spec :name) cwd prompt 'switch
                              (append (list "--fork" session-id)
                                      (plist-get spec :args))))))

;;;###autoload
(defun nowis-pi-rename (&optional session new-name)
  "Rename SESSION to NEW-NAME across its socket and Ghostel buffer."
  (interactive)
  (let* ((s (or session (nowis-pi--current-session)))
         (old-name (plist-get s :name))
         (remote (plist-get s :remote))
         (cwd (plist-get s :cwd))
         (cmd (plist-get s :cmd))
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
      (when (file-exists-p old-cwd)
        (rename-file old-cwd new-cwd))
      (nowis-pi--cache-remove remote old-name)
      (nowis-pi--cache-put remote clean-new cwd new-sock cmd)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq ghostel-identity (nowis-pi--task-identity remote clean-new cmd))
          (rename-buffer (format "*pi:%s*" clean-new) t)
          (force-mode-line-update t)))
      (nowis-pi--on-status-change "" nil nil)
      (message "Renamed agent `%s' -> `%s'" old-name clean-new))))

(defun nowis-pi--session-detail (s root)
  "Return the human-readable activity or command summary for session S and ROOT."
  (let ((r-msg (and root (plist-get root :msg)))
        (cmd (plist-get s :cmd))
        (name (plist-get s :name)))
    (cond
     ((and r-msg (not (equal r-msg name))) r-msg)
     (cmd (format "$ %s" cmd))
     (t nil))))

(defun nowis-pi--session-headline (s &optional show-detail local-cwd)
  "Format the single-line summary for session S.
Include detail if SHOW-DETAIL is non-nil; strip remote prefix if LOCAL-CWD."
  (let* ((name (plist-get s :name))
         (raw-cwd (plist-get s :cwd))
         (abbr-cwd (if (and (plist-get s :remote)
                            (not (nowis-pi--host-connected-p (plist-get s :remote))))
                       raw-cwd
                     (let ((non-essential t))
                       (or (ignore-errors (abbreviate-file-name raw-cwd)) raw-cwd))))
         (cwd (if local-cwd (file-local-name abbr-cwd) abbr-cwd))
         (buf (plist-get s :buffer))
         (eff (nowis-pi--effective-status buf (plist-get s :sock-exists)))
         (records (nowis-pi--buffer-records buf))
         (root (plist-get records :root))
         (badge (nowis-pi--state-badge (plist-get eff :state)
                                       (plist-get eff :kind)
                                       (and root (plist-get root :tag))))
         (detail (nowis-pi--session-detail s root)))
    (format "%-18s %-20s %s%s"
            (propertize name 'face 'bold)
            badge
            cwd
            (if (and show-detail detail)
                (propertize (concat "  — " detail) 'face 'font-lock-doc-face)
              ""))))

(defvar nowis-pi-consult-source
  `(:name     "Pi Agent"
    :narrow   ?p
    :category nowis-pi-session
    :face     font-lock-keyword-face
    :action   ,(lambda (cand)
                 (when-let* ((s (get-text-property 0 'nowis-pi-session cand)))
                   (nowis-pi--open-session (plist-get s :name)
                                           (plist-get s :cwd))))
    :items    ,(lambda ()
                 (mapcar (lambda (s)
                           (propertize (nowis-pi--session-headline s t)
                                       'nowis-pi-session s))
                         (nowis-pi--all-known-sessions 'local))))
  "`consult-buffer' source for active and detached Pi agent sessions.")

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

(defun nowis-pi--terminate-session (s)
  "Kill session S's buffer, dtach/Pi processes and `.sock'/`.cwd' files."
  (let* ((name (plist-get s :name))
         (remote (plist-get s :remote))
         (argv-sock (plist-get (nowis-pi--read-meta remote name) :argv-sock))
         (local-sock (file-local-name (plist-get s :sock)))
         ;; `[/]...' still matches the path but not this `sh -c' command
         ;; line itself, so `pkill -f' cannot kill its own cleanup shell.
         (pattern (lambda (path)
                    (shell-quote-argument (concat "[/]" (substring path 1)))))
         (buf (nowis-pi--find-task-buffer remote name))
         (files (ignore-errors
                  (directory-files
                   (nowis-pi--host-socket-dir remote) nil
                   (format "\\`%s\\.cwd\\'" (regexp-quote name))))))
    (when (buffer-live-p buf)
      (let ((kill-buffer-query-functions nil))
        (when-let* ((proc (buffer-local-value 'ghostel--process buf)))
          (ignore-errors (delete-process proc)))
        (kill-buffer buf)))
    ;; KILL (not TERM) skips dtach's atexit socket unlink, so the dying
    ;; master can never delete a respawned session's socket of the same name;
    ;; Pi still exits via SIGHUP when the master's pty closes.
    (let ((default-directory (nowis-pi--host-socket-dir remote)))
      (ignore-errors
        (process-file
         "sh" nil nil nil "-c"
         (format "pkill -KILL -f %s; pkill -KILL -f %s; rm -f %s %s"
                 (funcall pattern argv-sock) (funcall pattern local-sock)
                 (shell-quote-argument (concat name ".sock"))
                 (shell-quote-argument (concat name ".cwd"))))))
    ;; Exited buffers leave no socket; reap their orphan metadata immediately.
    (unless (or files (plist-get s :sock-exists))
      (ignore-errors (delete-file (nowis-pi--cwd-file remote name))))
    (nowis-pi--cache-remove remote name)))

;;;###autoload
(defun nowis-pi-restart (&optional session)
  "Kill SESSION and start a fresh session in its directory, like `nowis-pi-new'.
The `[name] [-args | ! cmd]' prompt defaults to the old name (or `! cmd' for
command sessions); the new buffer replaces the old buffer in its window."
  (interactive)
  (let* ((s (or session (nowis-pi--current-session)))
         (old (plist-get s :name))
         (old-cmd (plist-get s :cmd))
         (remote (plist-get s :remote))
         (spec (nowis-pi--read-new-spec
                old (and old-cmd (format "%s ! %s" old old-cmd))))
         (new-name (plist-get spec :name))
         (prompt (nowis-pi--initial-prompt-default))
         (win (when-let* ((buf (nowis-pi--find-task-buffer remote old)))
                (get-buffer-window buf t))))
    ;; Check before killing so a typo cannot cost the old session.
    (unless (or (equal new-name old)
                (equal (nowis-pi--next-free-name remote new-name) new-name))
      (user-error "An agent named `%s' already exists" new-name))
    (nowis-pi--terminate-session s)
    (let ((buf (nowis-pi--open-session new-name (plist-get s :cwd) prompt
                                       'background
                                       (plist-get spec :args)
                                       (plist-get spec :cmd))))
      (if (window-live-p win)
          (progn (set-window-buffer win buf) (select-window win))
        (pop-to-buffer buf)))))

;;;###autoload
(defun nowis-pi-kill (&optional session)
  "Terminate SESSION's Pi process (or remove remote host at point in `*Pi Fleet*')."
  (interactive)
  (if-let* ((host (and (null session)
                       (null (get-text-property (point) 'nowis-pi-session))
                       (get-text-property (point) 'nowis-pi-host))))
      (when (yes-or-no-p (format "Remove remote host `%s' from Pi Fleet? " host))
        (setq nowis-pi-hosts (delete host nowis-pi-hosts))
        (remhash host nowis-pi--disk-cache)
        (when (derived-mode-p 'nowis-pi-fleet-mode)
          (nowis-pi-fleet--refresh-in-place))
        (message "Removed remote host `%s'" host))
    (let* ((s (or session (nowis-pi--current-session)))
           (name (plist-get s :name)))
      (when (yes-or-no-p (format "Kill Pi agent session `%s'? " name))
        (nowis-pi--terminate-session s)
        (when (derived-mode-p 'nowis-pi-fleet-mode)
          (nowis-pi-fleet--refresh-in-place))
        (message "Killed Pi agent `%s'" name)))))


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
  "x"   #'nowis-pi-run
  "M-!" #'nowis-pi-run
  "f"   #'nowis-pi-fork
  "s"   #'nowis-pi-restart
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
               "\\<nowis-pi-fleet-mode-map> Pi Fleet: \\[nowis-pi-fleet-open-at-point] open  \\[nowis-pi-fleet-attach-at-point]/\\[nowis-pi-fleet-attach-all] attach/all  \\[nowis-pi-new] new  \\[nowis-pi-run] run  \\[nowis-pi-fork] fork  \\[nowis-pi-restart] restart  \\[nowis-pi-rename] rename  \\[nowis-pi-fleet-remote] remote  \\[nowis-pi-dired] dired  \\[nowis-pi-review] magit  \\[nowis-pi-kill] delete  \\[revert-buffer] refresh")))

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

(defun nowis-pi-fleet--render-content (&optional scan-mode)
  "Render the `*Pi Fleet*' dashboard content into a propertized string."
  (let ((collapsed-set nowis-pi-fleet--collapsed))
    (with-temp-buffer
      (let ((sessions (nowis-pi--all-known-sessions scan-mode))
            (disconnected (cl-loop for h in nowis-pi-hosts
                                   when (and h (not (nowis-pi--host-connected-p h)))
                                   collect h)))
        (when disconnected
          (dolist (h disconnected)
            (let ((start (point)))
              (insert (propertize
                       (format "☁ [%s] offline\n" (string-trim h "/" ":"))
                       'face 'shadow))
              (put-text-property start (point) 'nowis-pi-item 'host)
              (put-text-property start (point) 'nowis-pi-host h)))
          (insert "\n"))
        (if (null sessions)
            (insert (propertize "  No active Pi agents. Press `c' to start one, or `R' to connect a remote host.\n"
                                'face 'shadow))
          (dolist (group (seq-group-by (lambda (s) (plist-get s :host)) sessions))
            (pcase-let ((`(,host . ,host-sessions) group))
              (let ((h-start (point)))
                (insert (propertize (format "[%s]\n" (string-trim host "/" ":"))
                                    'face 'font-lock-keyword-face))
                (unless (equal host "local")
                  (put-text-property h-start (point) 'nowis-pi-host host)))
              (dolist (s host-sessions)
                (let* ((name (plist-get s :name))
                       (key (concat host "::" name))
                       (collapsed (member key collapsed-set))
                       (buf (plist-get s :buffer))
                       (eff (nowis-pi--effective-status buf (plist-get s :sock-exists)))
                       (records (nowis-pi--buffer-records buf))
                       (root (plist-get records :root))
                       (subs (plist-get records :subagents))
                       (prompt (plist-get records :prompt))
                       (stats (plist-get records :stats))
                       (detail (nowis-pi--session-detail s root))
                       (start (point))
                       (head-end nil))
                  (insert (format "%s %s\n"
                                  (if collapsed "▶" "▼")
                                  (nowis-pi--session-headline s collapsed t)))
                  (setq head-end (point))
                  (put-text-property start head-end 'nowis-pi-item 'session)
                  (put-text-property start head-end 'nowis-pi-key key)
                  (unless collapsed
                    (when (and prompt (not (string-empty-p prompt)))
                      (insert (propertize (format "  │ 👤 %s\n"
                                                  (truncate-string-to-width prompt 84 nil nil "…"))
                                          'face 'font-lock-doc-face)))
                    (when detail
                      (insert (propertize (format "  │ %s %s\n"
                                                  (if (eq (plist-get eff :state) 'done) "🤖" "🔧")
                                                  (truncate-string-to-width detail 84 nil nil "…"))
                                          'face 'font-lock-string-face)))
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

(defun nowis-pi-fleet--refresh-in-place (&optional scan-mode)
  "Redraw `*Pi Fleet*' only when content changed, preserving cursor position."
  (let ((new-content (nowis-pi-fleet--render-content scan-mode)))
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
  (nowis-pi-fleet--refresh-in-place t))

;;;###autoload
(defun nowis-pi-fleet ()
  "Open the `*Pi Fleet*' process dashboard without blocking on remote hosts."
  (interactive)
  (let ((buf (get-buffer-create "*Pi Fleet*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'nowis-pi-fleet-mode)
        (nowis-pi-fleet-mode))
      (nowis-pi-fleet--refresh-in-place 'local))
    (pop-to-buffer buf)))

(defun nowis-pi-fleet-open-at-point ()
  "Open the Pi terminal (or connect remote host) at point."
  (interactive)
  (if-let* ((host (and (null (get-text-property (point) 'nowis-pi-session))
                       (get-text-property (point) 'nowis-pi-host))))
      (nowis-pi-fleet-remote host)
    (let ((s (nowis-pi-fleet--session-at-point)))
      (nowis-pi--open-session (plist-get s :name)
                              (plist-get s :cwd)
                              nil 'switch))))

(defun nowis-pi-fleet-attach-at-point ()
  "Attach the `⚡ detached' session at point in the background."
  (interactive)
  (let* ((s (nowis-pi-fleet--session-at-point))
         (name (plist-get s :name))
         (buf (plist-get s :buffer)))
    (if (nowis-pi--buffer-attached-p buf)
        (message "Agent `%s' is already attached" name)
      (nowis-pi--open-session name (plist-get s :cwd) nil 'background)
      (nowis-pi-fleet--refresh-in-place)
      (message "Attached Pi agent `%s'" name))))

(defun nowis-pi-fleet-attach-all ()
  "Reattach all `⚡ detached' sessions so their live telemetry & status resume."
  (interactive)
  (let ((sessions (nowis-pi--all-known-sessions 'local))
        (count 0))
    (dolist (s sessions)
      (let ((buf (plist-get s :buffer)))
        (when (and (plist-get s :sock-exists)
                   (nowis-pi--host-connected-p (plist-get s :remote))
                   (not (nowis-pi--buffer-attached-p buf)))
          (nowis-pi--open-session (plist-get s :name)
                                  (plist-get s :cwd)
                                  nil 'background)
          (cl-incf count))))
    (nowis-pi-fleet--refresh-in-place)
    (message "Reattached %d background Pi agent(s)" count)))

(defun nowis-pi-fleet-remote (tramp-prefix &optional remove)
  "Connect or add remote TRAMP-PREFIX (from `~/.ssh/config') and scan its agents.
With prefix argument REMOVE (`C-u R'), remove a tracked remote host instead."
  (interactive
   (if current-prefix-arg
       (let ((remotes (remq nil nowis-pi-hosts)))
         (unless remotes
           (user-error "No remote hosts in `nowis-pi-hosts'"))
         (list (completing-read "Remove remote TRAMP host: " remotes nil t
                                nil nil (get-text-property (point) 'nowis-pi-host))
               t))
     (let* ((offline (cl-loop for h in nowis-pi-hosts
                              when (and h (not (nowis-pi--host-connected-p h)))
                              collect h))
            (at-pt (get-text-property (point) 'nowis-pi-host))
            (def (or at-pt (car offline)))
            (candidates (delete-dups (append offline (nowis-pi--ssh-host-candidates)))))
       (list (completing-read
              (if def
                  (format "Connect remote TRAMP host (default %s): " def)
                "Connect remote TRAMP host (e.g. /ssh:cloud:): ")
              candidates nil nil nil nil def)
             nil))))
  (if remove
      (progn
        (setq nowis-pi-hosts (delete tramp-prefix nowis-pi-hosts))
        (remhash tramp-prefix nowis-pi--disk-cache)
        (when (derived-mode-p 'nowis-pi-fleet-mode)
          (nowis-pi-fleet--refresh-in-place))
        (message "Removed remote host %s" tramp-prefix))
    (let ((clean (nowis-pi--normalize-remote tramp-prefix)))
      (unless clean
        (user-error "Invalid TRAMP prefix: %s (expected e.g. /ssh:host:)" tramp-prefix))
      (message "Connecting to %s..." clean)
      (unless (file-directory-p (concat clean "~/"))
        (user-error "Failed to connect to %s" clean))
      (cl-pushnew clean nowis-pi-hosts :test #'equal)
      (nowis-pi-fleet--refresh-in-place t)
      (message "Connected %s" clean))))

(with-eval-after-load 'savehist
  (cl-pushnew 'nowis-pi-hosts savehist-additional-variables))
(with-eval-after-load 'consult
  (cl-pushnew 'nowis-pi-consult-source consult-buffer-sources))
(with-eval-after-load 'ghostel
  (add-hook 'ghostel-program-status-functions #'nowis-pi--on-status-change)
  (add-hook 'ghostel-exit-functions #'nowis-pi--on-buffer-exit))

(provide 'nowis-pi)
;;; nowis-pi.el ends here
