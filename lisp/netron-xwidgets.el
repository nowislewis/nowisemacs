;;; netron-xwidgets.el --- Open local and TRAMP models in Netron -*- lexical-binding: t; -*-

;;; Commentary:
;; Open local or remote (TRAMP / tramp-rpc) models (.onnx, .mlir, .tflite, .ort)
;; in the system browser via a single reused `netron' server process.

;;; Code:

(require 'dired)
(require 'subr-x)

(defvar mlir-mode-map)

(defgroup netron-xwidgets nil
  "Open local and remote ML models in Netron."
  :group 'tools
  :prefix "netron-xwidgets-")

(defcustom netron-xwidgets-python-program "python3"
  "Python 3 interpreter used for remote/local model slimming."
  :type 'string)

(defcustom netron-xwidgets-cache-directory
  (expand-file-name "emacs-netron" temporary-file-directory)
  "Local directory (`/tmp/emacs-netron' by default) for staged/slimmed models."
  :type 'directory)

(defcustom netron-xwidgets-slim-models t
  "When non-nil, strip large weights from `.onnx' and `.mlir' files by default."
  :type 'boolean)

(defcustom netron-xwidgets-slim-threshold (* 1024 1024)
  "Minimum file size in bytes (1 MiB default) before slimming is applied."
  :type 'integer)

(defvar netron-xwidgets--server-process nil)
(defvar netron-xwidgets--server-url nil)

(defconst netron-xwidgets--python-slim-script
  "import base64, re, sys
SCHEMA = {0:{7:1,25:2}, 1:{1:3,5:5,15:6}, 2:{7:3,11:4}, 3:{5:4},
          4:{5:5,6:1,10:5,11:1,22:6,23:6}, 6:{1:5,2:5}}
DROP = {4, 5, 6, 7, 9, 10, 11, 13, 14}
def rv(b, p):
    v = s = 0
    while True:
        x = b[p]; p += 1; v |= (x & 0x7f) << s
        if not (x & 0x80): return v, p
        s += 7
def ev(v):
    if v < 0: v += 1 << 64
    o = bytearray()
    while v > 0x7f: o.append((v & 0x7f) | 0x80); v >>= 7
    o.append(v & 0x7f); return o
def sk(b, p, w):
    if w == 0: _, p = rv(b, p); return p
    if w == 1: return p + 8
    if w == 2: ln, p = rv(b, p); return p + ln
    if w == 5: return p + 4
    raise ValueError(w)
def slim_pb(b, s, e, k, mx=512):
    if k != 0 and e - s <= mx: return b[s:e]
    o, p, ch = bytearray(), s, SCHEMA.get(k, {})
    while p < e:
        fs = p; tag, p = rv(b, p); fn, w = tag >> 3, tag & 7
        if k == 5:
            p = sk(b, p, w)
            if fn not in DROP: o.extend(b[fs:p])
        elif w == 2 and fn in ch:
            ln, vs = rv(b, p); ve = vs + ln; sub = slim_pb(b, vs, ve, ch[fn], mx)
            o.extend(ev((fn << 3) | 2)); o.extend(ev(len(sub))); o.extend(sub); p = ve
        else:
            p = sk(b, p, w); o.extend(b[fs:p])
    return o
def slim_mlir(txt, mx=256):
    txt = re.sub(r'\"0x[0-9A-Fa-f]{64,}\"', '\"0x00000000\"', txt)
    o, i, n, pat = [], 0, len(txt), re.compile(r'\\bdense<')
    while i < n:
        m = pat.search(txt, i)
        if not m: o.append(txt[i:]); break
        o.append(txt[i:m.end()]); j, d, q = m.end(), 0, False
        while j < n:
            c = txt[j]
            if q:
                if c == '\\\\': j += 2; continue
                if c == '\"': q = False
            elif c == '\"': q = True
            elif c in '[(': d += 1
            elif c in '])' and d > 0: d -= 1
            elif c == '>' and d == 0: break
            j += 1
        body = txt[m.end():j]
        o.append('\"0x00\"' if len(body) > mx and body.lstrip().startswith('[') else body); i = j
    return ''.join(o).encode('utf-8')
raw = open(sys.argv[1], 'rb').read()
res = bytes(slim_pb(memoryview(raw), 0, len(raw), 0)) if sys.argv[2] == 'onnx' else slim_mlir(raw.decode('utf-8', 'replace'))
sys.stdout.write(base64.b64encode(res).decode('ascii'))
")

(defun netron-xwidgets--stage-file (file &optional force-full)
  "Stage FILE locally (slimming if eligible) and return local staged path."
  (let* ((file (expand-file-name file))
         (ext (downcase (or (file-name-extension file) "")))
         (kind (and (member ext '("onnx" "mlir")) ext))
         (size (file-attribute-size (file-attributes file)))
         (slim-p (and (not force-full) netron-xwidgets-slim-models kind
                      (or (null size) (>= size netron-xwidgets-slim-threshold)))))
    (if (and (not (file-remote-p file)) (not slim-p))
        file
      (let* ((dir (expand-file-name
                   (concat (substring (md5 (file-name-directory file)) 0 10)
                           (if slim-p "-slim" "-full"))
                   netron-xwidgets-cache-directory))
             (staged (expand-file-name (file-name-nondirectory file) dir)))
        (make-directory dir t)
        (when (or (not (file-exists-p staged)) (file-newer-than-file-p file staged))
          (message "Netron: %s %s..." (if slim-p "Slimming" "Copying") (file-name-nondirectory file))
          (unless (and slim-p
                       (let ((default-directory (file-name-directory file))
                             (local (or (file-remote-p file 'localname) file)))
                         (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (when (and (eql 0 (ignore-errors
                                               (process-file netron-xwidgets-python-program
                                                             nil (list (current-buffer) nil) nil
                                                             "-c" netron-xwidgets--python-slim-script
                                                             local kind)))
                                      (> (buffer-size) 0))
                             (ignore-errors
                               (base64-decode-region (point-min) (point-max))
                               (let ((coding-system-for-write 'no-conversion))
                                 (write-region (point-min) (point-max) staged nil 'silent))
                               t)))))
            (copy-file file staged t)))
        staged))))

;;;###autoload
(defun netron-xwidgets-start-server ()
  "Start the global Netron server if not running, or reuse the existing one."
  (interactive)
  (if (and netron-xwidgets--server-process
           (process-live-p netron-xwidgets--server-process)
           netron-xwidgets--server-url)
      netron-xwidgets--server-url
    (let* ((py (let ((shim (expand-file-name "~/.pyenv/shims/python3")))
                 (if (file-executable-p shim) shim netron-xwidgets-python-program)))
           (url nil)
           (proc (make-process
                  :name "netron-server"
                  :command (list py "-c"
                                 "import sys, netron.server as s\naddr = s.serve(None, address=('127.0.0.1', 0), browse=False)\nprint(f'http://{addr[0]}:{addr[1]}', flush=True)\nfor line in sys.stdin:\n    f = line.strip()\n    if f:\n        s._HTTPRequestHandler.content = s._ContentProvider(None, f, f, f)\n        print('OK', flush=True)\n")
                  :noquery t
                  :connection-type 'pipe
                  :filter (lambda (_p chunk)
                            (when (string-match "http://127\\.0\\.0\\.1:[0-9]+" chunk)
                              (setq url (match-string 0 chunk)))))))
      (let ((deadline (+ (float-time) 8.0)))
        (while (and (null url) (process-live-p proc) (< (float-time) deadline))
          (accept-process-output proc 0.05)))
      (unless url
        (when (process-live-p proc) (delete-process proc))
        (error "Netron server failed to start"))
      (set-process-filter proc #'ignore)
      (setq netron-xwidgets--server-process proc
            netron-xwidgets--server-url url))))

;;;###autoload
(defun netron-xwidgets-stop-server ()
  "Force-stop all running Netron server processes."
  (interactive)
  (dolist (p (process-list))
    (when (string-prefix-p "netron" (process-name p))
      (delete-process p)))
  (setq netron-xwidgets--server-process nil
        netron-xwidgets--server-url nil)
  (message "Netron server stopped."))

;;;###autoload
(defun netron-xwidgets-open (&optional file force-full)
  "Open FILE (from Dired, current buffer, or prompt) in Netron via browser.
With `C-u' (FORCE-FULL), disable weight slimming."
  (interactive
   (list (cond ((derived-mode-p 'dired-mode) (dired-get-filename))
               (buffer-file-name buffer-file-name)
               (t (read-file-name "Open model in Netron: " nil nil t)))
         current-prefix-arg))
  (let ((staged (netron-xwidgets--stage-file file force-full))
        (url (netron-xwidgets-start-server)))
    (process-send-string netron-xwidgets--server-process (concat staged "\n"))
    (accept-process-output netron-xwidgets--server-process 1.0)
    (browse-url url)))

;;;###autoload
(defun netron-xwidgets-clear-cache ()
  "Delete all cached models in `netron-xwidgets-cache-directory'."
  (interactive)
  (when (file-directory-p netron-xwidgets-cache-directory)
    (delete-directory netron-xwidgets-cache-directory t))
  (message "Netron: Cleared %s" netron-xwidgets-cache-directory))

(defun netron-xwidgets--find-file-a (orig file &optional wildcards)
  "Open `.onnx', `.ort', and `.tflite' files directly in Netron."
  (if (and (stringp file)
           (string-match-p "\\.\\(?:onnx\\|ort\\|tflite\\)\\'" file)
           (file-exists-p file))
      (netron-xwidgets-open file current-prefix-arg)
    (funcall orig file wildcards)))

;;;###autoload
(defun netron-xwidgets-enable ()
  "Enable Netron integration for Dired and `.onnx' files."
  (interactive)
  (keymap-set dired-mode-map "C-c C-v" #'netron-xwidgets-open)
  (with-eval-after-load 'mlir-mode
    (keymap-set mlir-mode-map "C-c C-v" #'netron-xwidgets-open))
  (advice-add 'find-file :around #'netron-xwidgets--find-file-a))

(provide 'netron-xwidgets)
;;; netron-xwidgets.el ends here
