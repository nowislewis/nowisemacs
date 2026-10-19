;;; nowis-board.el --- Excalidraw + Denote/Org companion board -*- lexical-binding: t; -*-

;;; Commentary:
;; Lightweight external-browser Excalidraw board integrated with Denote and
;; Org mode.  Cards are standard Excalidraw rectangles whose `link' field
;; holds a native `denote:<ID>' or `denote:<ID>::#<CUSTOM_ID>' URI.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-id)
(require 'ox)
(require 'ox-html)
(require 'pulse)
(require 'seq)
(require 'subr-x)
(require 'url-util)

(defvar denote-directory)
(declare-function denote-directory "denote" ())
(declare-function denote-file-prompt "denote" (&optional files-matching-regexp prompt-text no-require-match has-identifier))
(declare-function denote-format-file-name "denote" (dir-path id keywords title extension signature))
(declare-function denote-format-front-matter "denote" (title date keywords id signature filetype))
(declare-function denote-get-path-by-id "denote" (id))
(declare-function denote-link-ol-get-id "denote" ())
(declare-function denote-retrieve-filename-identifier "denote" (file))
(declare-function denote-retrieve-title-or-filename "denote" (file type))
(declare-function denote-sluggify-title "denote" (str))

(defgroup nowis-board nil
  "External-browser Excalidraw board backed by Denote and Org."
  :group 'org
  :prefix "nowis-board-")

(defcustom nowis-board-sync-delay 0.35
  "Idle seconds before Org edits push updated card HTML to the browser."
  :type 'number)

(defcustom nowis-board-export-svg t
  "When non-nil, write a companion `.svg' file alongside `.excalidraw' saves."
  :type 'boolean)

(defcustom nowis-board-browser-function #'browse-url
  "Function used to open the board URL in an external browser."
  :type 'function)

(defconst nowis-board--web-dir
  (expand-file-name
   "nowis-board"
   (file-name-directory (or load-file-name buffer-file-name
                            (locate-library "nowis-board"))))
  "Directory containing static web assets for `nowis-board'.")

(defvar nowis-board--server-process nil)
(defvar nowis-board--server-port nil)
(defvar nowis-board--sse-clients nil
  "Alist of (PROC . BOARD-FILE) for active Server-Sent Events connections.")
(defvar nowis-board--active-file nil
  "Most recently opened or interacted `.excalidraw' board file.")
(defvar nowis-board--selected-ids (make-hash-table :test #'equal)
  "Map from board file path to list of currently selected element IDs.")
(defvar nowis-board--sync-timer nil)
(defvar nowis-board--dirty-files nil)
(defvar nowis-board--pending-inserts nil
  "Alist of (BOARD-FILE . LINK) queued to insert when `/api/events' connects.")

;;; Denote & Org Resolution Primitives

;; Parse `denote:ID[::target]' into (:id ID :file FILE :target TARGET) so
;; whole notes and heading subtrees share a single lookup contract.
(defun nowis-board--resolve-denote-link (link)
  "Resolve a `denote:' LINK into a plist of `:id', `:file', and `:target'."
  (require 'denote)
  (when (and (stringp link) (string-prefix-p "denote:" link))
    (let* ((parts (split-string (substring link (length "denote:")) "::" t))
           (id (car parts))
           (target (cadr parts))
           (file (and id (denote-get-path-by-id id))))
      (when (and file (file-exists-p file))
        (list :id id :file (expand-file-name file) :target target)))))

;; Locate a heading inside an Org buffer by `#CUSTOM_ID', `ID', or `*Title'.
(defun nowis-board--goto-org-target (target)
  "Move point in current Org buffer to TARGET (`#id' or `*heading')."
  (widen)
  (goto-char (point-min))
  (cond
   ((or (null target) (string-empty-p target)) t)
   ((string-prefix-p "*" target)
    (re-search-forward
     (format "^\\*+[ \t]+\\(?:TODO\\|DONE\\|NEXT\\|WAITING\\)?[ \t]*%s\\b"
             (regexp-quote (substring target 1)))
     nil t))
   (t
    (let* ((id (string-remove-prefix "#" target))
           (pos (or (org-find-property "CUSTOM_ID" id)
                    (org-find-entry-with-id id))))
      (when pos (goto-char pos) t)))))

;; Convert Org markup into an HTML fragment and preserve `denote:'/`id:'/`file:'
;; links as `data-emacs-link' attributes for in-card click navigation.
(defun nowis-board--org-to-html (org-text base-dir)
  "Render ORG-TEXT to an HTML fragment relative to BASE-DIR."
  (let* ((pre-processed
          (replace-regexp-in-string
           "\\[\\[\\(\\(?:denote\\|id\\|file\\):[^]\n]+\\)\\]\\(?:\\[\\([^]\n]+\\)\\]\\)?\\]"
           (lambda (m)
             (let ((uri (match-string 1 m))
                   (desc (or (match-string 2 m) (match-string 1 m))))
               (format "@@html:<a href=\"#\" data-emacs-link=\"%s\">%s</a>@@"
                       (org-html-encode-plain-text uri)
                       (org-html-encode-plain-text desc))))
           org-text t t))
         (default-directory (or base-dir default-directory))
         (org-export-with-toc nil)
         (org-export-with-section-numbers nil)
         (org-export-with-drawers nil)
         (org-export-with-planning nil)
         (org-export-with-sub-superscripts '{})
         (org-html-validation-link nil))
    (org-export-string-as pre-processed 'html t)))

;; Extract title, subtitle badge, and rendered HTML for a `denote:' card URI.
(defun nowis-board--card-payload (link)
  "Build the JSON-serializable card alist for `denote:' LINK."
  (if-let* ((resolved (nowis-board--resolve-denote-link link))
            (file (plist-get resolved :file))
            (id (plist-get resolved :id))
            (buf (or (find-buffer-visiting file)
                     (let ((enable-local-variables :safe))
                       (find-file-noselect file)))))
      (let ((target (plist-get resolved :target)))
        (with-current-buffer buf
          (save-excursion
            (save-restriction
              (if (not (nowis-board--goto-org-target target))
                  `((link . ,link)
                    (title . ,(format "Missing heading: %s" target))
                    (subtitle . ,id)
                    (html . "<p><em>Target heading not found in note.</em></p>"))
                (let* ((is-heading (and target (org-at-heading-p)))
                       (note-title (or (denote-retrieve-title-or-filename file 'org)
                                       (file-name-base file)))
                       (title (if is-heading (org-get-heading t t t t) note-title))
                       (subtitle (if is-heading (format "%s · §" note-title) id))
                       (raw-body
                        (if is-heading
                            (save-excursion
                              (org-back-to-heading t)
                              (forward-line 1)
                              (buffer-substring-no-properties
                               (point)
                               (save-excursion (org-end-of-subtree t t) (point))))
                          (buffer-substring-no-properties (point-min) (point-max))))
                       (html (nowis-board--org-to-html
                              raw-body (file-name-directory file))))
                  `((link . ,link)
                    (title . ,title)
                    (subtitle . ,subtitle)
                    (html . ,(if (string-empty-p (string-trim html))
                                 "<p><em>(Empty)</em></p>"
                               html)))))))))
    `((link . ,link)
      (title . ,(format "Unresolved: %s" link))
      (subtitle . "missing")
      (html . "<p><em>Denote note not found in denote-directory.</em></p>"))))

;;; Silent Companion Navigation (Browser -> Emacs without stealing focus)

;; Jump to LINK in an Emacs window, unfold, recenter, and pulse without
;; calling `select-frame-set-input-focus'.
(defun nowis-board--visit-link (link)
  "Display and highlight LINK (`denote:', `id:', or `file:') in Emacs."
  (let (buf pos)
    (cond
     ((string-prefix-p "denote:" link)
      (when-let* ((resolved (nowis-board--resolve-denote-link link))
                  (file (plist-get resolved :file)))
        (setq buf (find-file-noselect file))
        (with-current-buffer buf
          (nowis-board--goto-org-target (plist-get resolved :target))
          (setq pos (point)))))
     ((string-prefix-p "id:" link)
      (when-let* ((m (org-id-find (substring link 3) 'marker)))
        (setq buf (marker-buffer m)
              pos (marker-position m))
        (set-marker m nil)))
     ((string-prefix-p "file:" link)
      (let ((path (expand-file-name (substring link 5))))
        (when (file-exists-p path)
          (setq buf (find-file-noselect path)
                pos 1)))))
    (when (and buf (buffer-live-p buf))
      (let ((win (display-buffer
                  buf '((display-buffer-reuse-window
                         display-buffer-use-some-window)))))
        (with-current-buffer buf
          (when pos (goto-char pos))
          (when (derived-mode-p 'org-mode)
            (org-fold-show-context 'link-search)
            (when (org-at-heading-p)
              (org-fold-show-entry)
              (org-fold-show-children))))
        (when (window-live-p win)
          (with-selected-window win
            (when pos (goto-char pos))
            (recenter)
            (pulse-momentary-highlight-one-line (point))))))))

;;; Scene JSON Helpers

(defun nowis-board--default-scene ()
  "Return a minimal empty `.excalidraw' scene alist."
  `((type . "excalidraw")
    (version . 2)
    (source . "nowis-board")
    (elements . [])
    (appState . ((gridSize . :null)
                 (viewBackgroundColor . "#f8fafc")))
    (files . ,(make-hash-table :test #'equal))))

(defun nowis-board--read-scene (file)
  "Read `.excalidraw' FILE as an alist, or return a fresh scene."
  (if (and file (file-exists-p file) (> (file-attribute-size (file-attributes file)) 0))
      (let ((json-object-type 'alist)
            (json-array-type 'vector)
            (json-false :json-false)
            (json-null nil))
        (json-read-file file))
    (nowis-board--default-scene)))

(defun nowis-board--write-scene (file scene-or-json &optional svg)
  "Atomically write SCENE-OR-JSON to FILE and optional companion SVG."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix)
        (json-encoding-pretty-print t))
    (with-temp-file file
      (insert (if (stringp scene-or-json)
                  scene-or-json
                (json-encode scene-or-json))
              "\n"))
    (when (and nowis-board-export-svg (stringp svg) (not (string-empty-p svg)))
      (with-temp-file (concat file ".svg")
        (insert svg)))))

(defun nowis-board--scene-cards (scene)
  "Collect card payloads for all non-deleted `denote:' elements in SCENE."
  (let (cards)
    (seq-doseq (el (alist-get 'elements scene))
      (let ((link (alist-get 'link el)))
        (when (and (not (eq (alist-get 'isDeleted el) t))
                   (stringp link)
                   (string-prefix-p "denote:" link)
                   (not (assoc link cards)))
          (push (cons link (nowis-board--card-payload link)) cards))))
    cards))

;;; Live Org -> Browser Sync (SSE)

(defun nowis-board--live-client-p (board-file)
  "Return non-nil if BOARD-FILE has an active browser SSE connection."
  (let ((target (expand-file-name board-file)))
    (seq-some (lambda (cell)
                (and (process-live-p (car cell))
                     (equal (expand-file-name (cdr cell)) target)))
              nowis-board--sse-clients)))

(defun nowis-board--sse-broadcast (event data &optional board-file)
  "Broadcast SSE EVENT and DATA to connected clients (optionally for BOARD-FILE)."
  (setq nowis-board--sse-clients
        (seq-filter (lambda (cell) (process-live-p (car cell)))
                    nowis-board--sse-clients))
  (let ((payload (encode-coding-string
                  (format "event: %s\ndata: %s\n\n" event (json-encode data))
                  'utf-8)))
    (pcase-dolist (`(,proc . ,file) nowis-board--sse-clients)
      (when (or (null board-file)
                (string-empty-p file)
                (equal (expand-file-name file) (expand-file-name board-file)))
        (condition-case nil
            (process-send-string proc payload)
          (error (setq nowis-board--sse-clients
                       (assq-delete-all proc nowis-board--sse-clients))))))))

;; Re-render all cards on active boards that reference any modified Denote file.
(defun nowis-board--flush-dirty-files ()
  "Push updated card HTML for `nowis-board--dirty-files' over SSE."
  (setq nowis-board--sync-timer nil)
  (let ((files (delete-dups nowis-board--dirty-files)))
    (setq nowis-board--dirty-files nil)
    (when (and files nowis-board--sse-clients)
      (let ((ids (delq nil (mapcar #'denote-retrieve-filename-identifier files)))
            (board-files (delete-dups
                          (delq nil (cons nowis-board--active-file
                                          (mapcar #'cdr nowis-board--sse-clients)))))
            updates)
        (dolist (bfile board-files)
          (when (and bfile (file-exists-p bfile))
            (seq-doseq (el (alist-get 'elements (nowis-board--read-scene bfile)))
              (let ((link (alist-get 'link el)))
                (when (and (stringp link)
                           (seq-some (lambda (id)
                                       (string-prefix-p (concat "denote:" id) link))
                                     ids)
                           (not (assoc link updates)))
                  (push (cons link (nowis-board--card-payload link))
                        updates))))))
        (when updates
          (nowis-board--sse-broadcast "cards" updates))))))

(defun nowis-board--schedule-buffer-sync (&rest _)
  "Queue current Denote Org buffer for debounced SSE card refresh."
  (when (and nowis-board--sse-clients
             buffer-file-name
             (derived-mode-p 'org-mode)
             (denote-retrieve-filename-identifier buffer-file-name))
    (cl-pushnew (expand-file-name buffer-file-name)
                nowis-board--dirty-files :test #'equal)
    (when (timerp nowis-board--sync-timer)
      (cancel-timer nowis-board--sync-timer))
    (setq nowis-board--sync-timer
          (run-with-idle-timer nowis-board-sync-delay nil
                               #'nowis-board--flush-dirty-files))))

(defun nowis-board--org-mode-setup ()
  "Attach buffer-local change listener inside Org buffers."
  (add-hook 'after-change-functions #'nowis-board--schedule-buffer-sync nil t)
  (add-hook 'after-save-hook #'nowis-board--schedule-buffer-sync nil t))

(defun nowis-board--attach-org-hooks ()
  "Enable live change tracking in existing and future Org buffers."
  (add-hook 'org-mode-hook #'nowis-board--org-mode-setup)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'org-mode)
        (nowis-board--org-mode-setup)))))

;;; Embedded HTTP + SSE Server

(defun nowis-board--query-param (query key)
  "Return URL-decoded parameter KEY from QUERY string."
  (cadr (assoc key (url-parse-query-string (or query "")))))

(defun nowis-board--http-respond (proc status content-type body)
  "Send an HTTP response with STATUS, CONTENT-TYPE, and string BODY to PROC."
  (when (process-live-p proc)
    (let* ((encoded (encode-coding-string (or body "") 'utf-8))
           (headers (format (concat "HTTP/1.1 %s\r\n"
                                    "Content-Type: %s\r\n"
                                    "Content-Length: %d\r\n"
                                    "Cache-Control: no-store\r\n"
                                    "Connection: close\r\n\r\n")
                            status content-type (string-bytes encoded))))
      (process-send-string
       proc (concat (encode-coding-string headers 'utf-8) encoded))
      (delete-process proc))))

(defun nowis-board--handle-request (proc method path query body)
  "Route a complete HTTP request from PROC."
  (cond
   ((and (equal method "GET") (member path '("/" "/index.html")))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "index.html" nowis-board--web-dir))
      (nowis-board--http-respond
       proc "200 OK" "text/html; charset=utf-8" (buffer-string))))

   ((and (equal method "GET") (equal path "/api/board"))
    (let* ((file (expand-file-name (or (nowis-board--query-param query "file")
                                       nowis-board--active-file)))
           (scene (nowis-board--read-scene file))
           (scene-json (when (and file (file-exists-p file)
                                  (> (file-attribute-size (file-attributes file)) 0))
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))))
           (cards (nowis-board--scene-cards scene)))
      (setq nowis-board--active-file file)
      (nowis-board--http-respond
       proc "200 OK" "application/json; charset=utf-8"
       (json-encode `((file . ,file)
                      (title . ,(file-name-base file))
                      (sceneJson . ,scene-json)
                      (cards . ,cards))))))

   ((and (equal method "GET") (equal path "/api/card"))
    (nowis-board--http-respond
     proc "200 OK" "application/json; charset=utf-8"
     (json-encode (nowis-board--card-payload
                   (nowis-board--query-param query "link")))))

   ((and (equal method "GET") (equal path "/api/visit"))
    (when-let* ((link (nowis-board--query-param query "link")))
      (nowis-board--visit-link link))
    (nowis-board--http-respond
     proc "200 OK" "application/json; charset=utf-8" "{\"ok\":true}"))

   ((and (equal method "POST") (equal path "/api/save"))
    (let* ((file (expand-file-name (or (nowis-board--query-param query "file")
                                       nowis-board--active-file)))
           (json-object-type 'alist)
           (json-array-type 'vector)
           (json-false :json-false)
           (json-null nil)
           (payload (json-read-from-string body))
           (scene (or (alist-get 'sceneJson payload)
                      (alist-get 'scene payload)))
           (selected (append (alist-get 'selectedIds payload) nil))
           (svg (alist-get 'svg payload)))
      (when file
        (puthash file selected nowis-board--selected-ids)
        (when scene
          (nowis-board--write-scene file scene svg)))
      (nowis-board--http-respond
       proc "200 OK" "application/json; charset=utf-8" "{\"ok\":true}")))

   ((and (equal method "GET") (equal path "/api/events"))
    (let ((file (expand-file-name
                 (or (nowis-board--query-param query "file")
                     nowis-board--active-file ""))))
      (process-send-string
       proc
       (concat "HTTP/1.1 200 OK\r\n"
               "Content-Type: text/event-stream\r\n"
               "Cache-Control: no-cache\r\n"
               "Connection: keep-alive\r\n\r\n"
               ": connected\n\n"))
      (push (cons proc file) nowis-board--sse-clients)
      (when-let* ((pending (alist-get file nowis-board--pending-inserts
                                      nil nil #'equal)))
        (setf (alist-get file nowis-board--pending-inserts nil 'remove #'equal) nil)
        (nowis-board--sse-broadcast
         "insert-card"
         `((link . ,pending) (card . ,(nowis-board--card-payload pending)))
         file))))

   (t
    (nowis-board--http-respond
     proc "404 Not Found" "text/plain; charset=utf-8" "Not Found"))))

;; Accumulate unibyte TCP chunks until full HTTP headers and `Content-Length'
;; byte body arrive.
(defun nowis-board--server-filter (proc chunk)
  "Accumulate incoming bytes from PROC and dispatch complete HTTP requests."
  (let ((buf (concat (or (process-get proc :raw-buffer) (unibyte-string))
                     (encode-coding-string chunk 'raw-text))))
    (process-put proc :raw-buffer buf)
    (when-let* ((header-end (string-match "\r\n\r\n" buf)))
      (let* ((header-str (substring buf 0 header-end))
             (body-start (+ header-end 4))
             (content-length
              (if (string-match "^[Cc]ontent-[Ll]ength:[ \t]*\\([0-9]+\\)" header-str)
                  (string-to-number (match-string 1 header-str))
                0)))
        (when (>= (length buf) (+ body-start content-length))
          (let* ((body-bytes (substring buf body-start (+ body-start content-length)))
                 (body (decode-coding-string body-bytes 'utf-8))
                 (first-line (car (split-string header-str "\r\n")))
                 (tokens (split-string first-line " "))
                 (method (car tokens))
                 (q-split (split-string (or (cadr tokens) "/") "\\?"))
                 (path (car q-split))
                 (query (cadr q-split)))
            (process-put proc :raw-buffer nil)
            (condition-case err
                (nowis-board--handle-request proc method path query body)
              (error
               (nowis-board--http-respond
                proc "500 Internal Server Error" "text/plain; charset=utf-8"
                (error-message-string err))))))))))

(defun nowis-board--ensure-server ()
  "Start the local HTTP/SSE server if not already running and return its port."
  (unless (and nowis-board--server-process
               (process-live-p nowis-board--server-process))
    (let ((proc (make-network-process
                 :name "nowis-board-server"
                 :server t
                 :host "127.0.0.1"
                 :service t
                 :coding 'binary
                 :noquery t
                 :filter #'nowis-board--server-filter)))
      (setq nowis-board--server-process proc
            nowis-board--server-port (process-contact proc :service))
      (nowis-board--attach-org-hooks)))
  nowis-board--server-port)

;;; Board File Discovery & Creation

(defun nowis-board--read-board-file ()
  "Prompt for an existing `.excalidraw' board in `denote-directory' or create one."
  (require 'denote)
  (let* ((dir (file-name-as-directory (denote-directory)))
         (files (directory-files-recursively dir "\\.excalidraw\\'"))
         (rel-alist (mapcar (lambda (f) (cons (file-relative-name f dir) f)) files))
         (choices (cons '("[New Denote board...]" . :new) rel-alist))
         (default (and nowis-board--active-file
                       (file-exists-p nowis-board--active-file)
                       (file-relative-name nowis-board--active-file dir)))
         (sel (completing-read "Board: " choices nil t nil nil
                               (or default "[New Denote board...]")))
         (picked (alist-get sel choices nil nil #'equal)))
    (if (eq picked :new)
        (let* ((title (read-string "New board title: "))
               (slug (denote-sluggify-title (if (string-empty-p title) "board" title)))
               (id (format-time-string "%Y%m%dT%H%M%S"))
               (file (expand-file-name
                      (format "%s--%s__board.excalidraw" id slug) dir)))
          (nowis-board--write-scene file (nowis-board--default-scene))
          file)
      picked)))

;;; Interactive Commands

;;;###autoload
(defun nowis-board-open (&optional file)
  "Open or create an `.excalidraw' board FILE in the external browser."
  (interactive)
  (let* ((target (expand-file-name (or file (nowis-board--read-board-file))))
         (port (nowis-board--ensure-server))
         (url (format "http://127.0.0.1:%d/?file=%s"
                      port (url-hexify-string target))))
    (unless (file-exists-p target)
      (nowis-board--write-scene target (nowis-board--default-scene)))
    (setq nowis-board--active-file target)
    (funcall nowis-board-browser-function url)
    (message "Opened board: %s" (file-name-nondirectory target))))

;;;###autoload
(defun nowis-board-add-here (&optional whole-note)
  "Send current Org heading (or whole Denote note with prefix WHOLE-NOTE) to board."
  (interactive "P")
  (require 'denote)
  (unless (and buffer-file-name (derived-mode-p 'org-mode))
    (user-error "Run this command inside a file-backed Org buffer"))
  (let ((file-id (denote-retrieve-filename-identifier buffer-file-name)))
    (unless file-id
      (user-error "Current buffer is not a Denote file (missing identifier)"))
    (let* ((on-heading (and (not whole-note) (not (org-before-first-heading-p))))
           (link (if on-heading
                     (save-excursion
                       (org-back-to-heading t)
                       (format "denote:%s::#%s" file-id (denote-link-ol-get-id)))
                   (format "denote:%s" file-id)))
           (board-file (expand-file-name
                        (or (and nowis-board--active-file
                                 (file-exists-p nowis-board--active-file)
                                 nowis-board--active-file)
                            (nowis-board--read-board-file))))
           (card (nowis-board--card-payload link)))
      (setq nowis-board--active-file board-file)
      (if (nowis-board--live-client-p board-file)
          (nowis-board--sse-broadcast
           "insert-card" `((link . ,link) (card . ,card)) board-file)
        (setf (alist-get board-file nowis-board--pending-inserts nil nil #'equal)
              link)
        (nowis-board-open board-file))
      (message "Added %s to %s" link (file-name-nondirectory board-file)))))

;; Extract plain text from a draft shape (either a `text' element or a shape
;; with a bound `text' element) so it can be promoted into Denote/Org.
(defun nowis-board--draft-candidates (scene)
  "Return an alist of (LABEL . (ELEMENT-ID . TEXT)) for unlinked shapes in SCENE."
  (let* ((elements (append (alist-get 'elements scene) nil))
         (by-container (make-hash-table :test #'equal))
         candidates)
    (dolist (el elements)
      (when (and (not (eq (alist-get 'isDeleted el) t))
                 (equal (alist-get 'type el) "text")
                 (stringp (alist-get 'containerId el)))
        (puthash (alist-get 'containerId el)
                 (or (alist-get 'originalText el) (alist-get 'text el) "")
                 by-container)))
    (dolist (el elements)
      (let* ((id (alist-get 'id el))
             (type (alist-get 'type el))
             (link (alist-get 'link el))
             (deleted (eq (alist-get 'isDeleted el) t))
             (container (alist-get 'containerId el))
             (text (cond
                    ((equal type "text")
                     (and (null container)
                          (or (alist-get 'originalText el) (alist-get 'text el) "")))
                    ((member type '("rectangle" "diamond" "ellipse"))
                     (or (gethash id by-container)
                         (alist-get 'originalText el)
                         (alist-get 'text el)
                         "")))))
        (when (and (not deleted)
                   (or (null link) (string-empty-p link))
                   text
                   (not (string-empty-p (string-trim text))))
          (let* ((clean (string-trim text))
                 (first-line (car (split-string clean "\n" t)))
                 (label (format "%s  [%s]"
                                (truncate-string-to-width first-line 60 nil nil "…")
                                id)))
            (push (cons label (cons id clean)) candidates)))))
    (nreverse candidates)))

;;;###autoload
(defun nowis-board-promote ()
  "Promote a draft shape on the active board into a Denote note or Org heading."
  (interactive)
  (require 'denote)
  (let* ((board-file (or (and nowis-board--active-file
                              (file-exists-p nowis-board--active-file)
                              nowis-board--active-file)
                         (nowis-board--read-board-file)))
         (scene (nowis-board--read-scene board-file))
         (candidates (nowis-board--draft-candidates scene)))
    (unless candidates
      (user-error "No unlinked draft text/shapes found on %s"
                  (file-name-nondirectory board-file)))
    (let* ((selected-ids (gethash board-file nowis-board--selected-ids))
           (default-cand (seq-find (lambda (c) (member (cadr c) selected-ids))
                                   candidates))
           (choice (if (= (length candidates) 1)
                       (cdar candidates)
                     (cdr (assoc (completing-read
                                  "Promote draft element: " candidates nil t
                                  nil nil (car default-cand))
                                 candidates))))
           (el-id (car choice))
           (lines (split-string (cdr choice) "\n"))
           (title (string-trim (replace-regexp-in-string "^\\*+[ \t]*" "" (car lines))))
           (body (string-trim (string-join (cdr lines) "\n")))
           (target-kind (completing-read
                         "Promote as: "
                         '("1. Heading in current Org buffer"
                           "2. Heading in existing Denote Org file"
                           "3. New standalone Denote Org note")
                         nil t))
           link)
      (if (string-prefix-p "3." target-kind)
          (let* ((dir (file-name-as-directory (denote-directory)))
                 (id (format-time-string "%Y%m%dT%H%M%S"))
                 (slug (denote-sluggify-title title))
                 (new-file (denote-format-file-name dir id nil slug ".org" nil)))
            (with-current-buffer (find-file-noselect new-file)
              (insert (denote-format-front-matter
                       title (current-time) nil id "" 'org))
              (unless (string-empty-p body)
                (insert "\n" body "\n"))
              (save-buffer))
            (setq link (format "denote:%s" id)))
        (let* ((org-file (if (string-prefix-p "1." target-kind)
                             (or (and (derived-mode-p 'org-mode) buffer-file-name)
                                 (user-error "Current buffer is not a file-backed Org buffer"))
                           (denote-file-prompt "\\.org\\'" "Target Denote Org file")))
               (file-id (or (denote-retrieve-filename-identifier org-file)
                            (user-error "File is not a Denote note: %s" org-file))))
          (with-current-buffer (find-file-noselect org-file)
            (save-excursion
              (widen)
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert (format "\n* %s\n" title))
              (forward-line -1)
              (let ((hid (denote-link-ol-get-id)))
                (goto-char (point-max))
                (unless (string-empty-p body)
                  (insert body "\n"))
                (save-buffer)
                (setq link (format "denote:%s::#%s" file-id hid)))))))
      (if (nowis-board--live-client-p board-file)
          (nowis-board--sse-broadcast
           "promote-selected"
           `((elementId . ,el-id)
             (link . ,link)
             (card . ,(nowis-board--card-payload link)))
           board-file)
        (seq-doseq (el (alist-get 'elements scene))
          (cond
           ((equal (alist-get 'id el) el-id)
            (setf (alist-get 'type el) "rectangle"
                  (alist-get 'width el) (max (or (alist-get 'width el) 0) 360)
                  (alist-get 'height el) (max (or (alist-get 'height el) 0) 240)
                  (alist-get 'roughness el) 0
                  (alist-get 'fillStyle el) "solid"
                  (alist-get 'link el) link
                  (alist-get 'version el) (1+ (or (alist-get 'version el) 1))))
           ((equal (alist-get 'containerId el) el-id)
            (setf (alist-get 'isDeleted el) t))))
        (nowis-board--write-scene board-file scene))
      (message "Promoted draft to %s" link))))

(provide 'nowis-board)
;;; nowis-board.el ends here
