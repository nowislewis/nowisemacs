;;; nowis-gantt.el --- Project GTD → TaskJuggler gantt -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: org, gantt, taskjuggler, project

;;; Commentary:
;;
;; Project a GTD subtree into a TaskJuggler (.tjp) file and let the tj3
;; solver draw a Gantt chart.  The GTD org tree stays the SINGLE SOURCE OF
;; TRUTH; the .tjp and the HTML are disposable, derived-on-demand artifacts.
;;
;; ============================ PHILOSOPHY ==================================
;;   - GTD is the persistent "stock"; the gantt is transient "flow".
;;   - We do NOT hand-maintain a second TaskJuggler tree; we PROJECT one on
;;     demand from the org headings you already keep.
;;   - CLOCK intervals become tj3 `booking' lines, so a single chart shows
;;     the PAST as fact (纯时间, 柳比歇夫) and the FUTURE as a solved schedule
;;     (侯世达法则: let the solver, not wishful thinking, place the bars).
;;   - Every task attribute reuses a built-in org facility, so nothing here
;;     drifts when you retune `org-todo-keywords' etc.
;;
;; ============================ QUICK START =================================
;;   1. Put :gantt: on a project headline.
;;   2. With point anywhere in that subtree:  M-x nowis/gtd-gantt
;;      -> projects the subtree, runs tj3, opens the HTML gantt.
;;
;; ======================= FIELD MAPPING (per task) ========================
;;   org heading tree     → task nesting (WBS: a parent = summary bar)
;;   :ID: (org-id)        → task id     (only needed if something depends on it;
;;                                        else a stable id from buffer pos)
;;   :Effort:             → effort      (5d / 1w / 4h / 1:30; "d"/"w" = WORK
;;                                        days/weeks; missing -> default 1d)
;;   SCHEDULED            → start       (soft anchor, only on leaves w/o clock)
;;   CLOCK intervals      → booking     (actual work done; see below)
;;   :DEPENDS: a b        → depends     (space/comma separated task ids)
;;   :DEPENDS: previous   → depends the previous SIBLING task
;;   :ALLOCATE: name      → allocate    (default: single resource `me')
;;   :LIMITS: dailymax 4h → limits      (每天对半: effort/rate = longer span)
;;   org DONE state       → complete 100 (via :todo-type, keyword-list-free)
;;   tag :milestone:      → milestone   (zero-duration diamond; no effort)
;;
;; ======================= SCHEDULING SEMANTICS ============================
;; DEPENDS on a CONTAINER = wait for ALL its children.  So a task that must
;; start only after a whole phase finishes just depends on that phase's id.
;;
;; SEQUENTIAL siblings -- two equivalent ways:
;;   * whole group in order: put :ORDERED: t on the PARENT; each child then
;;     implicitly depends on the one above it.
;;   * one task only:        that task writes :DEPENDS: previous.
;; Without either, siblings have NO order dependency; any apparent serial
;; run is merely the single shared resource queueing (a resource, not an
;; ordering, constraint -- give them different resources to see them run
;; in parallel).
;;
;; MILESTONE: tag a leaf :milestone: to make it a zero-duration point.  A
;; missing :Effort: does NOT make a milestone (it gets the default effort),
;; so an un-estimated task never silently collapses to a point.
;;
;; ============================== CLOCK ====================================
;; Each CLOCK line inside a LEAF headline (not its children) becomes one
;;   booking me <start> +<minutes>
;; tj3 treats bookings as work that already happened, so it (a) fixes that
;; slice of the bar in the past and (b) derives a % complete from
;; booked-vs-effort.  A DONE task additionally forces complete 100.
;; The project's outer start is pulled back to the earliest CLOCK so those
;; past bookings always fall inside the project time frame.
;;
;; ========================= PROJECT-LEVEL PROPS ===========================
;;   :GANTT_START: 2026-07-06  (default: earliest CLOCK, else today)
;;   :GANTT_END:   2026-09-01  (default: start + 3 months)
;;   :GANTT_NOW:   2026-07-09  (default: today; the past/future divider)
;;   :ORDERED: t               (children run sequentially, as above)

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-duration)
(require 'org-id)
(require 'cl-lib)

;;;; ---- Customisation -------------------------------------------------------

(defgroup nowis/gantt nil
  "Project GTD subtree into a TaskJuggler gantt."
  :group 'org)

(defcustom nowis/gantt-command "tj3"
  "The tj3 executable."
  :type 'string :group 'nowis/gantt)

(defcustom nowis/gantt-output-dir
  (expand-file-name "nowis-gantt/" temporary-file-directory)
  "Directory where the throwaway .tjp and HTML report are written.
Defaults to a subdirectory of the system temp dir, since these are
regenerated on demand from the org single-source-of-truth."
  :type 'directory :group 'nowis/gantt)

(defcustom nowis/gantt-project-tag "gantt"
  "Headlines carrying this tag are treated as TaskJuggler projects."
  :type 'string :group 'nowis/gantt)

(defcustom nowis/gantt-resource-id "me"
  "Default single resource id used when a task has no :allocate:."
  :type 'string :group 'nowis/gantt)

(defcustom nowis/gantt-resource-name "Me"
  "Human name for the default resource id `nowis/gantt-resource-id'."
  :type 'string :group 'nowis/gantt)

(defcustom nowis/gantt-resource-names nil
  "Alist mapping resource id (string) to a human display name (string).
Any id used in :ALLOCATE: that is absent here is declared under its own
id as the name, e.g. (\"alice\" . \"Alice Chen\")."
  :type '(alist :key-type string :value-type string)
  :group 'nowis/gantt)

(defcustom nowis/gantt-done-color "#a6e3a1"
  "Background color for the name cell of a 100%%-complete (DONE) task,
giving completion a simple, glanceable visual in the report.  Nil = off."
  :type '(choice (const :tag "off" nil) string)
  :group 'nowis/gantt)

(defcustom nowis/gantt-scale "day"
  "Time granularity of the chart's horizontal axis: one column per unit.
Use \"day\" for day-by-day resolution, \"week\" or \"month\" for coarser
overviews of long projects."
  :type '(choice (const "day") (const "week") (const "month") (const "quarter"))
  :group 'nowis/gantt)

(defcustom nowis/gantt-chart-width 1200
  "Pixel width of the chart column.  Wider = more room for a day scale."
  :type 'integer :group 'nowis/gantt)

(defcustom nowis/gantt-default-effort "1d"
  "Effort used for a task headline that has none."
  :type 'string :group 'nowis/gantt)

(defcustom nowis/gantt-open-function #'browse-url
  "Function used to open the generated HTML report."
  :type 'function :group 'nowis/gantt)

(defcustom nowis/gantt-timezone "Asia/Shanghai"
  "Timezone name written into the tj3 project header."
  :type 'string :group 'nowis/gantt)

(defcustom nowis/gantt-project-header ""
  "Extra tj3 statements injected verbatim into the `project' block.
Use this to model YOUR real working rhythm rather than tj3's default
calendar (Mon-Fri, 8h/day).  For example, to reflect 柳比歇夫's idea of a
few hours of pure work per day plus holidays:

  (setq nowis/gantt-project-header
        (concat \"dailyworkinghours 4\\n\"
                \"workinghours mon - fri 9:00 - 13:00\\n\"
                \"workinghours sat, sun off\\n\"
                \"vacation \\\"\u56fd\u5e86\\\" 2026-10-01 - 2026-10-07\\n\"))

Note: with `dailyworkinghours N', tj3 treats 1d of effort as N hours, so
this changes how :Effort: days map onto the calendar.  Empty = tj3
defaults."
  :type 'string :group 'nowis/gantt)

;;;; ---- Helpers -------------------------------------------------------------

(defun nowis/gantt--effort-to-tj (effort-str)
  "Turn an org EFFORT string into a tj3 effort string.
tj3 shares org's unit vocabulary (min/h/d/w) and, like org project
planning, treats `d'/`w' as WORKING days/weeks -- so a bare \"5d\" or
\"1w\" is passed through verbatim.  Clock-style H:MM efforts are
converted to minutes."
  (cond
   ((or (null effort-str) (string-empty-p effort-str)) nowis/gantt-default-effort)
   ;; already a tj3-friendly unit token: pass through (5d, 1w, 4h, 90min)
   ((string-match-p "\\`[0-9.]+\\s-*\\(min\\|h\\|d\\|w\\|m\\|y\\)\\'"
                    (string-trim effort-str))
    (replace-regexp-in-string "\\s-+" "" (string-trim effort-str)))
   ;; H:MM (or MM) clock form -> minutes
   (t (let ((min (ignore-errors (org-duration-to-minutes effort-str))))
        (if (and min (> min 0)) (format "%dmin" (round min))
          nowis/gantt-default-effort)))))

(defun nowis/gantt--id (h)
  "tj3-safe id for headline H: its :ID: property, else its buffer position.
Using :begin gives a stable, state-free synthetic id (no counter needed)."
  (let ((raw (org-element-property :ID h)))
    (if (and raw (not (string-empty-p raw)))
        (let ((s (replace-regexp-in-string "[^A-Za-z0-9_]" "_" raw)))
          (if (string-match-p "\\`[A-Za-z]" s) s (concat "t_" s)))
      (format "t_%d" (org-element-property :begin h)))))

(defun nowis/gantt--tj-time (time &optional with-time)
  "Format an Emacs TIME as a tj3 date, optionally with HH:MM."
  (format-time-string (if with-time "%Y-%m-%d-%H:%M" "%Y-%m-%d") time))

(defun nowis/gantt--name (h)
  "Display name for headline H.  `:raw-value' already has the TODO keyword
and tags stripped by org-element, so we only drop stats cookies and
escape quotes -- no hard-coded keyword list to maintain."
  (let ((s (or (org-element-property :raw-value h) "task")))
    (setq s (replace-regexp-in-string "\\[[0-9]*[/%][0-9]*\\]" "" s))
    (replace-regexp-in-string "\"" "'" (string-trim s))))

;;;; ---- Extraction ----------------------------------------------------------

(defun nowis/gantt--children (h)
  "Direct child headlines of headline H (one level deeper)."
  (let ((lvl (org-element-property :level h)))
    (seq-filter (lambda (el)
                  (and (eq (org-element-type el) 'headline)
                       (= (org-element-property :level el) (1+ lvl))))
                (org-element-contents h))))

(defun nowis/gantt--own-bookings (h)
  "H's own CLOCK intervals as (START . MINUTES), not descending into kids."
  (org-element-map (org-element-contents h) 'clock
    (lambda (c)
      (let* ((v (org-element-property :value c))
             (s (and v (org-timestamp-to-time v)))
             (e (and v (org-timestamp-to-time v t)))
             (min (and s e (round (/ (float-time (time-subtract e s)) 60.0)))))
        (and min (> min 0) (cons s min))))
    nil nil 'headline))

(defun nowis/gantt--make-children (parent)
  "Build task plists for PARENT's direct child headlines.
If PARENT carries :ORDERED: t, each child implicitly depends on its
previous sibling (sequential), reproducing org's ORDERED / TaskJuggler
semantics.  A child may also opt in per-task with :DEPENDS: previous."
  (let ((ordered (org-element-property :ORDERED parent))
        (prev-id nil) (out '()))
    (dolist (k (nowis/gantt--children parent) (nreverse out))
      (let ((node (nowis/gantt--task-node k prev-id ordered)))
        (push node out)
        (setq prev-id (plist-get node :id))))))

(defun nowis/gantt--task-node (h &optional prev-id ordered)
  "Return a task plist for headline H, recursively including :children.
PREV-ID is the id of the preceding sibling (for ORDERED / `previous').
ORDERED non-nil means the parent requested sequential siblings.
A task WITH children is a container: its own effort/booking are dropped;
tj3 derives its schedule from the children (TaskJuggler WBS semantics)."
  (let* ((depraw (org-element-property :DEPENDS h))
         ;; split declared depends; translate the literal `previous' into
         ;; the real previous-sibling id.
         (declared (and depraw (split-string depraw "[ ,]+" t)))
         (declared (delq nil
                         (mapcar (lambda (d)
                                   (if (member d '("previous" "previous-sibling"))
                                       prev-id d))
                                 declared)))
         ;; ORDERED parent: add previous sibling unless already depended on.
         (depends (if (and ordered prev-id (not (member prev-id declared)))
                      (cons prev-id declared)
                    declared))
         (children (nowis/gantt--make-children h))
         (leaf (null children))
         (sched (org-element-property :scheduled h))
         ;; A leaf is a milestone iff explicitly tagged :milestone: -- a
         ;; zero-duration point carrying no effort/allocate/booking, only a
         ;; date and/or dependencies.  (We do NOT infer milestones from a
         ;; missing :Effort:, so an un-estimated task stays a normal task
         ;; and gets the default effort instead of silently vanishing.)
         (milestone (and leaf
                         (member "milestone" (org-element-property :tags h)))))
    (list :id (nowis/gantt--id h)
          :name (nowis/gantt--name h)
          :container (not leaf)
          :milestone milestone
          :effort (and leaf (not milestone)
                       (nowis/gantt--effort-to-tj (org-element-property :EFFORT h)))
          ;; org DONE -> 100% complete; else let tj3 infer from bookings/now.
          ;; `:todo-type' is org's built-in done/todo classifier, so this
          ;; tracks your `org-todo-keywords' with no hard-coded list.
          :complete (and leaf (not milestone)
                         (eq (org-element-property :todo-type h) 'done) 100)
          :depends depends
          ;; :ALLOCATE: may name several people (space/comma separated);
          ;; tj3 then shares the effort among them.  Default: the single
          ;; resource `nowis/gantt-resource-id'.
          :allocate (and leaf (not milestone)
                         (let ((a (org-element-property :ALLOCATE h)))
                           (if a (split-string a "[ ,]+" t)
                             (list nowis/gantt-resource-id))))
          :limits (and leaf (not milestone) (org-element-property :LIMITS h))
          :start (and leaf sched (org-timestamp-to-time sched))
          :bookings (and leaf (not milestone) (nowis/gantt--own-bookings h))
          :children children)))

(defun nowis/gantt--resources (tasks)
  "Return the de-duplicated list of resource ids used anywhere in TASKS."
  (let (ids)
    (cl-labels ((walk (tk)
                  (dolist (r (plist-get tk :allocate)) (cl-pushnew r ids :test #'equal))
                  (mapc #'walk (plist-get tk :children))))
      (mapc #'walk tasks))
    (nreverse ids)))

(defun nowis/gantt--collect (project-h)
  "Collect (META . TASKS) from a :gantt: PROJECT-H headline element.
Project start = GANTT_START, else the earliest CLOCK (past bookings must
lie inside the frame; future SCHEDULED never will), else today."
  (let* ((tasks (nowis/gantt--make-children project-h))
         (resources (nowis/gantt--resources tasks))
         (earliest (car (sort (org-element-map (org-element-contents project-h)
                                  'clock
                                (lambda (c)
                                  (org-timestamp-to-time
                                   (org-element-property :value c))))
                              #'time-less-p))))
    (cons (list :name (nowis/gantt--name project-h)
                :id (nowis/gantt--id project-h)
                :resources resources
                :start (or (org-element-property :GANTT_START project-h)
                           (and earliest (nowis/gantt--tj-time earliest))
                           (format-time-string "%Y-%m-%d"))
                :end (org-element-property :GANTT_END project-h)
                :now (org-element-property :GANTT_NOW project-h))
          tasks)))

;;;; ---- Emission ------------------------------------------------------------

(defun nowis/gantt--emit-task (tk depth)
  "Insert TK as a tj3 task at nesting DEPTH.  Containers recurse; leaves
carry the effort/booking/limits payload.  Nesting reproduces the org
headline hierarchy as TaskJuggler WBS."
  (let ((pad (make-string (* 2 depth) ?\s))
        (ind (make-string (* 2 (1+ depth)) ?\s)))
    (insert (format "%stask %s \"%s\" {\n" pad
                    (plist-get tk :id) (plist-get tk :name)))
    (when (plist-get tk :depends)
      (insert (format "%sdepends %s\n" ind
                      (mapconcat (lambda (d) (concat "!" d))
                                 (plist-get tk :depends) ", "))))
    (cond
     ;; container: no payload of its own; just nest children
     ((plist-get tk :container)
      (dolist (c (plist-get tk :children))
        (nowis/gantt--emit-task c (1+ depth))))
     ;; milestone: a zero-duration point (diamond); only an optional start
     ((plist-get tk :milestone)
      (insert (format "%smilestone\n" ind))
      (when (plist-get tk :start)
        (insert (format "%sstart %s-09:00\n" ind
                        (nowis/gantt--tj-time (plist-get tk :start))))))
     ;; ordinary leaf task
     (t
        (insert (format "%seffort %s\n" ind (plist-get tk :effort)))
        (insert (format "%sallocate %s\n" ind
                        (mapconcat #'identity (plist-get tk :allocate) ", ")))
        (when (plist-get tk :complete)
          (insert (format "%scomplete %d\n" ind (plist-get tk :complete))))
        (when (plist-get tk :limits)
          (insert (format "%slimits { %s }\n" ind (plist-get tk :limits))))
        (when (and (plist-get tk :start) (null (plist-get tk :bookings)))
          (insert (format "%sstart %s-09:00\n" ind
                          (nowis/gantt--tj-time (plist-get tk :start)))))
        (dolist (bk (plist-get tk :bookings))
          (insert (format "%sbooking %s %s +%dmin\n" ind
                          nowis/gantt-resource-id
                          (nowis/gantt--tj-time (car bk) t)
                          (cdr bk))))))
    (insert (format "%s}\n" pad))))

(defun nowis/gantt--emit (meta tasks)
  "Return the .tjp text for META and TASKS."
  (let* ((start (or (plist-get meta :start)
                    (format-time-string "%Y-%m-%d")))
         (end (plist-get meta :end))
         (now (plist-get meta :now))
         (name (or (plist-get meta :name) "Project"))
         (pid (plist-get meta :id)))
    (with-temp-buffer
      (insert (format "project %s \"%s\" %s %s {\n"
                      pid name start
                      (if end (format "- %s" end) "+3m")))
      (insert (format "  timezone \"%s\"\n" nowis/gantt-timezone))
      (when now (insert (format "  now %s-12:00\n" now)))
      ;; user-supplied calendar / working-hours / vacations, indented
      (unless (string-empty-p (string-trim nowis/gantt-project-header))
        (dolist (line (split-string (string-trim nowis/gantt-project-header) "\n"))
          (insert (format "  %s\n" (string-trim-right line)))))
      (insert "}\n\n")
      ;; Declare every resource used by the tasks.  Names come from
      ;; `nowis/gantt-resource-names', with the default id mapped to
      ;; `nowis/gantt-resource-name'; unknown ids use the id as the name.
      (dolist (rid (or (plist-get meta :resources)
                       (list nowis/gantt-resource-id)))
        (insert (format "resource %s \"%s\"\n" rid
                        (or (cdr (assoc rid nowis/gantt-resource-names))
                            (and (equal rid nowis/gantt-resource-id)
                                 nowis/gantt-resource-name)
                            rid))))
      (insert "\n")
      (dolist (tk tasks) (nowis/gantt--emit-task tk 0))
      (insert "\ntaskreport gantt \"gantt\" {\n")
      (insert "  formats html\n")
      ;; effort = pure work (纯时间); duration = calendar span.  When a task
      ;; carries `limits { dailymax .. }' (每天对半) the two diverge, which
      ;; is how the limit becomes visible in the report beside the chart.
      ;; A cellcolor paints the name of DONE (100%%) tasks for a glanceable
      ;; completion cue.
      ;; The chart column takes a `scale' (day/week/month) and width so the
      ;; axis granularity is one column per unit (day-by-day by default).
      (insert (format "  columns bsi, name%s, start, end, effort, duration, complete, chart { scale %s width %d }\n"
                      (if nowis/gantt-done-color
                          (format " { cellcolor plan.complete = 100.0 \"%s\" }"
                                  nowis/gantt-done-color)
                        "")
                      nowis/gantt-scale
                      nowis/gantt-chart-width))
      (insert "}\n")
      (buffer-string))))

;;;; ---- Driver --------------------------------------------------------------

(defun nowis/gantt--enclosing-project ()
  "Return the org-element headline of the enclosing :gantt: subtree, or nil."
  (save-excursion
    (let (found)
      (while (and (not found) (org-up-heading-safe))
        (when (member nowis/gantt-project-tag (org-get-tags nil t))
          (setq found (point))))
      ;; also check current heading
      (unless found
        (org-back-to-heading t)
        (when (member nowis/gantt-project-tag (org-get-tags nil t))
          (setq found (point))))
      (when found
        (goto-char found)
        (org-element-at-point)))))

;;;###autoload
(defun nowis/gtd-gantt ()
  "Project the enclosing :gantt: subtree into tj3 and open the Gantt HTML."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org buffer"))
  (let ((ph (nowis/gantt--enclosing-project)))
    (unless ph
      (user-error "Point is not inside a headline tagged :%s:"
                  nowis/gantt-project-tag))
    ;; re-parse the subtree fully (org-element-at-point is shallow)
    (let* ((beg (org-element-property :begin ph))
           (end (org-element-property :end ph))
           (subtree (save-restriction
                      (narrow-to-region beg end)
                      (org-element-map (org-element-parse-buffer) 'headline
                        #'identity nil t)))
           (cell (nowis/gantt--collect subtree))
           (meta (car cell))
           (tasks (cdr cell)))
      (unless tasks (user-error "No task headlines found under :%s: subtree"
                                nowis/gantt-project-tag))
      (make-directory nowis/gantt-output-dir t)
      (let* ((base (concat "gtd-" (plist-get meta :id)))
             (tjp (expand-file-name (concat base ".tjp") nowis/gantt-output-dir))
             (html (expand-file-name "gantt.html" nowis/gantt-output-dir)))
        (let ((coding-system-for-write 'utf-8))
          (with-temp-file tjp (insert (nowis/gantt--emit meta tasks))))
        (let* ((default-directory nowis/gantt-output-dir)
               (out (with-output-to-string
                      (with-current-buffer standard-output
                        (call-process nowis/gantt-command nil t nil
                                      (file-name-nondirectory tjp))))))
          (if (string-match-p "Error:" out)
              (progn
                (with-current-buffer (get-buffer-create "*tj3 output*")
                  (erase-buffer) (insert out) (display-buffer (current-buffer)))
                (user-error "tj3 reported errors; see *tj3 output*"))
            (if (not (file-exists-p html))
                (user-error "tj3 ran but no %s produced" html)
              (if (y-or-n-p (format "Gantt built: %s  Open it? " html))
                  (funcall nowis/gantt-open-function html)
                (message "Gantt: %s" html)))))))))

(provide 'nowis-gantt)
;;; nowis-gantt.el ends here
