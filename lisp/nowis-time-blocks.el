;;; nowis-time-blocks.el --- Daily time-block SVG: plan vs actual -*- lexical-binding: t -*-

;; Author: Lewis Liu
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: org, clock, time, svg

;;; Commentary:
;;
;; A single-day, two-column SVG time chart inspired by 《奇特的一生》
;; (event-time logging) and 《把时间当朋友》 (calibrate your sense of time).
;;
;; Left band  = plan   : headlines with a timed SCHEDULED, height = EFFORT.
;; Main column= actual : every CLOCK interval of the day (incl. unplanned).
;;
;; Actual blocks are BLUE when they match a planned task, ORANGE when
;; unplanned.  If a matched task has an EFFORT estimate, the deviation
;; (actual vs estimate) is printed beside the block — the whole point of
;; both books: make the gap between what-you-thought and what-happened
;; visible.
;;
;; Data comes entirely from `org-element' (no regexps).  All rectangles
;; are drawn by one helper `nowis/tb--block'; only colours differ.
;;
;;   M-x nowis/time-blocks         → today
;;   C-u M-x nowis/time-blocks     → pick a date

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-duration)
(require 'svg)
(require 'cl-lib)

;;;; ---- Customisation -------------------------------------------------------

(defgroup nowis/time-blocks nil
  "Daily plan-vs-actual time-block chart."
  :group 'org)

(defcustom nowis/tb-px-per-min 0.5
  "Vertical pixels per minute (0.5 = 30px/hour, ~720px for a full day)."
  :type 'number :group 'nowis/time-blocks)

(defcustom nowis/tb-day-start-hour 0
  "First hour shown on the daily grid (0-23)."
  :type 'integer :group 'nowis/time-blocks)

(defcustom nowis/tb-day-end-hour 24
  "Last hour shown on the daily grid (1-24). 24 means end of day."
  :type 'integer :group 'nowis/time-blocks)

(defcustom nowis/tb-grid-minutes 30
  "Grid line spacing in minutes (e.g. 30 for half-hour, 60 for hourly)."
  :type 'integer :group 'nowis/time-blocks)

(defcustom nowis/tb-plan-color "#4c78c8"
  "Colour for planned (blue) blocks."
  :type 'string :group 'nowis/time-blocks)

(defcustom nowis/tb-unplanned-color "#e08a3c"
  "Colour for unplanned (orange) actual blocks."
  :type 'string :group 'nowis/time-blocks)

(defcustom nowis/tb-default-effort-min nil
  "Fabricated effort height when a plan has none.
Nil means draw a thin marker instead of inventing a height."
  :type '(choice (const :tag "thin marker" nil) number)
  :group 'nowis/time-blocks)

(defcustom nowis/tb-files-function #'nowis/tb--default-files
  "Function returning the list of org files to scan.
Defaults to every .org under the gtd directory (including the
datetree archive) so that clock records of already-archived tasks
still show up when viewing past days."
  :type 'function :group 'nowis/time-blocks)

(defun nowis/tb--default-files ()
  "All files under the gtd directory (unified source)."
  (if (fboundp 'nowis/gtd-files)
      (nowis/gtd-files)
    (org-agenda-files)))

;;;; ---- Geometry constants --------------------------------------------------

(defconst nowis/tb--top 24)      ; top padding
(defconst nowis/tb--bot 16)      ; bottom padding
(defconst nowis/tb--axis-x 48)   ; x where time labels sit
(defconst nowis/tb--plan-x 52)   ; left plan column (now wide, labelled)
(defconst nowis/tb--plan-w 150)
(defconst nowis/tb--main-x 214)  ; main actual column
(defconst nowis/tb--main-w 300)
(defconst nowis/tb--dev-x 522)   ; deviation text column
(defconst nowis/tb--width 600)

;;;; ---- Data extraction (org-element, no regexps) ---------------------------

(defun nowis/tb--same-day-p (time day)
  "Non-nil if TIME falls on DAY (a `decode-time'-style day list from
`org-read-date' → encoded as (SEC MIN HOUR DAY MONTH YEAR ...))."
  (and time
       (equal (format-time-string "%F" time)
              (format-time-string "%F" day))))

(defun nowis/tb--minutes (time day-start)
  "Minutes from DAY-START (a time) to TIME."
  (/ (float-time (time-subtract time day-start)) 60.0))

(defun nowis/tb--collect (day)
  "Return (PLANS . ACTUALS) for DAY across `org-agenda-files'.
Each item is a plist: :title :start :end :effort (minutes or nil)."
  (let ((plans '()) (actuals '()))
    (dolist (file (funcall nowis/tb-files-function))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (let ((tree (org-element-parse-buffer)))
             ;; Left: timed SCHEDULED headlines.
             (org-element-map tree 'headline
               (lambda (h)
                 (let ((s (org-element-property :scheduled h)))
                   (when (and s (org-element-property :hour-start s))
                     (let ((start (org-timestamp-to-time s)))
                       (when (nowis/tb--same-day-p start day)
                         (let ((eff (org-element-property :EFFORT h)))
                           (push (list :title (org-element-property :raw-value h)
                                       :start start
                                       :effort (and eff (org-duration-to-minutes eff)))
                                 plans))))))))
             ;; Right: every CLOCK interval.
             (org-element-map tree 'clock
               (lambda (c)
                 (let* ((v (org-element-property :value c))
                        (start (org-timestamp-to-time v))
                        (end (org-timestamp-to-time v t)))
                   (when (and start end (nowis/tb--same-day-p start day))
                     (let ((head (org-element-lineage c '(headline) t)))
                       (push (list :title (and head (org-element-property :raw-value head))
                                   :start start :end end)
                             actuals)))))))))))
    ;; order is irrelevant: blocks are placed at absolute coordinates.
    (cons plans actuals)))

;;;; ---- Drawing -------------------------------------------------------------

(defun nowis/tb--block (svg x y w h color &optional label sublabel)
  "Draw one rounded rect (the ONLY block primitive) with optional texts."
  (svg-rectangle svg x y w (max h 2) :rx 3 :fill color :fill-opacity 0.85)
  (when (and label (> h 12))
    (svg-text svg (nowis/tb--clip label 22)
              :x (+ x 5) :y (+ y 13) :font-size 11 :fill "white"))
  (when (and sublabel (> h 26))
    (svg-text svg sublabel
              :x (+ x 5) :y (+ y 25) :font-size 10 :fill "#eeeeee")))

(defun nowis/tb--clip (s n)
  (if (> (length s) n) (concat (substring s 0 (1- n)) "…") s))

(defun nowis/tb--norm (title)
  "Normalise TITLE for matching: drop TODO word, leading [tag], stats, ws."
  (when title
    (let ((s title))
      (setq s (replace-regexp-in-string
               "^\\(TODO\\|DONE\\|WAIT\\|SMDY\\|AXED\\)\\s-+" "" s))
      (setq s (replace-regexp-in-string "^\\[[^]]*\\]\\s-*" "" s))
      (setq s (replace-regexp-in-string "\\[[0-9]*[/%][0-9]*\\]" "" s))
      (string-trim s))))

(defun nowis/tb--planned-p (title plans)
  "Return the plan plist matching TITLE (normalised), or nil."
  (and title
       (let ((key (nowis/tb--norm title)))
         (cl-find-if (lambda (p) (equal (nowis/tb--norm (plist-get p :title)) key))
                     plans))))

(defun nowis/tb--deviation (actual-min effort-min)
  "Return a plain-language deviation string vs the EFFORT estimate.
\"多用 20 分\" when over, \"少用 15 分\" when under, nil when equal or no estimate."
  (when (and effort-min (> effort-min 0))
    (let ((diff (round (- actual-min effort-min))))
      (unless (zerop diff)
        (format "%s %d 分" (if (> diff 0) "多用" "少用") (abs diff))))))

(defun nowis/tb--svg (day plans actuals)
  "Build the SVG image for DAY from PLANS and ACTUALS.
Always draws a full-day grid from `nowis/tb-day-start-hour' to
`nowis/tb-day-end-hour' regardless of whether tasks exist."
  (let* ((d (decode-time day))
         (day-start (encode-time 0 0 nowis/tb-day-start-hour
                                 (nth 3 d) (nth 4 d) (nth 5 d)))
         (day-end (time-add day-start
                            (* 3600 (- nowis/tb-day-end-hour
                                       nowis/tb-day-start-hour))))
         (span (/ (float-time (time-subtract day-end day-start)) 60.0)) ; min
         (y-of (lambda (tm)
                 (+ nowis/tb--top
                    (* nowis/tb-px-per-min
                       (max 0 (min span (nowis/tb--minutes tm day-start)))))))
         (height (+ nowis/tb--top nowis/tb--bot
                    (* nowis/tb-px-per-min span)))
         (svg (svg-create nowis/tb--width height :stroke-width 1)))
    (svg-rectangle svg 0 0 nowis/tb--width height :fill "#1e1e2e")
    ;; column separators (plan band | main column)
    (svg-line svg nowis/tb--axis-x nowis/tb--top nowis/tb--axis-x
              (- height nowis/tb--bot) :stroke "#3a3a4a")
    (svg-line svg (- nowis/tb--main-x 6) nowis/tb--top (- nowis/tb--main-x 6)
              (- height nowis/tb--bot) :stroke "#3a3a4a")
    ;; time grid: a line every `nowis/tb-grid-minutes'; on-the-hour lines
    ;; are brighter and labelled.
    (let ((tm day-start))
      (while (<= (float-time tm) (float-time day-end))
        (let* ((dc (decode-time tm))
               (on-hour (= (nth 1 dc) 0))
               (y (funcall y-of tm)))
          (svg-line svg nowis/tb--axis-x y nowis/tb--width y
                    :stroke (if on-hour "#45475a" "#2f3042"))
          (when on-hour
            (svg-text svg (format-time-string "%H:%M" tm)
                      :x 4 :y (+ y 4) :font-size 10 :fill "#8a8a9a")))
        (setq tm (time-add tm (* 60 nowis/tb-grid-minutes)))))
    ;; plan column (left): now wide, always labelled with time + title.
    (dolist (p plans)
      (let* ((y (funcall y-of (plist-get p :start)))
             (eff (or (plist-get p :effort) nowis/tb-default-effort-min))
             (h (if eff (* nowis/tb-px-per-min eff) 3)))
        (svg-rectangle svg nowis/tb--plan-x y nowis/tb--plan-w (max h 2)
                       :rx 3 :fill nowis/tb-plan-color :fill-opacity 0.55)
        ;; label sits beside the top of the block (works even for thin ones)
        (svg-text svg (format "%s %s"
                              (format-time-string "%H:%M" (plist-get p :start))
                              (nowis/tb--clip (nowis/tb--norm (plist-get p :title)) 12))
                  :x (+ nowis/tb--plan-x 4) :y (+ y 11)
                  :font-size 10 :fill "#cdd6f4")))
    ;; Aggregate clock segments per task (normalised title):
    ;;   :total = sum of all segment minutes, :first = earliest start (secs).
    (let ((aggs (make-hash-table :test 'equal)))
      (dolist (a actuals)
        (let* ((k (nowis/tb--norm (plist-get a :title)))
               (m (nowis/tb--minutes (plist-get a :end) (plist-get a :start)))
               (s (float-time (plist-get a :start)))
               (cur (gethash k aggs)))
          (puthash k (list :total (+ m (or (plist-get cur :total) 0))
                           :first (min s (or (plist-get cur :first) s)))
                   aggs)))
    ;; actual blocks (main) + deviation + connector to plan.
    ;; Blocks occupy the full main column: with honest clocking (one task
    ;; clocked at a time) intervals never overlap, so no side-by-side layout
    ;; is needed.
    (dolist (a actuals)
      (let* ((start (plist-get a :start)) (end (plist-get a :end))
             (y (funcall y-of start))
             (mins (nowis/tb--minutes end start))
             (h (* nowis/tb-px-per-min mins))
             (bx nowis/tb--main-x)
             (bw nowis/tb--main-w)
             (plan (nowis/tb--planned-p (plist-get a :title) plans))
             (color (if plan nowis/tb-plan-color nowis/tb-unplanned-color))
             (dur (org-duration-from-minutes mins))
             (key (nowis/tb--norm (plist-get a :title)))
             (agg (and key (gethash key aggs)))
             ;; annotate only on this task's FIRST (earliest) segment
             (firstp (and agg (= (float-time start)
                                 (plist-get agg :first))))
             ;; deviation uses TOTAL clocked minutes across all segments;
             ;; lateness (start delay) is left to the connector line's slope.
             (dev (and plan firstp
                       (nowis/tb--deviation (plist-get agg :total)
                                            (plist-get plan :effort)))))
        ;; connector: link EVERY segment of a planned task back to its plan
        ;; block, so multi-segment clocks are visibly grouped to one SCHEDULED.
        (when plan
          (let* ((py (funcall y-of (plist-get plan :start)))
                 (px (+ nowis/tb--plan-x nowis/tb--plan-w))
                 (mx bx)
                 (mid (/ (+ px mx) 2)))
            ;; elbow path: plan-edge → mid → down/up → segment left edge
            (svg-line svg px py mid py :stroke color :stroke-width 1.5)
            (svg-line svg mid py mid y :stroke color :stroke-width 1.5)
            (svg-line svg mid y mx y :stroke color :stroke-width 1.5)
            ;; dot on the plan block marks the shared origin of the segments
            (svg-circle svg px py 2 :fill color)))
        (nowis/tb--block svg bx y bw h
                         color (plist-get a :title) dur)
        ;; deviation vs estimate (plain language) — once per task.
        ;; "多用" = over (warm/warning), "少用" = under (green).
        (when dev
          (svg-text svg dev :x nowis/tb--dev-x :y (+ y 11)
                    :font-size 10 :fill (if (string-prefix-p "多用" dev)
                                            nowis/tb-unplanned-color "#8fd694"))))))
    ;; now line (only if DAY is today)
    (when (nowis/tb--same-day-p (current-time) day)
      (let ((y (funcall y-of (current-time))))
        (when (and (>= y nowis/tb--top) (<= y (- height nowis/tb--bot)))
          (svg-line svg nowis/tb--axis-x y nowis/tb--width y
                    :stroke "#e05a5a" :stroke-dasharray "4 3"))))
    svg))

;;;; ---- Command / mode ------------------------------------------------------

(defvar-local nowis/tb--day nil "Day currently displayed in this buffer.")

(define-derived-mode nowis/tb-mode special-mode "TimeBlocks"
  "Major mode for the daily time-block chart.")

(define-key nowis/tb-mode-map (kbd "g") #'nowis/tb-refresh)

(defun nowis/tb-refresh ()
  "Re-render the chart for the buffer's current day."
  (interactive)
  (nowis/time-blocks nowis/tb--day))

;;;###autoload
(defun nowis/time-blocks (&optional day)
  "Show the daily plan-vs-actual time-block chart.
With `\\[universal-argument]' prefix, or when DAY is nil interactively
after a prefix, prompt for a date.  DAY is an Emacs time value."
  (interactive
   (list (if current-prefix-arg
             (org-read-date nil t nil "选择日期")
           (current-time))))
  (let* ((day (or day (current-time)))
         (cell (nowis/tb--collect day))
         (svg (nowis/tb--svg day (car cell) (cdr cell)))
         (buf (get-buffer-create "*Time Blocks*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'nowis/tb-mode) (nowis/tb-mode))
      (setq nowis/tb--day day)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "  %s  计划(左·蓝) vs 实际(右)  ·  橙=计划外  ·  g 刷新\n\n"
                        (format-time-string "%Y-%m-%d %a" day)))
        (svg-insert-image svg)))
    (pop-to-buffer buf)))

(provide 'nowis-time-blocks)
;;; nowis-time-blocks.el ends here
