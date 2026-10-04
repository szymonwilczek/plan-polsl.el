;;; plan-polsl-events.el --- Academic events from a shared Org file -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, org

;;; Commentary:
;; Events such as rector's hours, days off, tests and exams are kept in
;; one Org file chosen by the user, typically in a synchronized
;; repository, so every device sees the same events.
;; Each event is a heading tagged with its type, with an active
;; timestamp and optional PRZEDMIOT (course) and SALA (room) properties:
;;
;;   * Kolokwium 1 :kolokwium:
;;   :PROPERTIES:
;;   :PRZEDMIOT: Analiza danych i inteligencja obliczeniowa
;;   :SALA: 416b
;;   :END:
;;   <2026-11-19 czw 14:00-16:15>
;;
;; Rector's and dean's hours and days off cancel the classes they
;; overlap; the other types are shown next to the classes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'time-date)

(declare-function org-read-date "org")
(declare-function plan-polsl-view--show-week "plan-polsl-view")
(defvar plan-polsl-view-entries)
(defvar plan-polsl-view-active-monday)

(defcustom plan-polsl-events-file nil
  "Org file holding academic events, such as tests and days off.
Keep it in a synchronized place, such as a dotfiles repository, to
share the events between devices."
  :type '(choice (const :tag "Not Set" nil) file)
  :group 'plan-polsl)

(defconst plan-polsl-events-types
  '(("rektorskie" "Godziny rektorskie" t)
    ("dziekanskie" "Godziny dziekańskie" t)
    ("wolne" "Dzień wolny" t)
    ("kolokwium" "Kolokwium" nil)
    ("egzamin" "Egzamin" nil)
    ("projekt" "Projekt" nil)
    ("odrabianie" "Odrabianie" nil)
    ("inne" "Wydarzenie" nil))
  "Event types as (TAG LABEL CANCELS-CLASSES).
Labels of types shown as timetable lines fit the 12 column badge.")

(defun plan-polsl-events-type-label (type)
  "Return the display label of event TYPE tag."
  (or (nth 1 (assoc type plan-polsl-events-types)) type))

(defun plan-polsl-events-cancelling-p (event)
  "Return non-nil when EVENT cancels the classes it overlaps."
  (nth 2 (assoc (plist-get event :type) plan-polsl-events-types)))

(defconst plan-polsl-events--timestamp-re
  (concat "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[^>0-9]*"
          "\\(?:\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)"
          "\\(?:-\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\)?\\)?[^>]*>")
  "Regexp matching an active Org timestamp with optional time range.")

(defconst plan-polsl-events--heading-re
  "^\\(\\*+\\)[ \t]+\\(.*?\\)[ \t]+:\\(\\(?:[^ \t\n:]+:\\)+\\)[ \t]*$"
  "Regexp matching a tagged Org heading.")

(defun plan-polsl-events--pad-time (time)
  "Return TIME \"h:mm\" as \"hh:mm\", or nil."
  (and time (if (= (length time) 4) (concat "0" time) time)))

(defun plan-polsl-events--parse-timestamp (text)
  "Parse the first active timestamp or range in TEXT.
Return (:start DATE :end DATE :from TIME :to TIME :ts-beg N :ts-end N),
with dates as \"YYYY-MM-DD\" and times as \"hh:mm\" or nil."
  (when (string-match plan-polsl-events--timestamp-re text)
    (let ((beg (match-beginning 0))
          (end (match-end 0))
          (start (match-string 1 text))
          (from (plan-polsl-events--pad-time (match-string 2 text)))
          (to (plan-polsl-events--pad-time (match-string 3 text))))
      (if (eq (string-match (concat "--" plan-polsl-events--timestamp-re) text end) end)
          (list :start start :end (match-string 1 text)
                :from from
                :to (plan-polsl-events--pad-time (match-string 2 text))
                :ts-beg beg :ts-end (match-end 0))
        (list :start start :end start :from from :to to
              :ts-beg beg :ts-end end)))))

(defun plan-polsl-events--property (section name)
  "Return the value of property NAME in Org SECTION text, or nil."
  (when (string-match (format "^[ \t]*:%s:[ \t]*\\(.*?\\)[ \t]*$" name) section)
    (let ((value (match-string 1 section)))
      (unless (string-empty-p value) value))))

(defun plan-polsl-events-parse-buffer ()
  "Return the events in the current Org buffer, in buffer order.
Each event is a plist with :type, :title, :start, :end, :from, :to,
:course, :room and the buffer positions :pos and :end-pos of its
heading and section. Headings without an event type tag or without a
timestamp are skipped."
  (save-excursion
    (goto-char (point-min))
    (let ((events nil))
      (while (re-search-forward plan-polsl-events--heading-re nil t)
        (let* ((pos (match-beginning 0))
               (level (length (match-string 1)))
               (title (match-string-no-properties 2))
               (tags (split-string (match-string-no-properties 3) ":" t))
               (type (cl-find-if (lambda (tag) (assoc tag plan-polsl-events-types)) tags))
               (body-beg (min (point-max) (1+ (line-end-position))))
               (end-pos (save-excursion
                          (if (re-search-forward "^\\*+[ \t]" nil t)
                              (match-beginning 0)
                            (point-max))))
               (section (buffer-substring-no-properties body-beg end-pos))
               (ts (plan-polsl-events--parse-timestamp section)))
          (when (and type ts)
            (push (list :type type
                        :title title
                        :start (plist-get ts :start)
                        :end (plist-get ts :end)
                        :from (plist-get ts :from)
                        :to (plist-get ts :to)
                        :course (plan-polsl-events--property section "PRZEDMIOT")
                        :room (plan-polsl-events--property section "SALA")
                        :level level
                        :pos pos
                        :end-pos end-pos)
                  events))))
      (nreverse events))))

(defvar plan-polsl-events--cache nil
  "Cached events as (FILE MTIME EVENTS).")

(defun plan-polsl-events-list ()
  "Return all events from `plan-polsl-events-file', sorted by start.
The file is read again only when its modification time changed, for
example after pulling the repository holding it."
  (let ((file (and plan-polsl-events-file
                   (expand-file-name plan-polsl-events-file))))
    (when (and file (file-readable-p file))
      (let ((mtime (file-attribute-modification-time (file-attributes file))))
        (unless (and (equal (nth 0 plan-polsl-events--cache) file)
                     (equal (nth 1 plan-polsl-events--cache) mtime))
          (setq plan-polsl-events--cache
                (list file mtime
                      (with-temp-buffer
                        (insert-file-contents file)
                        (sort (plan-polsl-events-parse-buffer)
                              (lambda (a b)
                                (string< (concat (plist-get a :start) (or (plist-get a :from) ""))
                                         (concat (plist-get b :start) (or (plist-get b :from) "")))))))))
        (nth 2 plan-polsl-events--cache)))))

(defun plan-polsl-events-on-date (date &optional events)
  "Return EVENTS (default all) taking place on DATE \"YYYY-MM-DD\"."
  (cl-remove-if-not (lambda (ev)
                      (and (not (string< date (plist-get ev :start)))
                           (not (string< (plist-get ev :end) date))))
                    (or events (plan-polsl-events-list))))

(defun plan-polsl-events-hours-on (event date)
  "Return (FROM . TO) hours EVENT occupies on DATE.
A day of a multi-day event without hours lasts from \"00:00\" to
\"24:00\"; hours apply only to its first and last day."
  (cons (or (and (equal date (plist-get event :start)) (plist-get event :from))
            "00:00")
        (or (and (equal date (plist-get event :end)) (plist-get event :to))
            "24:00")))

(defun plan-polsl-events-cancelling (date start end &optional events)
  "Return the first cancelling event overlapping DATE from START to END.
START and END are \"hh:mm\"; EVENTS default to all events."
  (cl-find-if (lambda (ev)
                (and (plan-polsl-events-cancelling-p ev)
                     (let ((hours (plan-polsl-events-hours-on ev date)))
                       (and (string< start (cdr hours))
                            (string< (car hours) end)))))
              (plan-polsl-events-on-date date events)))

(defconst plan-polsl-events--day-abbrevs
  ["nie" "pon" "wto" "śro" "czw" "pią" "sob"]
  "Day names for Org timestamps, indexed by `decode-time' weekday.")

(defun plan-polsl-events--date-stamp (date &optional from to)
  "Return an active Org timestamp for DATE with optional FROM-TO hours."
  (pcase-let ((`(,y ,m ,d) (mapcar #'string-to-number (split-string date "-"))))
    (format "<%s %s%s>"
            date
            (aref plan-polsl-events--day-abbrevs
                  (nth 6 (decode-time (encode-time 0 0 12 d m y))))
            (cond ((and from to) (format " %s-%s" from to))
                  (from (concat " " from))
                  (t "")))))

(defun plan-polsl-events-timestamp (event)
  "Return the Org timestamp or range of EVENT."
  (let ((start (plist-get event :start))
        (end (plist-get event :end))
        (from (plist-get event :from))
        (to (plist-get event :to)))
    (if (equal start end)
        (plan-polsl-events--date-stamp start from to)
      (concat (plan-polsl-events--date-stamp start from)
              "--"
              (plan-polsl-events--date-stamp end to)))))

(defun plan-polsl-events--key (event)
  "Return the fields identifying EVENT in its file."
  (mapcar (lambda (k) (plist-get event k))
          '(:type :title :start :end :from :to :course :room)))

(defun plan-polsl-events--format (event &optional level props body)
  "Return the Org text of EVENT as a heading of LEVEL (default 1).
PROPS are further property lines and BODY further text kept from an
earlier version of the event."
  (let ((drawer (append
                 (when-let* ((course (plist-get event :course)))
                   (list (format ":PRZEDMIOT: %s" course)))
                 (when-let* ((room (plist-get event :room)))
                   (list (format ":SALA: %s" room)))
                 props)))
    (concat (make-string (or level 1) ?*) " "
            (plist-get event :title) " :" (plist-get event :type) ":
"
            (if drawer
                (concat ":PROPERTIES:
" (mapconcat #'identity drawer "
") "
:END:
")
              "")
            (plan-polsl-events-timestamp event) "
"
            (if (and body (not (string-empty-p body))) (concat body "
") "")
            "
")))

(defun plan-polsl-events--leftovers (section)
  "Return (PROPS . BODY) of event SECTION text not managed by this package.
PROPS are property lines other than PRZEDMIOT and SALA, BODY the text
left after removing the property drawer and the event timestamp."
  (with-temp-buffer
    (insert section)
    (let ((props nil))
      (goto-char (point-min))
      (when-let* ((ts (plan-polsl-events--parse-timestamp (buffer-string))))
        (delete-region (1+ (plist-get ts :ts-beg)) (1+ (plist-get ts :ts-end)))
        (goto-char (1+ (plist-get ts :ts-beg)))
        (if (string-blank-p (buffer-substring (line-beginning-position) (line-end-position)))
            (delete-region (line-beginning-position)
                           (min (point-max) (1+ (line-end-position))))
          (delete-region (point) (progn (skip-chars-forward " \t") (point)))))
      (goto-char (point-min))
      (when (re-search-forward "^[ 	]*:PROPERTIES:[ 	]*
" nil t)
        (let ((beg (match-beginning 0)))
          (while (and (not (looking-at "^[ 	]*:END:")) (not (eobp)))
            (let ((line (string-trim (buffer-substring (line-beginning-position)
                                                       (line-end-position)))))
              (unless (string-match-p "\\`:\\(PRZEDMIOT\\|SALA\\):" line)
                (push line props)))
            (forward-line 1))
          (forward-line 1)
          (delete-region beg (point))))
      (cons (nreverse props) (string-trim (buffer-string) "[
]+" "[ 	
]+")))))

(defun plan-polsl-events--before-p (a b)
  "Return non-nil when event A starts before event B."
  (string< (concat (plist-get a :start) (or (plist-get a :from) ""))
           (concat (plist-get b :start) (or (plist-get b :from) ""))))

(defun plan-polsl-events--insert (event &optional props body)
  "Insert EVENT into the current buffer, keeping events sorted by start.
PROPS and BODY are passed to `plan-polsl-events--format'."
  (let* ((events (plan-polsl-events-parse-buffer))
         (next (cl-find-if (lambda (e) (plan-polsl-events--before-p event e)) events))
         (level (plist-get (or next (car (last events))) :level)))
    (save-excursion
      (if next
          (goto-char (plist-get next :pos))
        (goto-char (point-max))
        (unless (bobp)
          (skip-chars-backward " 	
")
          (delete-region (point) (point-max))
          (insert "

")))
      (insert (plan-polsl-events--format event level props body)))))

(defun plan-polsl-events--find (event)
  "Return the event in the current buffer matching EVENT, or signal."
  (or (cl-find (plan-polsl-events--key event) (plan-polsl-events-parse-buffer)
               :key #'plan-polsl-events--key :test #'equal)
      (user-error "Nie znaleziono wydarzenia \"%s\" w pliku (zmienione w międzyczasie?)"
                  (plist-get event :title))))

(defun plan-polsl-events--file ()
  "Return the expanded events file name, or signal when unset."
  (unless plan-polsl-events-file
    (user-error "Ustaw plik wydarzeń: plan-polsl-events-file"))
  (expand-file-name plan-polsl-events-file))

(defmacro plan-polsl-events--with-file (&rest body)
  "Run BODY in a buffer visiting the events file, then save it.
A missing file is created with a title line."
  (declare (indent 0))
  `(let ((file (plan-polsl-events--file)))
     (make-directory (file-name-directory file) t)
     (with-current-buffer (find-file-noselect file)
       (when (= (buffer-size) 0)
         (insert "#+title: Wydarzenia PolSL\n\n"))
       (prog1 (progn ,@body)
         (save-buffer)))))

(defun plan-polsl-events-add (event)
  "Add EVENT to `plan-polsl-events-file'."
  (plan-polsl-events--with-file
    (plan-polsl-events--insert event)))

(defun plan-polsl-events-update (old new)
  "Replace event OLD with NEW in `plan-polsl-events-file'.
Notes and properties written under OLD by hand are kept."
  (plan-polsl-events--with-file
    (let* ((found (plan-polsl-events--find old))
           (body-beg (save-excursion
                       (goto-char (plist-get found :pos))
                       (min (point-max) (1+ (line-end-position)))))
           (rest (plan-polsl-events--leftovers
                  (buffer-substring-no-properties body-beg (plist-get found :end-pos)))))
      (delete-region (plist-get found :pos) (plist-get found :end-pos))
      (plan-polsl-events--insert new (car rest) (cdr rest)))))

(defun plan-polsl-events-delete (event)
  "Remove EVENT from `plan-polsl-events-file'."
  (plan-polsl-events--with-file
    (let ((found (plan-polsl-events--find event)))
      (delete-region (plist-get found :pos) (plist-get found :end-pos)))))

(defun plan-polsl-events--blank-to-nil (string)
  "Return STRING trimmed, or nil when it is blank."
  (let ((s (string-trim (or string ""))))
    (unless (string-empty-p s) s)))

(defun plan-polsl-events--parse-hours (string)
  "Parse hours STRING \"hh:mm-hh:mm\" or \"hh:mm\" into (FROM . TO).
Return t for a blank STRING (whole day) and nil when it is invalid."
  (let ((s (string-trim (or string ""))))
    (cond
     ((string-empty-p s) t)
     ((string-match "\\`\\([0-9]\\{1,2\\}:[0-5][0-9]\\)\\(?:[ \t]*-[ \t]*\\([0-9]\\{1,2\\}:[0-5][0-9]\\)\\)?\\'" s)
      (let ((from (plan-polsl-events--pad-time (match-string 1 s)))
            (to (plan-polsl-events--pad-time (match-string 2 s))))
        (when (and (string< from "24:00") (or (null to) (string< from to)))
          (cons from to)))))))

(defun plan-polsl-events--read-hours (prompt initial)
  "Read hours with PROMPT and INITIAL input until they are valid.
Return (FROM . TO), or nil for the whole day."
  (let ((hours nil))
    (while (null hours)
      (setq hours (plan-polsl-events--parse-hours (read-string prompt initial)))
      (unless hours
        (message "Nieprawidłowe godziny, wpisz np. 12:00-16:00")
        (sit-for 1.5)))
    (if (eq hours t) nil hours)))

(defun plan-polsl-events--read-date (prompt default)
  "Read a date with PROMPT and the Org calendar, starting at DEFAULT.
DEFAULT and the result are \"YYYY-MM-DD\"."
  (require 'org)
  (org-read-date nil nil nil prompt
                 (and default (date-to-time (concat default " 12:00")))))

(defun plan-polsl-events--courses ()
  "Return course names known from the timetable buffer and the events."
  (delete-dups
   (delq nil (append
              (mapcar (lambda (e) (or (plist-get e :full-title) (plist-get e :title)))
                      (bound-and-true-p plan-polsl-view-entries))
              (mapcar (lambda (ev) (plist-get ev :course))
                      (plan-polsl-events-list))))))

(defun plan-polsl-events-read (defaults)
  "Read an event in the minibuffer, step by step, prefilled with DEFAULTS.
DEFAULTS is an event plist; missing fields are asked without a
suggestion. Return the new event plist."
  (let* ((labels (mapcar (lambda (type) (cons (nth 1 type) (car type)))
                         plan-polsl-events-types))
         (type (cdr (assoc (completing-read
                            "Typ wydarzenia: " labels nil t nil nil
                            (and (plist-get defaults :type)
                                 (plan-polsl-events-type-label (plist-get defaults :type))))
                           labels)))
         (cancels (nth 2 (assoc type plan-polsl-events-types)))
         (start (plan-polsl-events--read-date "Data: " (plist-get defaults :start)))
         (end (if cancels
                  (plan-polsl-events--read-date
                   "Do dnia (RET = ten sam dzień): "
                   (let ((end (plist-get defaults :end)))
                     (if (and end (not (string< end start))) end start)))
                start))
         (_ (when (string< end start)
              (user-error "Koniec (%s) jest przed początkiem (%s)" end start)))
         (hours (plan-polsl-events--read-hours
                 (if (equal start end)
                     "Godziny (np. 12:00-16:00, puste = cały dzień): "
                   "Od godziny pierwszego dnia do godziny ostatniego (puste = całe dni): ")
                 (let ((from (plist-get defaults :from))
                       (to (plist-get defaults :to)))
                   (cond ((and from to) (format "%s-%s" from to))
                         (from from)))))
         (course (unless cancels
                   (plan-polsl-events--blank-to-nil
                    (completing-read "Przedmiot (puste = brak): "
                                     (plan-polsl-events--courses) nil nil
                                     (plist-get defaults :course)))))
         (room (unless cancels
                 (plan-polsl-events--blank-to-nil
                  (read-string "Sala (puste = brak): " (plist-get defaults :room)))))
         (title (or (plan-polsl-events--blank-to-nil
                     (read-string "Nazwa: "
                                  (if (equal (plist-get defaults :type) type)
                                      (plist-get defaults :title)
                                    (plan-polsl-events-type-label type))))
                    (plan-polsl-events-type-label type))))
    (list :type type :title title :start start :end end
          :from (car hours) :to (cdr hours)
          :course course :room room)))

(defun plan-polsl-events--refresh-view ()
  "Render the timetable in the current buffer again, if it shows one."
  (when (and (derived-mode-p 'plan-polsl-mode) plan-polsl-view-active-monday)
    (plan-polsl-view--show-week plan-polsl-view-active-monday)))

(defun plan-polsl-events--class-defaults ()
  "Return event defaults taken from the class at point, if any."
  (let ((entry (get-text-property (point) 'plan-polsl-entry)))
    (when (and entry (not (plist-get entry :event)))
      (list :from (plist-get entry :start-time)
            :to (plist-get entry :end-time)
            :course (or (plist-get entry :full-title) (plist-get entry :title))
            :room (car (plist-get entry :rooms))))))

(defun plan-polsl-events--check-file ()
  "Return non-nil when `plan-polsl-events-file' is set, else explain it."
  (or plan-polsl-events-file
      (progn
        (message (concat "Wydarzenia: najpierw ustaw plik w konfiguracji, np. "
                         "(setq plan-polsl-events-file \"~/dotfiles/polsl/wydarzenia.org\")"))
        nil)))

;;;###autoload
(defun plan-polsl-event-create ()
  "Create an event, such as a test or rector's hours, in the minibuffer.
The date starts at the timetable day at point; on a class, its hours,
course and room are suggested. The event is added to
`plan-polsl-events-file'."
  (interactive)
  (when (plan-polsl-events--check-file)
    (let ((event (plan-polsl-events-read
                  (append (list :start (or (get-text-property (point) 'plan-polsl-date)
                                           (format-time-string "%F")))
                          (plan-polsl-events--class-defaults)))))
      (plan-polsl-events-add event)
      (plan-polsl-events--refresh-view)
      (message "Dodano: %s (%s)" (plist-get event :title)
               (plan-polsl-events-timestamp event))
      event)))

(defun plan-polsl-events--describe (event)
  "Return a one-line description of EVENT for completion."
  (format "%s: %s (%s)"
          (plan-polsl-events-type-label (plist-get event :type))
          (plist-get event :title)
          (plan-polsl-events-timestamp event)))

(defun plan-polsl-events-at-point ()
  "Return the event at point in the timetable.
That is the event on the current line, else an event on the day at
point, chosen in the minibuffer when there are several."
  (let* ((entry (get-text-property (point) 'plan-polsl-entry))
         (date (get-text-property (point) 'plan-polsl-date))
         (events (cond ((plist-get entry :event) (list (plist-get entry :event)))
                       (date (plan-polsl-events-on-date date))
                       (t (user-error "Ustaw kursor na dniu z wydarzeniem")))))
    (cond
     ((null events) (user-error "Brak wydarzeń w tym dniu"))
     ((null (cdr events)) (car events))
     (t (let ((choices (mapcar (lambda (ev) (cons (plan-polsl-events--describe ev) ev))
                               events)))
          (cdr (assoc (completing-read "Wydarzenie: " choices nil t) choices)))))))

;;;###autoload
(defun plan-polsl-event-edit ()
  "Edit the event at point in the minibuffer, prefilled with its values.
See `plan-polsl-events-at-point' for which event is edited."
  (interactive)
  (when (plan-polsl-events--check-file)
    (let* ((old (plan-polsl-events-at-point))
           (new (plan-polsl-events-read old)))
      (plan-polsl-events-update old new)
      (plan-polsl-events--refresh-view)
      (message "Zapisano: %s (%s)" (plist-get new :title)
               (plan-polsl-events-timestamp new))
      new)))

(provide 'plan-polsl-events)
;;; plan-polsl-events.el ends here
