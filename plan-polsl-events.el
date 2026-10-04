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
    ("projekt" "Termin projektu" nil)
    ("odrabianie" "Odrabianie zajęć" nil)
    ("inne" "Wydarzenie" nil))
  "Event types as (TAG LABEL CANCELS-CLASSES).")

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

(provide 'plan-polsl-events)
;;; plan-polsl-events.el ends here
