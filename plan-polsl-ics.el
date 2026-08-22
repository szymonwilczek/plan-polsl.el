;;; plan-polsl-ics.el --- iCalendar (ICS) exact semester dates parser -*- lexical-binding: t; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, ics

;;; Commentary:
;; Fetches and parses PolSL semester iCalendar (.ics) files,
;; extracting exact calendar dates (accounting for semester start/end,
;; free days, rector days, and bi-weekly schedules) into Org-mode.

;;; Code:

(require 'cl-lib)
(require 'time-date)

(defconst plan-polsl-ics-day-abbrevs
  ["nie" "pon" "wto" "śro" "czw" "pią" "sob"]
  "Day of week abbreviations indexed by 0=Sun .. 6=Sat.")

(defun plan-polsl-ics-parse-datetime (dt-str)
  "Convert UTC iCalendar DTSTART/DTEND string (e.g. \"20260929T060000Z\") to local date/time."
  (if (and dt-str (string-match "\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)" dt-str))
      (let* ((year (string-to-number (match-string 1 dt-str)))
             (month (string-to-number (match-string 2 dt-str)))
             (day (string-to-number (match-string 3 dt-str)))
             (hour (string-to-number (match-string 4 dt-str)))
             (min (string-to-number (match-string 5 dt-str)))
             (utc-time (encode-time 0 min hour day month year t))
             (local (decode-time utc-time "Europe/Warsaw"))
             (l-year (nth 5 local))
             (l-month (nth 4 local))
             (l-day (nth 3 local))
             (l-hour (nth 2 local))
             (l-min (nth 1 local))
             (l-dow (nth 6 local)))
        (list :date (format "%04d-%02d-%02d" l-year l-month l-day)
              :time (format "%02d:%02d" l-hour l-min)
              :dow l-dow
              :day-abbrev (aref plan-polsl-ics-day-abbrevs l-dow)
              :timestamp (float-time utc-time)))
    nil))

(defun plan-polsl-ics-parse-summary (summary)
  "Parse SUMMARY string from VEVENT into title, type, sections, and extra info."
  (let* ((s (string-trim (or summary "")))
         (biweekly (string-prefix-p "*" s))
         (clean (replace-regexp-in-string "^\\*\\s*" "" s))

         ;; extract sections
         (sections (if (string-match "(sek\\.?[ \t\n]*\\([0-9, ]+\\))" clean)
                       (split-string (match-string 1 clean) "[, ]+" t)
                     nil))
         (clean-no-sec (replace-regexp-in-string "(sek\\.?[^)]+)" "" clean))

         ;; detect type
         (type (cond
                ((string-match-p "\\blab\\b" (downcase clean)) "Laboratorium")
                ((string-match-p "\\bwyk\\b" (downcase clean)) "Wykład")
                ((string-match-p "\\bsem\\b" (downcase clean)) "Seminarium")
                ((string-match-p "\\bćw\\b" (downcase clean)) "Ćwiczenia")
                ((string-match-p "\\bproj\\b" (downcase clean)) "Projekt")
                (t "Zajęcia")))

         ;; clean title
         (title (string-trim (car (split-string clean-no-sec "\\(lab\\|wyk\\|sem\\|ćw\\|proj\\)" t)))))
    (list :title (if (string-blank-p title) clean title)
          :type type
          :sections sections
          :biweekly (and biweekly t)
          :raw-summary s)))

(defun plan-polsl-ics-parse (ics-text)
  "Parse ICS-TEXT content into a list of structured academic class occurrences."
  (let ((events nil)
        (pos 0))
    (while (string-match "BEGIN:VEVENT\\(\\(?:.\\|\n\\)*?\\)END:VEVENT" ics-text pos)
      (let* ((body (match-string 1 ics-text))
             (dtstart-raw (when (string-match "DTSTART:[ \t]*\\([0-9TZ]+\\)" body)
                            (match-string 1 body)))
             (dtend-raw (when (string-match "DTEND:[ \t]*\\([0-9TZ]+\\)" body)
                          (match-string 1 body)))
             (summary-raw (when (string-match "SUMMARY:[ \t]*\\([^\r\n]+\\)" body)
                            (match-string 1 body)))
             (location-raw (when (string-match "LOCATION:[ \t]*\\([^\r\n]+\\)" body)
                             (match-string 1 body)))
             (start-info (plan-polsl-ics-parse-datetime dtstart-raw))
             (end-info (plan-polsl-ics-parse-datetime dtend-raw))
             (sum-info (plan-polsl-ics-parse-summary summary-raw)))
        (when (and start-info end-info sum-info)
          (push (list :date (plist-get start-info :date)
                      :day-abbrev (plist-get start-info :day-abbrev)
                      :dow (plist-get start-info :dow)
                      :start-time (plist-get start-info :time)
                      :end-time (plist-get end-info :time)
                      :title (plist-get sum-info :title)
                      :type (plist-get sum-info :type)
                      :sections (plist-get sum-info :sections)
                      :biweekly (plist-get sum-info :biweekly)
                      :raw-summary (plist-get sum-info :raw-summary)
                      :location (or location-raw "")
                      :timestamp (plist-get start-info :timestamp))
                events)))
      (setq pos (match-end 0)))

    ;; sort chronologically by real calendar timestamp
    (sort (nreverse events)
          (lambda (a b)
            (< (plist-get a :timestamp) (plist-get b :timestamp))))))

(defun plan-polsl-ics-generate-org-document (events &optional group-id section)
  "Generate an Org-mode document from real calendar EVENTS with exact dates."
  (let* ((sec-info (if section (format " (Sekcja: %s)" section) " (Wszystkie sekcje)"))
         (grp-info (if group-id (format "Grupa: %s" group-id) "Plan Zajęć"))
         (out (format "#+title: Plan Zajęć Politechniki Śląskiej - %s%s\n#+author: plan-polsl.el\n#+category: PolSL\n#+startup: overview\n#+filetags: :polsl:uczelnia:\n\n"
                      grp-info sec-info))
         (current-month nil))
    (dolist (e events)
      (let* ((date-str (plist-get e :date))
             (month-str (substring date-str 0 7)) ; "YYYY-MM"
             (title (plist-get e :title))
             (type (plist-get e :type))
             (day-abbrev (plist-get e :day-abbrev))
             (start-time (plist-get e :start-time))
             (end-time (plist-get e :end-time))
             (sections (plist-get e :sections))
             (raw-sum (plist-get e :raw-summary))
             (tag (cond
                   ((string-match-p "wyk" (downcase type)) "wyklad")
                   ((string-match-p "lab" (downcase type)) "lab")
                   ((string-match-p "sem" (downcase type)) "seminarium")
                   (t "zajecia")))
             (sec-str (if sections (concat " (sek. " (mapconcat #'identity sections ", ") ")") "")))

        ;; create month headline
        (unless (string-equal current-month month-str)
          (setq current-month month-str)
          (setq out (concat out (format "* Kalendarz: %s\n\n" month-str))))

        ;; insert event with EXACT calendar date
        (setq out (concat out
                          (format "** %s - %s%s :%s:uczelnia:\n   <%s %s %s-%s>\n   :PROPERTIES:\n   :TYP: %s\n%s   :OPIS: %s\n   :END:\n\n"
                                  title type sec-str tag
                                  date-str day-abbrev start-time end-time
                                  type
                                  (if sections (format "   :SEKCJA: %s\n" (mapconcat #'identity sections ", ")) "")
                                  raw-sum)))))
    out))

(defun plan-polsl-ics-fetch-schedule (group-id &optional type)
  "Fetch .ics calendar file from plan.polsl.pl for GROUP-ID."
  (let* ((base (or (bound-and-true-p plan-polsl-base-url) "https://plan.polsl.pl/"))
         (type-val (or type 0))
         (url (format "%splan.php?type=%s&id=%s&cvsfile=true&wd=1"
                      (if (string-suffix-p "/" base) base (concat base "/"))
                      type-val
                      (url-hexify-string (format "%s" group-id)))))
    (if (executable-find "curl")
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (call-process "curl" nil t nil
                        "-s" "-k" "--http1.1"
                        "--ciphers" "DEFAULT@SECLEVEL=1"
                        "-A" "Emacs plan-polsl.el/0.1.0 (GNU Emacs)"
                        url)
          (if (> (buffer-size) 100)
              (decode-coding-string (buffer-string) 'utf-8)
            (error "plan-polsl: Nie udało się pobrać pliku .ics z %s" url)))
      (plan-polsl-http-fetch url))))

(provide 'plan-polsl-ics)
;;; plan-polsl-ics.el ends here
