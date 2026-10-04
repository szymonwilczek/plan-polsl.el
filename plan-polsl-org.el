;;; plan-polsl-org.el --- Org-mode schedule generator and agenda integration -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, org

;;; Commentary:
;; Generates formatted Org-mode schedule files with recurring timestamps,
;; property drawers, and automatically manages registration in `org-agenda-files'

;;; Code:

(require 'cl-lib)
(require 'time-date)
(require 'plan-polsl-semester)

(defvar org-agenda-files)

(defconst plan-polsl-org-day-abbrevs
  ["pon" "wto" "śro" "czw" "pią" "sob" "nie"]
  "Short day of week names used in Org active timestamps.")

(defun plan-polsl-org--format-timestamp (day-index start-time end-time &optional biweekly cycle)
  "Format an active recurring Org timestamp for DAY-INDEX, START-TIME, END-TIME.
BIWEEKLY classes repeat every two weeks; CYCLE `even' makes them start
in week 2. The first occurrence never falls before the semester start."
  (let* ((start (format-time-string "%F" (plan-polsl-semester-start)))
         (target-time (time-add (plan-polsl-semester-first-monday)
                                (days-to-time (+ (1- day-index)
                                                 (if (eq cycle 'even) 7 0)))))
         (step (if biweekly 14 7)))
    (while (string< (format-time-string "%F" target-time) start)
      (setq target-time (time-add target-time (days-to-time step))))
    (let* ((dec (decode-time target-time))
           (day-abbrev (aref plan-polsl-org-day-abbrevs (1- day-index))))
      (format "<%04d-%02d-%02d %s %s-%s %s>"
              (nth 5 dec) (nth 4 dec) (nth 3 dec) day-abbrev start-time end-time
              (if biweekly "+2w" "+1w")))))

(defun plan-polsl-org--format-date-timestamp (date day-index start-time end-time)
  "Format a non-repeating active Org timestamp.
DATE is \"YYYY-MM-DD\", DAY-INDEX the ISO weekday (1=Mon .. 7=Sun),
START-TIME and END-TIME are \"hh:mm\" strings."
  (format "<%s %s %s-%s>"
          date (aref plan-polsl-org-day-abbrevs (1- day-index)) start-time end-time))

(defun plan-polsl-org--type-to-tag (type-str)
  "Convert class TYPE-STR to a clean Org tag."
  (let ((ltype (downcase (or type-str ""))))
    (cond
     ((string-match-p "wyk" ltype) "wyklad")
     ((string-match-p "lab" ltype) "lab")
     ((string-match-p "sem" ltype) "seminarium")
     ((string-match-p "ćw" ltype) "cwiczenia")
     ((string-match-p "proj" ltype) "projekt")
     ((string-match-p "egz" ltype) "egzamin")
     (t "zajecia"))))

(defun plan-polsl-org-format-entry (entry)
  "Format a single parsed class ENTRY into an Org headline."
  (let* ((title (plist-get entry :title))
         (type (plist-get entry :type))
         (tag (plan-polsl-org--type-to-tag type))
         (day-idx (plist-get entry :day-index))
         (start-time (plist-get entry :start-time))
         (end-time (plist-get entry :end-time))
         (biweekly (plist-get entry :biweekly))
         (sections (plist-get entry :sections))
         (teachers (plist-get entry :teachers))
         (rooms (plist-get entry :rooms))
         (date (plist-get entry :date))
         (timestamp (if date
                        (plan-polsl-org--format-date-timestamp date day-idx start-time end-time)
                      (plan-polsl-org--format-timestamp day-idx start-time end-time biweekly
                                                        (plist-get entry :cycle))))
         (sec-str (if sections (concat " (sek. " (mapconcat #'identity sections ", ") ")") ""))
         (teachers-str (if teachers (mapconcat #'identity teachers ", ") "Brak danych"))
         (rooms-str (if rooms (mapconcat #'identity rooms ", ") "Brak danych")))
    (format "** %s%s - %s :%s:uczelnia:\n   %s\n   :PROPERTIES:\n   :TYP: %s\n   :SALA: %s\n%s   :PROWADZACY: %s\n%s   :CYKL: %s\n%s   :END:\n\n"
            title sec-str type tag
            timestamp
            type
            rooms-str
            (if-let* ((building (plist-get entry :building)))
                (format "   :BUDYNEK: %s\n" building)
              "")
            teachers-str
            (if sections (format "   :SEKCJA: %s\n" (mapconcat #'identity sections ", ")) "")
            (cond
             (biweekly "Co 2 tygodnie (*)")
             ((and date (not (eq (plist-get entry :cycle) 'weekly))) "Pojedynczy termin")
             (t "Co tydzień"))
            (if-let* ((url (plist-get entry :url)))
                (format "   :URL: %s\n" url)
              ""))))

(defun plan-polsl-org-generate-document (entries &optional title-info)
  "Generate complete Org-mode document string for ENTRIES and TITLE-INFO."
  (let* ((header (format "#+title: Plan Zajęć Politechniki Śląskiej - %s\n#+author: plan-polsl.el\n#+category: PolSL\n#+startup: overview\n#+filetags: :polsl:uczelnia:\n\n"
                         (or title-info "Plan Zajęć")))
         (by-day (make-vector 5 nil)))

    ;; group entries by day of week
    (dolist (e entries)
      (let ((idx (1- (plist-get e :day-index))))
        (when (and (>= idx 0) (< idx 5))
          (aset by-day idx (append (aref by-day idx) (list e))))))

    ;; build document
    (let ((out header))
      (dotimes (i 5)
        (let ((day-entries (aref by-day i))
              (day-name (aref ["Poniedziałek" "Wtorek" "Środa" "Czwartek" "Piątek"] i)))
          (setq out (concat out (format "* %s\n\n" day-name)))
          (if day-entries
              (dolist (e day-entries)
                (setq out (concat out (plan-polsl-org-format-entry e))))
            (setq out (concat out "  Brak zaplanowanych zajęć.\n\n")))))
      out)))

(defun plan-polsl-org-generate-dated-document (entries &optional title-info)
  "Generate an Org document for dated ENTRIES (from USOS) and TITLE-INFO.
ENTRIES must be sorted chronologically; each date gets its own heading."
  (let ((out (format "#+title: Plan Zajęć Politechniki Śląskiej - %s\n#+author: plan-polsl.el (USOS)\n#+category: PolSL\n#+startup: overview\n#+filetags: :polsl:uczelnia:\n\n"
                     (or title-info "Plan Zajęć")))
        (day-names ["Poniedziałek" "Wtorek" "Środa" "Czwartek" "Piątek" "Sobota" "Niedziela"])
        (current-date nil))
    (if (null entries)
        (concat out "Brak zaplanowanych zajęć.\n")
      (dolist (e entries)
        (let ((date (plist-get e :date)))
          (unless (equal date current-date)
            (setq current-date date)
            (setq out (concat out (format "* %s %s\n\n"
                                          (aref day-names (1- (plist-get e :day-index)))
                                          (format-time-string
                                           "%d.%m.%Y" (date-to-time (concat date " 12:00")))))))
          (setq out (concat out (plan-polsl-org-format-entry e)))))
      out)))

(defun plan-polsl-org-register-in-agenda (file)
  "Add FILE to `org-agenda-files' if not already registered."
  (let ((expanded (expand-file-name file)))
    (if (boundp 'org-agenda-files)
        (let ((files (if (listp org-agenda-files)
                         org-agenda-files
                       (list org-agenda-files))))
          (unless (member expanded files)
            (setq org-agenda-files (append files (list expanded)))))
      (setq org-agenda-files (list expanded)))))

(defun plan-polsl-org-write-to-file (content target-file)
  "Write Org CONTENT to TARGET-FILE, creating parent directories if needed."
  (let ((dir (file-name-directory (expand-file-name target-file))))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (with-temp-file target-file
    (insert content))
  (when (bound-and-true-p plan-polsl-auto-add-to-agenda)
    (plan-polsl-org-register-in-agenda target-file)
    (when-let* ((events-file (bound-and-true-p plan-polsl-events-file)))
      (when (file-exists-p events-file)
        (plan-polsl-org-register-in-agenda events-file)))))

(provide 'plan-polsl-org)
;;; plan-polsl-org.el ends here
