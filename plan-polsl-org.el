;;; plan-polsl-org.el --- Org-mode schedule generator and agenda integration -*- lexical-binding: t; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, org

;;; Commentary:
;; Generates formatted Org-mode schedule files with recurring timestamps,
;; property drawers, and automatically manages registration in `org-agenda-files'

;;; Code:

(require 'cl-lib)
(require 'time-date)

(defconst plan-polsl-org-day-abbrevs
  ["pon" "wto" "śro" "czw" "pią"]
  "Short day of week names used in Org timestamps.")

(defun plan-polsl-org--get-base-monday ()
  "Compute date components (YEAR MONTH DAY) for Monday of semester start.
If `plan-polsl-semester-start' is set, uses that date.
Otherwise, auto-computes the appropriate semester start (October for winter, March for summer)."
  (if-let ((custom (bound-and-true-p plan-polsl-semester-start)))
      (if (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" custom)
          (list (string-to-number (match-string 1 custom))
                (string-to-number (match-string 2 custom))
                (string-to-number (match-string 3 custom)))
        (plan-polsl-org--auto-semester-monday))
    (plan-polsl-org--auto-semester-monday)))

(defun plan-polsl-org--auto-semester-monday ()
  "Determine the Monday of the semester based on current calendar month."
  (let* ((now (decode-time))
         (cur-year (nth 5 now))
         (cur-month (nth 4 now))
         ;; if currently between March (3) and September (9): winter semester begins in October
         ;; if currently between October (10) and February (2): winter semester began in October of cur-year
         ;; (or prev year for Jan/Feb)
         (target-year (if (<= cur-month 2) (1- cur-year) cur-year))
         (target-month 10)

         ;; find first Monday in October
         (oct-1-time (encode-time 0 0 12 1 target-month target-year))
         (oct-1-dow (nth 6 (decode-time oct-1-time))) ; 0=Sun, 1=Mon ... 6=Sat
         (days-to-first-mon (cond
                             ((= oct-1-dow 1) 0) ; Oct 1 is Monday
                             ((= oct-1-dow 0) 1) ; Oct 1 is Sunday -> Oct 2 is Monday
                             (t (- 8 oct-1-dow)))) ; Oct 1 is Tue-Sat -> first Monday is (8 - dow)
         (first-mon-time (time-add oct-1-time (days-to-time days-to-first-mon)))
         (dec (decode-time first-mon-time)))
    (list (nth 5 dec) (nth 4 dec) (nth 3 dec))))

(defun plan-polsl-org--format-timestamp (day-index start-time end-time &optional biweekly)
  "Format an active recurring Org timestamp for DAY-INDEX (1-5), START-TIME, END-TIME."
  (let* ((monday (plan-polsl-org--get-base-monday))
         (day-offset (1- day-index))
         (monday-encoded (encode-time 0 0 12 (nth 2 monday) (nth 1 monday) (nth 0 monday)))
         (target-time (time-add monday-encoded (days-to-time day-offset)))
         (dec (decode-time target-time))
         (year (nth 5 dec))
         (month (nth 4 dec))
         (day (nth 3 dec))
         (day-abbrev (aref plan-polsl-org-day-abbrevs (1- day-index)))
         (repeat (if biweekly "+2w" "+1w")))
    (format "<%04d-%02d-%02d %s %s-%s %s>"
            year month day day-abbrev start-time end-time repeat)))

(defun plan-polsl-org--type-to-tag (type-str)
  "Convert class TYPE-STR to a clean Org tag."
  (let ((ltype (downcase (or type-str ""))))
    (cond
     ((string-match-p "wyk" ltype) "wyklad")
     ((string-match-p "lab" ltype) "lab")
     ((string-match-p "sem" ltype) "seminarium")
     ((string-match-p "ćw" ltype) "cwiczenia")
     ((string-match-p "proj" ltype) "projekt")
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
         (timestamp (plan-polsl-org--format-timestamp day-idx start-time end-time biweekly))
         (sec-str (if sections (concat " (sek. " (mapconcat #'identity sections ", ") ")") ""))
         (teachers-str (if teachers (mapconcat #'identity teachers ", ") "Brak danych"))
         (rooms-str (if rooms (mapconcat #'identity rooms ", ") "Brak danych")))
    (format "** %s%s - %s :%s:uczelnia:\n   %s\n   :PROPERTIES:\n   :TYP: %s\n   :SALA: %s\n   :PROWADZACY: %s\n%s   :CYKL: %s\n   :END:\n\n"
            title sec-str type tag
            timestamp
            type
            rooms-str
            teachers-str
            (if sections (format "   :SEKCJA: %s\n" (mapconcat #'identity sections ", ")) "")
            (if biweekly "Co 2 tygodnie (*)" "Co tydzień"))))

(defun plan-polsl-org-generate-document (entries &optional group-id)
  "Generate complete Org-mode document string for ENTRIES and GROUP-ID."
  (let* ((grp-info (if group-id (format "Grupa: %s" group-id) "Plan Zajęć"))
         (out (format "#+title: Plan Zajęć Politechniki Śląskiej - %s\n#+author: plan-polsl.el\n#+category: PolSL\n#+startup: overview\n#+filetags: :polsl:uczelnia:\n\n"
                      grp-info))

         ;; group entries by day of week
         (by-day (make-vector 5 nil)))
    (dolist (e entries)
      (let ((idx (1- (plist-get e :day-index))))
        (when (and (>= idx 0) (< idx 5))
          (aset by-day idx (append (aref by-day idx) (list e))))))
    (dotimes (i 5)
      (let ((day-entries (aref by-day i))
            (day-name (aref ["Poniedziałek" "Wtorek" "Środa" "Czwartek" "Piątek"] i)))
        (setq out (concat out (format "* %s\n\n" day-name)))
        (if day-entries
            (dolist (e day-entries)
              (setq out (concat out (plan-polsl-org-format-entry e))))
          (setq out (concat out "  Brak zaplanowanych zajęć.\n\n")))))
    out))

(defun plan-polsl-org-write-to-file (content target-file)
  "Write Org CONTENT to TARGET-FILE, ensuring parent directory exists."
  (let ((dir (file-name-directory (expand-file-name target-file))))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (with-temp-file target-file
    (insert content))
  (when (bound-and-true-p plan-polsl-auto-add-to-agenda)
    (plan-polsl-org-register-in-agenda target-file)))

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

(provide 'plan-polsl-org)
;;; plan-polsl-org.el ends here
