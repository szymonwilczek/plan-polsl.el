;;; plan-polsl-semester.el --- Semester start date -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl

;;; Commentary:
;; Determines when the current semester starts, which defines week
;; numbers and odd/even week parity. The dean announces the date every
;; semester, so users enter it once with `plan-polsl-set-semester-start';
;; it is stored in a private data file. Without it, the configured
;; `plan-polsl-semester-start' or a heuristic (October 1st for winter,
;; early March for summer) is used.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'time-date)
(require 'plan-polsl-store)

(defcustom plan-polsl-semester-start nil
  "Start date of the academic semester in \"YYYY-MM-DD\" format.
The date saved with `plan-polsl-set-semester-start' takes precedence.
When nil, the start is guessed: October 1st or early March."
  :type '(choice (const :tag "Auto (October / March)" nil)
                 (string :tag "Custom Date (YYYY-MM-DD)"))
  :group 'plan-polsl)

(defcustom plan-polsl-semester-file
  (plan-polsl-store-data-file "semester.eld")
  "File storing the semester start date set by `plan-polsl-set-semester-start'."
  :type 'file
  :group 'plan-polsl)

(defconst plan-polsl-semester--validity-days (* 26 7)
  "Days after a configured start date during which it is still used.
Later dates fall back to the heuristic, so a forgotten date from the
previous semester does not silently skew week parity forever.")

(defun plan-polsl-semester--parse-date (string)
  "Parse STRING as \"YYYY-MM-DD\" or \"DD.MM.YYYY\" into a time value.
Returns nil when STRING is not a valid date."
  (let ((s (string-trim (or string ""))))
    (pcase-let ((`(,y ,m ,d)
                 (cond
                  ((string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{1,2\\}\\)-\\([0-9]\\{1,2\\}\\)\\'" s)
                   (list (match-string 1 s) (match-string 2 s) (match-string 3 s)))
                  ((string-match "\\`\\([0-9]\\{1,2\\}\\)\\.\\([0-9]\\{1,2\\}\\)\\.\\([0-9]\\{4\\}\\)\\'" s)
                   (list (match-string 3 s) (match-string 2 s) (match-string 1 s))))))
      (when y
        (let* ((year (string-to-number y))
               (month (string-to-number m))
               (day (string-to-number d)))
          (when (and (<= 1 month 12)
                     (<= 1 day (date-days-in-month year month)))
            (encode-time 0 0 0 day month year)))))))

(defun plan-polsl-semester--monday (time)
  "Return the Monday (at midnight) of the week containing TIME."
  (let* ((dec (decode-time time))
         (dow (nth 6 dec))
         (back (if (= dow 0) 6 (1- dow))))
    (encode-time 0 0 0 (- (nth 3 dec) back) (nth 4 dec) (nth 5 dec))))

(defun plan-polsl-semester--heuristic-start (time)
  "Return the usual semester start date for TIME.
The winter semester starts on October 1st, the summer one on March
2nd. The semester is picked by the end of TIME's week, so the last
days of September already belong to the winter semester."
  (let* ((dec (decode-time (time-add (plan-polsl-semester--monday time)
                                     (days-to-time 6))))
         (year (nth 5 dec))
         (month (nth 4 dec)))
    (if (or (>= month 10) (<= month 2))
        (encode-time 0 0 0 1 10 (if (<= month 2) (1- year) year))
      (encode-time 0 0 0 2 3 year))))

(defun plan-polsl-semester--saved-start ()
  "Return the start date stored in `plan-polsl-semester-file', or nil."
  (plan-polsl-semester--parse-date
   (plist-get (plan-polsl-store-read plan-polsl-semester-file) :start)))

(defun plan-polsl-semester--applies-p (start time)
  "Return non-nil when configured START date is meant for TIME.
It applies from the month before START until
`plan-polsl-semester--validity-days' after it."
  (let ((delta (/ (float-time (time-subtract time start)) 86400)))
    (and (>= delta -31) (< delta plan-polsl-semester--validity-days))))

(defun plan-polsl-semester-start (&optional time)
  "Return the start date of the semester relevant for TIME (default now).
Uses, in order: the date saved by `plan-polsl-set-semester-start',
`plan-polsl-semester-start', and the October/March heuristic. Week 1 is
the week containing the returned date."
  (let ((time (or time (current-time))))
    (or (seq-find (lambda (start) (and start (plan-polsl-semester--applies-p start time)))
                  (list (plan-polsl-semester--saved-start)
                        (plan-polsl-semester--parse-date
                         plan-polsl-semester-start)))
        (plan-polsl-semester--heuristic-start time))))

(defun plan-polsl-semester-first-monday (&optional time)
  "Return the Monday of week 1 of the semester relevant for TIME."
  (plan-polsl-semester--monday (plan-polsl-semester-start time)))

;;;###autoload
(defun plan-polsl-set-semester-start (date)
  "Save DATE as the start of the current semester.
DATE is \"YYYY-MM-DD\" or \"DD.MM.YYYY\", as announced by the dean.
Week 1 is the week containing it; odd/even parity follows from it."
  (interactive
   (list (read-string
          (format "Początek semestru (RRRR-MM-DD lub DD.MM.RRRR) [%s]: "
                  (format-time-string "%Y-%m-%d" (plan-polsl-semester-start)))
          nil nil
          (format-time-string "%Y-%m-%d" (plan-polsl-semester-start)))))
  (let ((time (plan-polsl-semester--parse-date date)))
    (unless time
      (user-error "Nieprawidłowa data: %s (oczekiwano RRRR-MM-DD lub DD.MM.RRRR)" date))
    (plan-polsl-store-write plan-polsl-semester-file
                            "plan-polsl semester start date"
                            (list :start (format-time-string "%Y-%m-%d" time)))
    (message "Zapisano początek semestru: %s (tydzień 1 od %s)"
             (format-time-string "%d.%m.%Y" time)
             (format-time-string "%d.%m.%Y" (plan-polsl-semester--monday time)))
    time))

(provide 'plan-polsl-semester)
;;; plan-polsl-semester.el ends here
