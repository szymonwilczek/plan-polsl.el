;;; plan-polsl-org-test.el --- Tests for plan-polsl-org -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for Org document generation.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'plan-polsl)

;; never read the semester start saved by the developer running the tests
(setq plan-polsl-semester-file
      (expand-file-name "plan-polsl-test-none/semester.eld" temporary-file-directory))

(defmacro plan-polsl-org-test--on (year month day &rest body)
  "Run BODY as if today were YEAR-MONTH-DAY."
  (declare (indent 3))
  `(let ((now (encode-time 0 0 12 ,day ,month ,year)))
     (cl-letf (((symbol-function 'current-time) (lambda () now)))
       ,@body)))

(ert-deftest plan-polsl-org-test-recurring-timestamp ()
  (plan-polsl-org-test--on 2026 10 3
                           (let ((plan-polsl-semester-start "2026-10-05"))
                             (should (equal (plan-polsl-org--format-timestamp 3 "10:15" "11:45")
                                            "<2026-10-07 śro 10:15-11:45 +1w>"))
                             (should (equal (plan-polsl-org--format-timestamp 1 "08:30" "10:00" t)
                                            "<2026-10-05 pon 08:30-10:00 +2w>")))))

(ert-deftest plan-polsl-org-test-base-monday-in-summer ()
  ;; the summer semester no longer falls back to October
  (plan-polsl-org-test--on 2027 4 15
                           (should (equal (plan-polsl-org--format-timestamp 2 "08:30" "10:00")
                                          "<2027-03-02 wto 08:30-10:00 +1w>"))))

(ert-deftest plan-polsl-org-test-date-timestamp ()
  (should (equal (plan-polsl-org--format-date-timestamp "2026-10-05" 1 "08:30" "10:00")
                 "<2026-10-05 pon 08:30-10:00>"))
  (should (equal (plan-polsl-org--format-date-timestamp "2026-10-10" 6 "09:00" "12:00")
                 "<2026-10-10 sob 09:00-12:00>")))

(ert-deftest plan-polsl-org-test-dated-entry ()
  (let ((text (plan-polsl-org-format-entry
               (list :day-index 1 :date "2026-10-05" :start-time "08:30" :end-time "10:00"
                     :title "Analiza matematyczna" :type "Wykład" :biweekly t
                     :teachers '("dr Jan Kowalski") :rooms '("301")))))
    (should (string-match-p "^\\*\\* Analiza matematyczna - Wykład :wyklad:uczelnia:" text))
    (should (string-match-p "<2026-10-05 pon 08:30-10:00>$" text))
    (should-not (string-match-p "\\+[12]w" text))
    (should (string-match-p ":PROWADZACY: dr Jan Kowalski" text))
    (should (string-match-p ":CYKL: Co 2 tygodnie" text))))

(ert-deftest plan-polsl-org-test-single-meeting-cycle ()
  (let ((once (plan-polsl-org-format-entry
               (list :day-index 6 :date "2026-10-10" :start-time "12:00" :end-time "14:00"
                     :title "Egzamin z fizyki" :type "Egzamin")))
        (weekly (plan-polsl-org-format-entry
                 (list :day-index 1 :date "2026-10-05" :start-time "08:30" :end-time "10:00"
                       :title "AiR" :type "Wykład" :cycle 'weekly))))
    (should (string-match-p ":CYKL: Pojedynczy termin" once))
    (should (string-match-p ":CYKL: Co tydzień" weekly))))

(ert-deftest plan-polsl-org-test-usos-properties ()
  (let ((text (plan-polsl-org-format-entry
               (list :day-index 1 :date "2026-10-05" :start-time "08:30" :end-time "10:00"
                     :title "AiR" :type "Wykład" :rooms '("301")
                     :building "Wydział AEiI" :url "https://usosweb.polsl.pl/g")))
        (plain (plan-polsl-org-format-entry
                (list :day-index 1 :start-time "08:30" :end-time "10:00"
                      :title "AiR" :type "Wykład"))))
    (should (string-match-p ":SALA: 301\n   :BUDYNEK: Wydział AEiI\n" text))
    (should (string-match-p ":URL: https://usosweb.polsl.pl/g\n   :END:" text))
    (should-not (string-match-p "BUDYNEK\\|URL" plain))))

(ert-deftest plan-polsl-org-test-type-to-tag ()
  (should (equal (plan-polsl-org--type-to-tag "Wykład") "wyklad"))
  (should (equal (plan-polsl-org--type-to-tag "Laboratorium") "lab"))
  (should (equal (plan-polsl-org--type-to-tag "Egzamin") "egzamin"))
  (should (equal (plan-polsl-org--type-to-tag nil) "zajecia")))

(ert-deftest plan-polsl-org-test-dated-document ()
  (let ((doc (plan-polsl-org-generate-dated-document
              (list (list :day-index 1 :date "2026-10-05" :start-time "08:30" :end-time "10:00"
                          :title "AiR" :type "Wykład")
                    (list :day-index 1 :date "2026-10-05" :start-time "10:15" :end-time "11:45"
                          :title "PSy" :type "Laboratorium")
                    (list :day-index 6 :date "2026-10-10" :start-time "09:00" :end-time "12:00"
                          :title "Zjazd" :type "Ćwiczenia"))
              "Jan Kowalski")))
    (should (string-prefix-p "#+title: Plan Zajęć Politechniki Śląskiej - Jan Kowalski\n" doc))
    (should (string-match-p
             "\\* Poniedziałek 05.10.2026\n\n\\*\\* AiR[^\0]*\\*\\* PSy[^\0]*\\* Sobota 10.10.2026\n\n\\*\\* Zjazd"
             doc))
    (should (= (with-temp-buffer
                 (insert doc)
                 (count-matches "^\\* " (point-min) (point-max)))
               2)))
  (should (string-match-p "Brak zaplanowanych zajęć"
                          (plan-polsl-org-generate-dated-document nil))))

(ert-deftest plan-polsl-org-test-usos-sync ()
  (let* ((dir (make-temp-file "plan-polsl-org" t))
         (plan-polsl-target-file (expand-file-name "plan.org" dir))
         (plan-polsl-auto-add-to-agenda nil)
         (weeks nil))
    (unwind-protect
        (cl-letf (((symbol-function 'plan-polsl-usos-logged-in-p) #'always)
                  ((symbol-function 'plan-polsl-usos--load-token)
                   (lambda () '(:user-name "Jan Kowalski")))
                  ((symbol-function 'plan-polsl-usos-fetch-week)
                   (lambda (monday)
                     (push (format-time-string "%F" monday) weeks)
                     (list (list :day-index 1 :date (format-time-string "%F" monday)
                                 :start-time "08:30" :end-time "10:00"
                                 :title "AiR" :type "Wykład" :cycle 'weekly)))))
          (should (= (plan-polsl-usos-sync 3) 3))
          (should (= (length weeks) 3))
          (should (equal (car (last weeks))
                         (format-time-string "%F" (plan-polsl-view--get-monday (current-time)))))
          (with-temp-buffer
            (insert-file-contents plan-polsl-target-file)
            (should (search-forward "Jan Kowalski" nil t))
            (should (= (count-matches "^\\*\\* AiR" (point-min) (point-max)) 3))))
      (delete-directory dir t))))

(ert-deftest plan-polsl-org-test-sync-dispatch ()
  (let ((called nil)
        (plan-polsl-usos-default t))
    (cl-letf (((symbol-function 'plan-polsl-usos-sync) (lambda (&rest _) (setq called 'usos)))
              ((symbol-function 'plan-polsl--sync-polsl) (lambda (&rest _) (setq called 'polsl))))
      (cl-letf (((symbol-function 'plan-polsl-usos-logged-in-p) #'always))
        (plan-polsl-sync)
        (should (eq called 'usos))
        (plan-polsl-sync "343266256" 0)
        (should (eq called 'polsl)))
      (cl-letf (((symbol-function 'plan-polsl-usos-logged-in-p) #'ignore))
        (plan-polsl-sync)
        (should (eq called 'polsl))))))

(provide 'plan-polsl-org-test)
;;; plan-polsl-org-test.el ends here
