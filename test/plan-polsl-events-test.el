;;; plan-polsl-events-test.el --- Tests for plan-polsl-events -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for academic events read from an Org file.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'plan-polsl-events)

(defconst plan-polsl-events-test--org
  "#+title: Wydarzenia PolSL

* Godziny rektorskie: inauguracja :rektorskie:
<2026-10-01 czw 12:00-18:00>

* Kolokwium 1 :kolokwium:uczelnia:
:PROPERTIES:
:PRZEDMIOT: Analiza danych i inteligencja obliczeniowa
:SALA:      416b
:END:
<2026-11-19 czw 14:00-16:15>
Rozdziały 1-3.

* Przerwa świąteczna :wolne:
<2026-12-22 wto>--<2027-01-06 śro>

* Zakupy :prywatne:
<2026-10-02 pią>

* Termin bez daty :egzamin:
"
  "Sample events file.")

(defmacro plan-polsl-events-test--with-file (&rest body)
  "Run BODY with `plan-polsl-events-file' holding the sample events."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "plan-polsl-events" t))
          (plan-polsl-events-file (expand-file-name "wydarzenia.org" dir))
          (plan-polsl-events--cache nil))
     (unwind-protect
         (progn
           (with-temp-file plan-polsl-events-file
             (insert plan-polsl-events-test--org))
           ,@body)
       (delete-directory dir t))))

(ert-deftest plan-polsl-events-test-parse-timestamp ()
  (should (equal (plan-polsl-events--parse-timestamp "<2026-10-01 czw 9:00-18:00>")
                 '(:start "2026-10-01" :end "2026-10-01" :from "09:00" :to "18:00"
                          :ts-beg 0 :ts-end 27)))
  (let ((ts (plan-polsl-events--parse-timestamp "x <2026-12-22 wto>--<2027-01-06 śro 10:00>")))
    (should (equal (plist-get ts :start) "2026-12-22"))
    (should (equal (plist-get ts :end) "2027-01-06"))
    (should-not (plist-get ts :from))
    (should (equal (plist-get ts :to) "10:00")))
  (should-not (plan-polsl-events--parse-timestamp "[2026-10-01 czw]")))

(ert-deftest plan-polsl-events-test-list ()
  (plan-polsl-events-test--with-file
   (let ((events (plan-polsl-events-list)))
     (should (equal (mapcar (lambda (e) (plist-get e :type)) events)
                    '("rektorskie" "kolokwium" "wolne")))
     (let ((test (nth 1 events)))
       (should (equal (plist-get test :title) "Kolokwium 1"))
       (should (equal (plist-get test :course) "Analiza danych i inteligencja obliczeniowa"))
       (should (equal (plist-get test :room) "416b"))
       (should (equal (plist-get test :from) "14:00"))))))

(ert-deftest plan-polsl-events-test-cache ()
  (plan-polsl-events-test--with-file
   (should (= (length (plan-polsl-events-list)) 3))
   (with-temp-file plan-polsl-events-file
     (insert "* Egzamin :egzamin:\n<2027-02-01 pon>\n"))
   (set-file-times plan-polsl-events-file (time-add (current-time) 10))
   (should (equal (mapcar (lambda (e) (plist-get e :title)) (plan-polsl-events-list))
                  '("Egzamin")))))

(ert-deftest plan-polsl-events-test-unset ()
  (let ((plan-polsl-events-file nil))
    (should-not (plan-polsl-events-list))))

(ert-deftest plan-polsl-events-test-cancelling ()
  (plan-polsl-events-test--with-file
   ;; rector's hours from 12:00 cancel classes overlapping them
   (should (plan-polsl-events-cancelling "2026-10-01" "11:00" "14:00"))
   (should-not (plan-polsl-events-cancelling "2026-10-01" "08:30" "12:00"))
   ;; every day of a break, whole day
   (should (equal (plist-get (plan-polsl-events-cancelling "2026-12-31" "08:00" "09:30") :title)
                  "Przerwa świąteczna"))
   ;; tests do not cancel classes
   (should-not (plan-polsl-events-cancelling "2026-11-19" "14:00" "16:15"))
   (should (equal (mapcar (lambda (e) (plist-get e :type))
                          (plan-polsl-events-on-date "2026-11-19"))
                  '("kolokwium")))))

(provide 'plan-polsl-events-test)
;;; plan-polsl-events-test.el ends here
