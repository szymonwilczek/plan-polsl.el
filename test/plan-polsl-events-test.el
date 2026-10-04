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

(defun plan-polsl-events-test--file-string ()
  "Return the contents of the events file."
  (with-temp-buffer
    (insert-file-contents plan-polsl-events-file)
    (buffer-string)))

(defmacro plan-polsl-events-test--cleanup (&rest body)
  "Run BODY, then kill the buffer visiting the events file."
  (declare (indent 0))
  `(unwind-protect (progn ,@body)
     (when-let* ((buf (find-buffer-visiting plan-polsl-events-file)))
       (with-current-buffer buf (set-buffer-modified-p nil))
       (kill-buffer buf))))

(ert-deftest plan-polsl-events-test-timestamp ()
  (should (equal (plan-polsl-events-timestamp
                  '(:start "2026-10-01" :end "2026-10-01" :from "12:00" :to "18:00"))
                 "<2026-10-01 czw 12:00-18:00>"))
  (should (equal (plan-polsl-events-timestamp '(:start "2026-10-04" :end "2026-10-04"))
                 "<2026-10-04 nie>"))
  (should (equal (plan-polsl-events-timestamp
                  '(:start "2026-12-22" :end "2027-01-06" :from "12:00"))
                 "<2026-12-22 wto 12:00>--<2027-01-06 śro>")))

(ert-deftest plan-polsl-events-test-add-sorted ()
  (plan-polsl-events-test--with-file
   (plan-polsl-events-test--cleanup
    (plan-polsl-events-add '(:type "egzamin" :title "Egzamin AM" :start "2026-11-20"
                                   :end "2026-11-20" :from "09:00" :to "11:00"
                                   :room "Aula A"))
    (plan-polsl-events-add '(:type "inne" :title "Juwenalia" :start "2027-05-20"
                                   :end "2027-05-20"))
    (should (equal (mapcar (lambda (e) (plist-get e :title)) (plan-polsl-events-list))
                   '("Godziny rektorskie: inauguracja" "Kolokwium 1" "Egzamin AM"
                     "Przerwa świąteczna" "Juwenalia")))
    (should (string-match-p
             (regexp-quote "Rozdziały 1-3.

* Egzamin AM :egzamin:
:PROPERTIES:
:SALA: Aula A
:END:
<2026-11-20 pią 09:00-11:00>

* Przerwa świąteczna")
             (plan-polsl-events-test--file-string)))
    (should (string-suffix-p "* Juwenalia :inne:\n<2027-05-20 czw>\n\n"
                             (plan-polsl-events-test--file-string))))))

(ert-deftest plan-polsl-events-test-new-file ()
  (let* ((dir (make-temp-file "plan-polsl-events" t))
         (plan-polsl-events-file (expand-file-name "sub/wydarzenia.org" dir)))
    (unwind-protect
        (plan-polsl-events-test--cleanup
         (plan-polsl-events-add '(:type "wolne" :title "Rektorskie" :start "2026-10-02"
                                        :end "2026-10-02"))
         (should (equal (plan-polsl-events-test--file-string)
                        "#+title: Wydarzenia PolSL\n\n* Rektorskie :wolne:\n<2026-10-02 pią>\n\n")))
      (delete-directory dir t))))

(ert-deftest plan-polsl-events-test-update-keeps-notes ()
  (plan-polsl-events-test--with-file
   (plan-polsl-events-test--cleanup
    (with-temp-file plan-polsl-events-file
      (insert "* Kolokwium 1 :kolokwium:
:PROPERTIES:
:PRZEDMIOT: Fizyka
:ID:       abc
:END:
<2026-11-19 czw 14:00-16:15> przynieść kalkulator
Rozdziały 1-3.

* Inne :inne:
<2026-12-01 wto>
"))
    (let* ((old (car (plan-polsl-events-list)))
           (new (plist-put (plist-put (copy-sequence old) :title "Kolokwium poprawkowe")
                           :start "2027-01-10")))
      (setq new (plist-put new :end "2027-01-10"))
      (plan-polsl-events-update old new))
    (should (equal (plan-polsl-events-test--file-string)
                   "* Inne :inne:
<2026-12-01 wto>

* Kolokwium poprawkowe :kolokwium:
:PROPERTIES:
:PRZEDMIOT: Fizyka
:ID:       abc
:END:
<2027-01-10 nie 14:00-16:15>
przynieść kalkulator
Rozdziały 1-3.

")))))

(ert-deftest plan-polsl-events-test-delete ()
  (plan-polsl-events-test--with-file
   (plan-polsl-events-test--cleanup
    (plan-polsl-events-delete (nth 1 (plan-polsl-events-list)))
    (should (equal (mapcar (lambda (e) (plist-get e :type)) (plan-polsl-events-list))
                   '("rektorskie" "wolne")))
    (should-not (string-match-p "Rozdziały" (plan-polsl-events-test--file-string)))
    (should-error (plan-polsl-events-delete '(:title "Brak")) :type 'user-error))))

(provide 'plan-polsl-events-test)
;;; plan-polsl-events-test.el ends here
