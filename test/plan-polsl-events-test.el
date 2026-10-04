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

(defmacro plan-polsl-events-test--answers (answers &rest body)
  "Run BODY answering minibuffer prompts from the ANSWERS alist.
Keys are prompt prefixes, values the answers; prompts and their
initial inputs are recorded in `plan-polsl-events-test--asked'."
  (declare (indent 1))
  `(let ((answers ,answers))
     (cl-letf* ((answer (lambda (prompt initial)
                          (push (cons prompt initial) plan-polsl-events-test--asked)
                          (or (cdr (cl-find-if (lambda (a) (string-prefix-p (car a) prompt))
                                               answers))
                              initial "")))
                ((symbol-function 'read-string)
                 (lambda (prompt &optional initial &rest _) (funcall answer prompt initial)))
                ((symbol-function 'completing-read)
                 (lambda (prompt _coll &optional _pred _req initial _hist def &rest _)
                   (funcall answer prompt (or initial def))))
                ((symbol-function 'org-read-date)
                 (lambda (_with-time _to-time _from-string prompt &optional default &rest _)
                   (funcall answer prompt (and default (format-time-string "%F" default))))))
       ,@body)))

(defvar plan-polsl-events-test--asked nil
  "Prompts asked by `plan-polsl-events-test--answers', newest first.")

(ert-deftest plan-polsl-events-test-parse-hours ()
  (should (equal (plan-polsl-events--parse-hours " 9:00 - 11:30 ") '("09:00" . "11:30")))
  (should (equal (plan-polsl-events--parse-hours "12:00") '("12:00")))
  (should (eq (plan-polsl-events--parse-hours "") t))
  (should-not (plan-polsl-events--parse-hours "12:00-11:00"))
  (should-not (plan-polsl-events--parse-hours "rano")))

(ert-deftest plan-polsl-events-test-create-on-class ()
  (plan-polsl-events-test--with-file
   (plan-polsl-events-test--cleanup
    (let ((plan-polsl-events-test--asked nil))
      (with-temp-buffer
        (insert (propertize "  14:00 - 16:15  [Laboratorium]  ADiIO\n"
                            'plan-polsl-date "2026-11-26"
                            'plan-polsl-entry
                            '(:title "ADiIO" :full-title "Analiza danych i inteligencja obliczeniowa"
                                     :start-time "14:00" :end-time "16:15" :rooms ("416b"))))
        (goto-char (point-min))
        (plan-polsl-events-test--answers '(("Typ" . "Kolokwium") ("Nazwa" . "Kolokwium 2"))
                                         (plan-polsl-event-create)))
      ;; the class prefilled the date, hours, course and room
      (should (equal (cdr (assoc "Data: " plan-polsl-events-test--asked)) "2026-11-26"))
      (should-not (cl-find-if (lambda (a) (string-prefix-p "Do dnia" (car a)))
                              plan-polsl-events-test--asked))
      (should (equal (cdr (assoc "Nazwa: " plan-polsl-events-test--asked)) "Kolokwium"))
      (should (equal (plan-polsl-events--key
                      (cl-find "Kolokwium 2" (plan-polsl-events-list)
                               :key (lambda (e) (plist-get e :title)) :test #'equal))
                     '("kolokwium" "Kolokwium 2" "2026-11-26" "2026-11-26" "14:00" "16:15"
                       "Analiza danych i inteligencja obliczeniowa" "416b")))))))

(ert-deftest plan-polsl-events-test-create-days-off ()
  (plan-polsl-events-test--with-file
   (plan-polsl-events-test--cleanup
    (let ((plan-polsl-events-test--asked nil))
      (with-temp-buffer
        (plan-polsl-events-test--answers '(("Typ" . "Dzień wolny") ("Data" . "2027-04-01")
                                           ("Do dnia" . "2027-04-06") ("Nazwa" . "Wielkanoc"))
                                         (plan-polsl-event-create)))
      ;; days off ask for no course or room
      (should-not (cl-find-if (lambda (a) (string-prefix-p "Przedmiot" (car a)))
                              plan-polsl-events-test--asked))
      (let ((ev (car (last (plan-polsl-events-list)))))
        (should (equal (plist-get ev :title) "Wielkanoc"))
        (should (equal (plist-get ev :end) "2027-04-06"))
        (should-not (plist-get ev :from)))))))

(ert-deftest plan-polsl-events-test-create-unset ()
  (let ((plan-polsl-events-file nil))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (error "Should not ask")))
              ((symbol-function 'message) #'ignore))
      (should-not (plan-polsl-event-create)))))

(ert-deftest plan-polsl-events-test-at-point ()
  (plan-polsl-events-test--with-file
   (let ((test (nth 1 (plan-polsl-events-list))))
     (with-temp-buffer
       (insert (propertize "x" 'plan-polsl-entry (list :event test)))
       (goto-char (point-min))
       (should (eq (plan-polsl-events-at-point) test)))
     (with-temp-buffer
       (insert (propertize "Czwartek" 'plan-polsl-date "2026-11-19"))
       (goto-char (point-min))
       (should (equal (plist-get (plan-polsl-events-at-point) :title) "Kolokwium 1")))
     (with-temp-buffer
       (insert (propertize "Piątek" 'plan-polsl-date "2026-11-20"))
       (goto-char (point-min))
       (should-error (plan-polsl-events-at-point) :type 'user-error))
     (with-temp-buffer
       (should-error (plan-polsl-events-at-point) :type 'user-error)))))

(ert-deftest plan-polsl-events-test-at-point-choice ()
  (plan-polsl-events-test--with-file
   (with-temp-file plan-polsl-events-file
     (insert "* A :kolokwium:\n<2026-11-19 czw 10:00>\n* B :egzamin:\n<2026-11-19 czw 12:00>\n"))
   (with-temp-buffer
     (insert (propertize "Czwartek" 'plan-polsl-date "2026-11-19"))
     (goto-char (point-min))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt choices &rest _)
                  (car (cl-find-if (lambda (c) (string-match-p ": B " (car c))) choices)))))
       (should (equal (plist-get (plan-polsl-events-at-point) :title) "B"))))))

(ert-deftest plan-polsl-events-test-edit ()
  (plan-polsl-events-test--with-file
   (plan-polsl-events-test--cleanup
    (let ((plan-polsl-events-test--asked nil))
      (with-temp-buffer
        (insert (propertize "Czwartek" 'plan-polsl-date "2026-11-19"))
        (goto-char (point-min))
        (plan-polsl-events-test--answers '(("Godziny" . "15:00-16:00"))
                                         (plan-polsl-event-edit)))
      ;; every step is prefilled with the current values
      (should (equal (cdr (assoc "Typ wydarzenia: " plan-polsl-events-test--asked)) "Kolokwium"))
      (should (equal (cdr (assoc "Nazwa: " plan-polsl-events-test--asked)) "Kolokwium 1"))
      (should (equal (cdr (assoc "Sala (puste = brak): " plan-polsl-events-test--asked)) "416b"))
      (let ((ev (nth 1 (plan-polsl-events-list))))
        (should (equal (plist-get ev :title) "Kolokwium 1"))
        (should (equal (plist-get ev :from) "15:00"))
        (should (equal (plist-get ev :course) "Analiza danych i inteligencja obliczeniowa")))
      (should (string-match-p "Rozdziały 1-3" (plan-polsl-events-test--file-string)))))))

(ert-deftest plan-polsl-events-test-delete-command ()
  (plan-polsl-events-test--with-file
   (plan-polsl-events-test--cleanup
    (with-temp-buffer
      (insert (propertize "Czwartek" 'plan-polsl-date "2026-11-19"))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
        (should-not (plan-polsl-event-delete)))
      (should (= (length (plan-polsl-events-list)) 3))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (should (plan-polsl-event-delete)))
      (should (= (length (plan-polsl-events-list)) 2))))))

(ert-deftest plan-polsl-events-test-upcoming ()
  (plan-polsl-events-test--with-file
   (with-temp-file plan-polsl-events-file
     (insert "* Kolokwium 1 :kolokwium:\n<2026-11-19 czw 10:00>\n"
             "* Projekt :projekt:\n<2026-11-20 pią>\n"
             "* Rektorskie :rektorskie:\n<2026-11-19 czw>\n"
             "* Egzamin :egzamin:\n<2027-02-01 pon>\n"))
   (should (equal (plan-polsl-events-upcoming "2026-11-19")
                  "Najbliższe: Kolokwium 1 (19.11, dziś) • Projekt (20.11, jutro)"))
   (should (equal (plan-polsl-events-upcoming "2026-11-14")
                  "Najbliższe: Kolokwium 1 (19.11, za 5 dni) • Projekt (20.11, za 6 dni)"))
   (should-not (plan-polsl-events-upcoming "2026-11-21"))
   (let ((plan-polsl-events-upcoming-days 0))
     (should-not (plan-polsl-events-upcoming "2026-11-19")))))

(provide 'plan-polsl-events-test)
;;; plan-polsl-events-test.el ends here
