;;; plan-polsl-notes-test.el --- Tests for plan-polsl-notes -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for course notes.

;;; Code:

(require 'ert)
(require 'plan-polsl-notes)

(ert-deftest plan-polsl-notes-test-abbreviation ()
  (should (equal (plan-polsl-notes--abbreviation
                  "Analiza danych i inteligencja obliczeniowa")
                 "ADIIO"))
  (should (equal (plan-polsl-notes--abbreviation "Ćwiczenia z fizyki 2") "ĆZF2"))
  (should (equal (plan-polsl-notes--abbreviation "Sieci (projekt), część I") "SPCI"))
  (should (equal (plan-polsl-notes--abbreviation "PSy") "PSy"))
  (should (equal (plan-polsl-notes--abbreviation "  ") "")))

(defmacro plan-polsl-notes-test--with-dirs (&rest body)
  "Run BODY with temporary notes and data directories bound to `notes'."
  (declare (indent 0))
  `(let* ((notes (make-temp-file "plan-polsl-notes" t))
          (plan-polsl-notes-directory notes)
          (plan-polsl-notes-file (expand-file-name "data/notes.eld" notes)))
     (unwind-protect (progn ,@body)
       (delete-directory notes t))))

(ert-deftest plan-polsl-notes-test-saved-name ()
  (plan-polsl-notes-test--with-dirs
   (should-not (plan-polsl-notes--saved-name "Fizyka"))
   (plan-polsl-notes--save-name "Fizyka" "fiz")
   (plan-polsl-notes--save-name "Analiza matematyczna" "AM")
   (plan-polsl-notes--save-name "Fizyka" "FIZ")
   (should (equal (plan-polsl-notes--saved-name "Fizyka") "FIZ"))
   (should (equal (plan-polsl-notes--saved-name "Analiza matematyczna") "AM"))
   (should (= (file-modes plan-polsl-notes-file) #o600))))

(provide 'plan-polsl-notes-test)
;;; plan-polsl-notes-test.el ends here
