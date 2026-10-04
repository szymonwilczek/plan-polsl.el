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

(provide 'plan-polsl-notes-test)
;;; plan-polsl-notes-test.el ends here
