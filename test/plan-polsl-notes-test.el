;;; plan-polsl-notes-test.el --- Tests for plan-polsl-notes -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for course notes.

;;; Code:

(require 'cl-lib)
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

(ert-deftest plan-polsl-notes-test-directory-unset ()
  (let ((plan-polsl-notes-directory nil))
    (should-error (plan-polsl-notes--course-directory "Fizyka") :type 'user-error)))

(ert-deftest plan-polsl-notes-test-directory-created ()
  (plan-polsl-notes-test--with-dirs
    (let (initial)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt init) (setq initial init) "ADIIO-lab")))
        (should (equal (plan-polsl-notes--course-directory
                        "Analiza danych i inteligencja obliczeniowa")
                       (file-name-as-directory (expand-file-name "ADIIO-lab" notes)))))
      (should (equal initial "ADIIO"))
      (should (file-directory-p (expand-file-name "ADIIO-lab" notes)))
      ;; the chosen name is found next time without asking
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (error "Should not ask"))))
        (should (equal (plan-polsl-notes--course-directory
                        "Analiza danych i inteligencja obliczeniowa")
                       (file-name-as-directory (expand-file-name "ADIIO-lab" notes))))))))

(ert-deftest plan-polsl-notes-test-directory-existing ()
  (plan-polsl-notes-test--with-dirs
    (make-directory (expand-file-name "AM" notes))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) (error "Should not ask"))))
      (should (equal (plan-polsl-notes--course-directory "Analiza matematyczna")
                     (file-name-as-directory (expand-file-name "AM" notes)))))
    (should-not (plan-polsl-notes--saved-name "Analiza matematyczna"))))

(ert-deftest plan-polsl-notes-test-directory-empty-name ()
  (plan-polsl-notes-test--with-dirs
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "  ")))
      (should-error (plan-polsl-notes--course-directory "Fizyka") :type 'user-error))))

(provide 'plan-polsl-notes-test)
;;; plan-polsl-notes-test.el ends here
