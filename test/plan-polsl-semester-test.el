;;; plan-polsl-semester-test.el --- Tests for plan-polsl-semester -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for semester start resolution.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'plan-polsl-semester)

(defmacro plan-polsl-semester-test--with-file (&rest body)
  "Run BODY with a temporary `plan-polsl-semester-file' and no config."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "plan-polsl-semester" t))
          (plan-polsl-semester-file (expand-file-name "semester.eld" dir))
          (plan-polsl-semester-start nil))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(defun plan-polsl-semester-test--day (time)
  "Format TIME as YYYY-MM-DD."
  (format-time-string "%F" time))

(ert-deftest plan-polsl-semester-test-parse-date ()
  (should (equal (plan-polsl-semester-test--day
                  (plan-polsl-semester--parse-date "2026-10-01"))
                 "2026-10-01"))
  (should (equal (plan-polsl-semester-test--day
                  (plan-polsl-semester--parse-date " 1.10.2026 "))
                 "2026-10-01"))
  (should-not (plan-polsl-semester--parse-date "2026-02-30"))
  (should-not (plan-polsl-semester--parse-date "jutro"))
  (should-not (plan-polsl-semester--parse-date nil)))

(ert-deftest plan-polsl-semester-test-heuristic ()
  (plan-polsl-semester-test--with-file
   ;; the week of Monday 28.09.2026 contains October 1st
   (should (equal (plan-polsl-semester-test--day
                   (plan-polsl-semester-first-monday (encode-time 0 0 12 28 9 2026)))
                  "2026-09-28"))
   (should (equal (plan-polsl-semester-test--day
                   (plan-polsl-semester-start (encode-time 0 0 12 15 1 2027)))
                  "2026-10-01"))
   (should (equal (plan-polsl-semester-test--day
                   (plan-polsl-semester-start (encode-time 0 0 12 15 4 2027)))
                  "2027-03-02"))))

(ert-deftest plan-polsl-semester-test-saved-date ()
  (plan-polsl-semester-test--with-file
   (cl-letf (((symbol-function 'message) #'ignore))
     (plan-polsl-set-semester-start "23.02.2027"))
   (should (equal (plist-get (plan-polsl-store-read plan-polsl-semester-file) :start)
                  "2027-02-23"))
   (should (= (file-modes plan-polsl-semester-file) #o600))
   ;; used during that semester
   (should (equal (plan-polsl-semester-test--day
                   (plan-polsl-semester-start (encode-time 0 0 12 10 3 2027)))
                  "2027-02-23"))
   ;; ignored for a different semester, heuristic takes over
   (should (equal (plan-polsl-semester-test--day
                   (plan-polsl-semester-start (encode-time 0 0 12 15 10 2027)))
                  "2027-10-01"))
   (should-error (plan-polsl-set-semester-start "31.02.2027") :type 'user-error)))

(ert-deftest plan-polsl-semester-test-saved-beats-config ()
  (plan-polsl-semester-test--with-file
   (let ((plan-polsl-semester-start "2026-10-05"))
     (should (equal (plan-polsl-semester-test--day
                     (plan-polsl-semester-start (encode-time 0 0 12 20 10 2026)))
                    "2026-10-05"))
     (cl-letf (((symbol-function 'message) #'ignore))
       (plan-polsl-set-semester-start "2026-10-01"))
     (should (equal (plan-polsl-semester-test--day
                     (plan-polsl-semester-start (encode-time 0 0 12 20 10 2026)))
                    "2026-10-01")))))

(provide 'plan-polsl-semester-test)
;;; plan-polsl-semester-test.el ends here
