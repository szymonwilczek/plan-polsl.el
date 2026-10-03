;;; plan-polsl-parser-test.el --- Tests for plan-polsl-parser -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the plan.polsl.pl geometry and text helpers.

;;; Code:

(require 'ert)
(require 'plan-polsl-parser)

(ert-deftest plan-polsl-parser-test-snap-to-slot ()
  (should (= (plan-polsl-parser--snap-to-slot 505) 510))
  (should (= (plan-polsl-parser--snap-to-slot 618) 615))
  (should (= (plan-polsl-parser--snap-to-slot 482) 480)))

(ert-deftest plan-polsl-parser-test-coords-to-time ()
  ;; top 236 is 08:00, 0.75 px per minute
  (should (equal (plan-polsl-parser--coords-to-time 258 68)
                 '("08:30" . "10:00"))))

(ert-deftest plan-polsl-parser-test-coords-to-day ()
  (should (= (plan-polsl-parser--coords-to-day 88) 1))
  (should (= (plan-polsl-parser--coords-to-day 800) 3))
  (should (= (plan-polsl-parser--coords-to-day 1500) 5)))

(ert-deftest plan-polsl-parser-test-detect-cycle ()
  (should (eq (plan-polsl-parser--detect-cycle 338 88) 'weekly))
  (should (eq (plan-polsl-parser--detect-cycle 168 88) 'odd))
  (should (eq (plan-polsl-parser--detect-cycle 168 258) 'even)))

(ert-deftest plan-polsl-parser-test-extract-sections ()
  (should (equal (plan-polsl-parser--extract-sections "AiR lab (sek.10,11)")
                 '("10" "11")))
  (should-not (plan-polsl-parser--extract-sections "AiR wyk")))

(provide 'plan-polsl-parser-test)
;;; plan-polsl-parser-test.el ends here
