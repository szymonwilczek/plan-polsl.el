;;; plan-polsl-org-test.el --- Tests for plan-polsl-org -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for Org document generation.

;;; Code:

(require 'ert)
(require 'plan-polsl)

(ert-deftest plan-polsl-org-test-recurring-timestamp ()
  (let ((plan-polsl-semester-start "2026-10-05"))
    (should (equal (plan-polsl-org--format-timestamp 3 "10:15" "11:45")
                   "<2026-10-07 śro 10:15-11:45 +1w>"))
    (should (equal (plan-polsl-org--format-timestamp 1 "08:30" "10:00" t)
                   "<2026-10-05 pon 08:30-10:00 +2w>"))))

(ert-deftest plan-polsl-org-test-date-timestamp ()
  (should (equal (plan-polsl-org--format-date-timestamp "2026-10-05" 1 "08:30" "10:00")
                 "<2026-10-05 pon 08:30-10:00>"))
  (should (equal (plan-polsl-org--format-date-timestamp "2026-10-10" 6 "09:00" "12:00")
                 "<2026-10-10 sob 09:00-12:00>")))

(provide 'plan-polsl-org-test)
;;; plan-polsl-org-test.el ends here
