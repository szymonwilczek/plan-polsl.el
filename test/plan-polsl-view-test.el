;;; plan-polsl-view-test.el --- Tests for plan-polsl-view -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests rendering the timetable buffer from synthetic entries.

;;; Code:

(require 'ert)
(require 'plan-polsl)

(defconst plan-polsl-view-test--entries
  (list (list :day-index 1 :day-name "Poniedziałek"
              :start-time "08:30" :end-time "10:00"
              :title "AiR" :full-title "Automatyka i Robotyka" :type "Wykład"
              :cycle 'weekly :rooms '("301") :teachers '("JK"))
        (list :day-index 3 :day-name "Środa"
              :start-time "10:15" :end-time "11:45"
              :title "PSy" :type "Laboratorium" :cycle 'odd :biweekly t))
  "Synthetic plan.polsl.pl style entries.")

(defmacro plan-polsl-view-test--with-buffer (monday &rest body)
  "Render test entries for MONDAY into a temporary buffer and run BODY there."
  (declare (indent 1))
  `(let ((buf (plan-polsl-view--render-buffer
               plan-polsl-view-test--entries '(:title "Test") "123" 0 ,monday
               " *plan-polsl-view-test*")))
     (unwind-protect
         (with-current-buffer buf
           (cl-letf (((symbol-function 'plan-polsl-view--display-window) #'ignore))
             ,@body))
       (kill-buffer buf))))

(ert-deftest plan-polsl-view-test-render ()
  ;; 2026-10-05 is the Monday of week 1 (odd)
  (plan-polsl-view-test--with-buffer (encode-time 0 0 0 5 10 2026)
                                     (should (derived-mode-p 'plan-polsl-mode))
                                     (should (search-forward "Poniedziałek (05.10.2026)" nil t))
                                     (should (search-forward "08:30 - 10:00" nil t))
                                     (should (search-forward "* PSy" nil t))))

(ert-deftest plan-polsl-view-test-week-navigation ()
  (plan-polsl-view-test--with-buffer (encode-time 0 0 0 5 10 2026)
                                     (plan-polsl-next-week)
                                     (should (equal (format-time-string "%F" plan-polsl-view-active-monday) "2026-10-12"))
                                     (goto-char (point-min))
                                     (should (search-forward "Poniedziałek (12.10.2026)" nil t))
                                     ;; odd-week lab is hidden in the even week
                                     (goto-char (point-min))
                                     (should-not (search-forward "PSy" nil t))
                                     (plan-polsl-prev-week)
                                     (should (equal (format-time-string "%F" plan-polsl-view-active-monday) "2026-10-05"))))

(ert-deftest plan-polsl-view-test-require-plan ()
  (with-temp-buffer
    (should-error (plan-polsl-next-week) :type 'user-error)))

(provide 'plan-polsl-view-test)
;;; plan-polsl-view-test.el ends here
