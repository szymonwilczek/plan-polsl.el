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

(ert-deftest plan-polsl-view-test-weekend-only-when-busy ()
  (plan-polsl-view-test--with-buffer (encode-time 0 0 0 5 10 2026)
                                     (should-not (search-forward "Sobota" nil t)))
  (let ((plan-polsl-view-test--entries
         (append plan-polsl-view-test--entries
                 (list (list :day-index 6 :start-time "09:00" :end-time "12:00"
                             :title "Zjazd" :type "Ćwiczenia" :dates '("10.10"))))))
    (plan-polsl-view-test--with-buffer (encode-time 0 0 0 5 10 2026)
                                       (should (search-forward "Sobota (10.10.2026)" nil t))
                                       (should (search-forward "Zjazd" nil t))
                                       (should-not (search-forward "Niedziela" nil t)))))

(ert-deftest plan-polsl-view-test-detail-usos-entry ()
  (let ((entry (list :day-index 1 :day-name "Poniedziałek" :date "2026-10-05"
                     :start-time "08:30" :end-time "10:00"
                     :title "Analiza matematyczna" :type "Wykład"
                     :cycle 'odd :biweekly t :dates '("05.10")
                     :groups '("gr. 1") :teachers '("dr inż. Jan Kowalski")
                     :rooms '("301") :building "Wydział AEiI"
                     :url "https://usosweb.polsl.pl/group"))
        (opened nil))
    (save-window-excursion
      (plan-polsl-view--display-detail-popup entry)
      (with-current-buffer "*Plan PolSL: Szczegóły*"
        (let ((text (buffer-string)))
          (should (string-match-p "Poniedziałek 05.10.2026, 08:30 - 10:00" text))
          (should (string-match-p "Tydzień Nieparzysty" text))
          (should (string-match-p "-> dr inż. Jan Kowalski" text))
          (should (string-match-p "Budynek: +Wydział AEiI" text)))
        (goto-char (point-min))
        (search-forward "Strona zajęć")
        (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (setq opened u))))
          (plan-polsl-detail-open-target))
        (should (equal opened "https://usosweb.polsl.pl/group")))
      (kill-buffer "*Plan PolSL: Szczegóły*"))))

(ert-deftest plan-polsl-view-test-usos-buffer-name ()
  (should (equal (plan-polsl-view--buffer-name "usos" 'usos '(:title "Jan Kowalski"))
                 "*Plan PolSL: USOS*"))
  (let ((plan-polsl-id "123"))
    (should (equal (plan-polsl-view--buffer-name "123" 0 nil) "*Plan PolSL*"))
    (should (equal (plan-polsl-view--buffer-name "9" 0 '(:title "Sala 301"))
                   "*Plan PolSL: Sala 301*"))))

(defmacro plan-polsl-view-test--with-usos (fetched &rest body)
  "Run BODY logged in to a stubbed USOS; FETCHED collects fetched weeks."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'plan-polsl-usos-logged-in-p) #'always)
             ((symbol-function 'plan-polsl-usos--load-token)
              (lambda () '(:token "t" :secret "s" :user-name "Jan Kowalski")))
             ((symbol-function 'plan-polsl-view--display-window) #'ignore)
             ((symbol-function 'plan-polsl-usos-fetch-week-async)
              (lambda (monday callback _errback)
                (let ((day (format-time-string "%d.%m" monday)))
                  (push day ,fetched)
                  (funcall callback
                           (list (list :day-index 1 :date (format-time-string "%F" monday)
                                       :start-time "08:30" :end-time "10:00"
                                       :title (concat "Zajęcia " day) :type "Wykład"
                                       :dates (list day))))))))
     (unwind-protect (progn ,@body)
       (when (get-buffer "*Plan PolSL: USOS*")
         (kill-buffer "*Plan PolSL: USOS*")))))

(ert-deftest plan-polsl-view-test-usos-weeks ()
  (let ((fetched nil))
    (plan-polsl-view-test--with-usos fetched
                                     (plan-polsl-usos nil (encode-time 0 0 0 5 10 2026))
                                     (with-current-buffer "*Plan PolSL: USOS*"
                                       (should (eq plan-polsl-view-type 'usos))
                                       (goto-char (point-min))
                                       (should (search-forward "Plan Zajęć: Jan Kowalski (USOS)" nil t))
                                       (should (search-forward "Zajęcia 05.10" nil t))
                                       (plan-polsl-next-week)
                                       (goto-char (point-min))
                                       (should (search-forward "Zajęcia 12.10" nil t))
                                       ;; going back hits the per-week cache
                                       (plan-polsl-prev-week)
                                       (goto-char (point-min))
                                       (should (search-forward "Zajęcia 05.10" nil t)))
                                     (should (equal fetched '("12.10" "05.10")))
                                     ;; reopening shows the live buffer without fetching
                                     (plan-polsl-usos)
                                     (should (= (length fetched) 2))
                                     ;; refresh (r) drops the cache and refetches the shown week
                                     (with-current-buffer "*Plan PolSL: USOS*"
                                       (plan-polsl-refresh))
                                     (should (equal fetched '("05.10" "12.10" "05.10"))))))

(ert-deftest plan-polsl-view-test-dispatch ()
  (let ((called nil))
    (cl-letf (((symbol-function 'plan-polsl-usos)
               (lambda (&rest _) (setq called 'usos)))
              ((symbol-function 'plan-polsl-view--open-polsl)
               (lambda (&rest _) (setq called 'polsl))))
      (cl-letf (((symbol-function 'plan-polsl-usos-logged-in-p) #'always))
        (let ((plan-polsl-usos-default t))
          (plan-polsl)
          (should (eq called 'usos))
          ;; explicit ID always means plan.polsl.pl
          (plan-polsl "600" 10)
          (should (eq called 'polsl)))
        (let ((plan-polsl-usos-default nil))
          (plan-polsl)
          (should (eq called 'polsl))))
      (cl-letf (((symbol-function 'plan-polsl-usos-logged-in-p) #'ignore))
        (let ((plan-polsl-usos-default t))
          (plan-polsl)
          (should (eq called 'polsl)))))))

(ert-deftest plan-polsl-view-test-usos-requires-login ()
  (cl-letf (((symbol-function 'plan-polsl-usos-logged-in-p) #'ignore))
    (should-error (plan-polsl-usos) :type 'user-error)))

(ert-deftest plan-polsl-view-test-require-plan ()
  (with-temp-buffer
    (should-error (plan-polsl-next-week) :type 'user-error)))

(provide 'plan-polsl-view-test)
;;; plan-polsl-view-test.el ends here
