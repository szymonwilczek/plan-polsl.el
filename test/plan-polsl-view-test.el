;;; plan-polsl-view-test.el --- Tests for plan-polsl-view -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests rendering the timetable buffer from synthetic entries.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'plan-polsl)

;; never read the semester start saved by the developer running the tests
(setq plan-polsl-semester-file
      (expand-file-name "plan-polsl-test-none/semester.eld" temporary-file-directory))

(defconst plan-polsl-view-test--entries
  (list (list :day-index 1 :day-name "Poniedziałek"
              :start-time "08:30" :end-time "10:00"
              :title "AiR" :full-title "Automatyka i Robotyka" :type "Wykład"
              :cycle 'weekly :rooms '("301") :teachers '("JK"))
        (list :day-index 3 :day-name "Środa"
              :start-time "10:15" :end-time "11:45"
              :title "PSy" :type "Laboratorium" :cycle 'even :biweekly t))
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
  ;; 2026-10-05 is the Monday of week 2 (even)
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
                                     ;; even-week lab is hidden in the odd week
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

(ert-deftest plan-polsl-view-test-prefix-forces-plan-polsl ()
  (let ((args nil)
        (plan-polsl-id "343266256")
        (plan-polsl-usos-default t))
    (cl-letf (((symbol-function 'plan-polsl-usos-logged-in-p) #'always)
              ((symbol-function 'plan-polsl-usos) (lambda (&rest _) (setq args 'usos)))
              ((symbol-function 'plan-polsl-view--open-polsl)
               (lambda (&rest a) (setq args a))))
      (let ((current-prefix-arg '(4)))
        (call-interactively #'plan-polsl))
      (should (equal args '("343266256" nil nil nil)))
      (let ((current-prefix-arg nil))
        (call-interactively #'plan-polsl))
      (should (eq args 'usos)))))

(ert-deftest plan-polsl-view-test-usos-requires-login ()
  (cl-letf (((symbol-function 'plan-polsl-usos-logged-in-p) #'ignore))
    (should-error (plan-polsl-usos) :type 'user-error)))

(ert-deftest plan-polsl-view-test-winter-week-numbers ()
  ;; October 1st 2026 is a Thursday, its week is week 1
  (let ((w1 (plan-polsl-view--week-info (encode-time 0 0 0 28 9 2026)))
        (w2 (plan-polsl-view--week-info (encode-time 0 0 0 5 10 2026)))
        (jan (plan-polsl-view--week-info (encode-time 0 0 0 11 1 2027))))
    (should (= (plist-get w1 :week-num) 1))
    (should (eq (plist-get w1 :cycle) 'odd))
    (should (string-match-p "Tydzień 1, Nieparzysty" (plist-get w1 :label)))
    (should (= (plist-get w2 :week-num) 2))
    (should (eq (plist-get w2 :cycle) 'even))
    (should (= (plist-get jan :week-num) 16))))

(ert-deftest plan-polsl-view-test-saved-semester-start ()
  ;; a later start announced by the dean moves week 1 and the parity
  (let* ((dir (make-temp-file "plan-polsl-view" t))
         (plan-polsl-semester-file (expand-file-name "semester.eld" dir)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'message) #'ignore))
            (plan-polsl-set-semester-start "2026-10-05"))
          (let ((before (plan-polsl-view--week-info (encode-time 0 0 0 28 9 2026)))
                (w1 (plan-polsl-view--week-info (encode-time 0 0 0 5 10 2026))))
            (should (string-match-p "Poza semestrem" (plist-get before :label)))
            (should (= (plist-get w1 :week-num) 1))
            (should (eq (plist-get w1 :cycle) 'odd))))
      (delete-directory dir t))))

(ert-deftest plan-polsl-view-test-wrap-meta ()
  (should (equal (plan-polsl-view--wrap-meta '("Sala: 827" "Prow: A B, C D") nil)
                 '("Sala: 827 • Prow: A B, C D")))
  (should (equal (plan-polsl-view--wrap-meta '("Sala: 827" "Prow: Dr A, Dr B, Dr C") 20)
                 '("Sala: 827" "Prow: Dr A, Dr B," "Dr C")))
  ;; lists break after commas, not inside a name that fits on a line
  (should (equal (plan-polsl-view--wrap-meta
                  '("Grupy: gr. 10" "Prow: Dr hab. inż. Adam Ziębiński, Dr inż. Dariusz Caban") 36)
                 '("Grupy: gr. 10"
                   "Prow: Dr hab. inż. Adam Ziębiński,"
                   "Dr inż. Dariusz Caban")))
  ;; a name longer than the width breaks between words
  (should (equal (plan-polsl-view--wrap-meta
                  '("Grupy: gr. 10" "Prow: Dr hab. inż. Adam Ziębiński, Dr inż. Dariusz Caban") 30)
                 '("Grupy: gr. 10 • Prow: Dr hab."
                   "inż. Adam Ziębiński,"
                   "Dr inż. Dariusz Caban")))
  ;; a part too long for any line breaks between words, never inside one
  (should (equal (plan-polsl-view--wrap-meta
                  '("Sala: 827" "Grupy: Informatyka sem. 5 grupa dziekańska 4") 20)
                 '("Sala: 827 • Grupy:"
                   "Informatyka sem. 5"
                   "grupa dziekańska 4"))))

(ert-deftest plan-polsl-view-test-wrapped-entry-alignment ()
  (let* ((entry (list :start-time "11:00" :end-time "14:00" :title "Budowa komputerów"
                      :type "Laboratorium" :groups '("gr. 10") :rooms '("827")
                      :teachers '("Dr hab. inż. Adam Ziębiński" "Dr hab. inż. Michał Maćkowski"
                                  "Dr inż. Dariusz Caban" "Dr hab. inż. Rafał Cupek")))
         (lines (split-string (plan-polsl-view--format-entry-line entry 20 100) "\n"))
         (bar (string-match " │ " (car lines))))
    (should (> (length lines) 1))
    (dolist (l lines)
      (should (<= (string-width l) 100))
      (should (eq (string-match " │ " l) bar)))
    (dolist (l (cdr lines))
      (should (string-blank-p (substring l 0 bar))))))

(ert-deftest plan-polsl-view-test-wrap-words ()
  (should (equal (plan-polsl-view--wrap-words "Budowa komputerów" nil)
                 '("Budowa komputerów")))
  (should (equal (plan-polsl-view--wrap-words
                  "Analiza danych i inteligencja obliczeniowa" 20)
                 '("Analiza danych i" "inteligencja" "obliczeniowa")))
  (should (equal (plan-polsl-view--wrap-words "Elektrotechnika teoretyczna" 10)
                 '("Elektrotechnika" "teoretyczna")))
  (let ((line (car (plan-polsl-view--wrap-words
                    (propertize "Analiza danych" 'face 'bold) 8))))
    (should (eq (get-text-property 0 'face line) 'bold))))

(ert-deftest plan-polsl-view-test-wrapped-subject ()
  (let* ((entry (list :start-time "14:00" :end-time "16:15" :type "Laboratorium"
                      :title "Analiza danych i inteligencja obliczeniowa"
                      :rooms '("416b")
                      :teachers '("Dr inż. Łukasz Wróbel" "Dr inż. Michał Kozielski")))
         (lines (split-string (plan-polsl-view--format-entry-line entry 20 80) "\n"))
         (bar (string-match " │" (car lines))))
    (should (= (length lines) 3))
    (should (string-match-p "Analiza danych i *│" (nth 0 lines)))
    (should (string-match-p "^ +inteligencja *│" (nth 1 lines)))
    (should (string-match-p "^ +obliczeniowa *│" (nth 2 lines)))
    (dolist (l lines)
      (should (<= (string-width l) 80))
      (should (eq (string-match " │" l) bar))))
  ;; without meta data the name wraps alone
  (should (equal (length (split-string
                          (plan-polsl-view--format-entry-line
                           (list :start-time "08:00" :end-time "09:30" :type "Wykład"
                                 :title "Analiza danych i inteligencja obliczeniowa")
                           20 80)
                          "\n"))
                 3)))

(ert-deftest plan-polsl-view-test-subject-width-limit ()
  (let ((days (make-vector 7 nil)))
    (aset days 0 (list (list :title (make-string 80 ?x))))
    (should (= (plan-polsl-view--compute-subject-width days) 80))
    (should (= (plan-polsl-view--compute-subject-width days 120) 54))
    (should (= (plan-polsl-view--compute-subject-width days 40) 18))
    (aset days 0 (list (list :title "Fizyka")))
    (should (= (plan-polsl-view--compute-subject-width days 120) 18))))

(ert-deftest plan-polsl-view-test-wrap-keys ()
  (should (equal (plan-polsl-view--wrap-keys '("[q] Zamknij" "[r] Odśwież") nil)
                 '("  [q] Zamknij   [r] Odśwież")))
  (should (equal (plan-polsl-view--wrap-keys
                  '("[q] Zamknij" "[r] Odśwież" "[< / >] Tygodnie") 30)
                 '("  [q] Zamknij   [r] Odśwież" "  [< / >] Tygodnie")))
  (dolist (l (plan-polsl-view--wrap-keys plan-polsl-view--header-keys 120))
    (should (<= (string-width l) 120))))

(ert-deftest plan-polsl-view-test-width-limit ()
  (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 200)))
    (let ((plan-polsl-view-width 120))
      (should (= (plan-polsl-view--width (current-buffer)) 120)))
    (let ((plan-polsl-view-width 300))
      (should (= (plan-polsl-view--width (current-buffer)) 199)))
    (let ((plan-polsl-view-width nil))
      (should (= (plan-polsl-view--width (current-buffer)) 199))))
  (let ((plan-polsl-view-width 60))
    (plan-polsl-view-test--with-buffer (encode-time 0 0 0 5 10 2026)
                                       (should (= plan-polsl-view--rendered-width 60))
                                       (should (search-forward (make-string 60 ?─) nil t))
                                       (should-not (search-forward (make-string 61 ?─) nil t)))))

(ert-deftest plan-polsl-view-test-rerender-on-resize ()
  (plan-polsl-view-test--with-buffer (encode-time 0 0 0 5 10 2026)
                                     (save-window-excursion
                                       (switch-to-buffer (current-buffer))
                                       (let ((width plan-polsl-view--rendered-width))
                                         (should (memq #'plan-polsl-view--on-resize window-size-change-functions))
                                         ;; pretend it was rendered for another width
                                         (setq plan-polsl-view--rendered-width 30)
                                         (forward-line 5)
                                         (let ((line (line-number-at-pos)))
                                           (plan-polsl-view--on-resize (selected-window))
                                           (should (= plan-polsl-view--rendered-width width))
                                           (should (= (line-number-at-pos) line))
                                           (should (search-forward "PSy" nil t)))))))

(ert-deftest plan-polsl-view-test-require-plan ()
  (with-temp-buffer
    (should-error (plan-polsl-next-week) :type 'user-error)))

(provide 'plan-polsl-view-test)
;;; plan-polsl-view-test.el ends here
