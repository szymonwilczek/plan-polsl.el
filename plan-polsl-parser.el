;;; plan-polsl-parser.el --- HTML DOM and geometry parser -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, parser

;;; Commentary:
;; Parses plan.polsl.pl HTML course divs, extracts absolute geometry coordinates,
;; maps them to academic timetable slots, and extracts full course titles & teachers.

;;; Code:

(require 'cl-lib)
(require 'dom)

(defconst plan-polsl-parser-day-names
  ["Poniedziałek" "Wtorek" "Środa" "Czwartek" "Piątek"]
  "Names of days corresponding to 0-indexed column headers.")

(defconst plan-polsl-parser-grid-base-top 236
  "Top pixel coordinate corresponding to 08:00 on the PolSL schedule grid.")

(defconst plan-polsl-parser-grid-px-per-min 0.75
  "Vertical scaling factor in pixels per minute on standard 1920x1080 grid.")

(defconst plan-polsl-parser-grid-origin-left 88
  "Left pixel offset of Monday column on the PolSL schedule grid.")

(defconst plan-polsl-parser-grid-col-width 350
  "Horizontal day column width in pixels.")

(defconst plan-polsl-parser-standard-slots
  '((510 . 510)   ; 08:30
    (600 . 600)   ; 10:00
    (615 . 615)   ; 10:15
    (705 . 705)   ; 11:45
    (795 . 795)   ; 13:15
    (825 . 825)   ; 13:45
    (915 . 915)   ; 15:15
    (930 . 930)   ; 15:30
    (1020 . 1020) ; 17:00
    (1035 . 1035) ; 17:15
    (1125 . 1125) ; 18:45
    (1140 . 1140) ; 19:00
    (1230 . 1230) ; 20:30
    )
  "Standard class block boundary minutes from midnight.")

(defun plan-polsl-parser--style-prop (style prop &optional default)
  "Extract numerical or string value for CSS PROP in STYLE.
If DEFAULT is a number, returns integer; otherwise returns string."
  (let ((pattern (if (numberp default)
                     (format "%s:[ \t\n]*\\([0-9]+\\)" prop)
                   (format "%s:[ \t\n]*\\([^; \t\n]+\\)" prop))))
    (if (string-match pattern (or style ""))
        (if (numberp default)
            (string-to-number (match-string 1 style))
          (match-string 1 style))
      default)))

(defun plan-polsl-parser--snap-to-slot (minutes)
  "Snap raw MINUTES from midnight to the closest standard PolSL class slot."
  (or (cdr (cl-find-if (lambda (s) (<= (abs (- minutes (car s))) 14))
                       plan-polsl-parser-standard-slots))
      (* 5 (round (/ (float minutes) 5.0)))))

(defun plan-polsl-parser--coords-to-time (top height)
  "Convert TOP and HEIGHT pixel coordinates to (START-TIME . END-TIME) string pair."
  (let* ((raw-start (+ (* 8 60)
                       (round (/ (- top plan-polsl-parser-grid-base-top)
                                 plan-polsl-parser-grid-px-per-min))))
         (raw-dur (round (/ height plan-polsl-parser-grid-px-per-min)))
         (start-min (plan-polsl-parser--snap-to-slot raw-start))
         (end-min (plan-polsl-parser--snap-to-slot (+ raw-start raw-dur))))
    (cons (format "%02d:%02d" (/ start-min 60) (% start-min 60))
          (format "%02d:%02d" (/ end-min 60) (% end-min 60)))))

(defun plan-polsl-parser--coords-to-day (left)
  "Map LEFT pixel coordinate to 1-based day of week (1=Mon .. 5=Fri)."
  (cond
   ((< left 430) 1)   ; Poniedziałek
   ((< left 780) 2)   ; Wtorek
   ((< left 1130) 3)  ; Środa
   ((< left 1480) 4)  ; Czwartek
   (t 5)))            ; Piątek

(defun plan-polsl-parser--detect-cycle (width left)
  "Determine recurrence cycle (`weekly', `odd', or `even') based on WIDTH and LEFT."
  (if (> width 200)
      'weekly
    (let ((col-offset (mod (- left plan-polsl-parser-grid-origin-left)
                           plan-polsl-parser-grid-col-width)))
      (if (< col-offset 120) 'odd 'even))))

(defun plan-polsl-parser--extract-sections (text)
  "Extract lab/group section numbers from TEXT (e.g. \"sek.10,11\" -> \\='(\"10\" \"11\"))."
  (if (string-match "(sek\\.?[ \t\n]*\\([0-9, ]+\\))" (or text ""))
      (split-string (match-string 1 text) "[, ]+" t)
    nil))

(defun plan-polsl-parser--extract-dates (text)
  "Extract explicit class meeting dates from occurrence line in TEXT."
  (when (string-match "występowanie:[ \t\n]*\\([0-9., \t\n]+\\)" (or text ""))
    (delq nil (mapcar (lambda (d)
                        (let ((cl (string-trim d)))
                          (when (string-match-p "^[0-9]+\\.[0-9]+" cl)
                            cl)))
                      (split-string (match-string 1 text) "[, \t\n]+" t)))))

(defun plan-polsl-parser--detect-type (text bg-color)
  "Detect class type (Wykład, Lab, Ćwiczenia, Seminarium) from TEXT and BG-COLOR."
  (let ((ltext (downcase (or text ""))))
    (cond
     ((string-match-p "\\blab\\b" ltext) "Laboratorium")
     ((string-match-p "\\bwyk\\b" ltext) "Wykład")
     ((string-match-p "\\bsem\\b" ltext) "Seminarium")
     ((string-match-p "\\bćw\\b" ltext) "Ćwiczenia")
     ((string-match-p "\\bproj\\b" ltext) "Projekt")
     ((string-equal-ignore-case bg-color "#7bf78d") "Wykład")
     ((string-equal-ignore-case bg-color "#7cb0f6") "Laboratorium")
     ((string-equal-ignore-case bg-color "#a9fd43") "Seminarium")
     (t "Zajęcia"))))

(defun plan-polsl-parser--clean-title (text)
  "Extract concise course title from raw div TEXT."
  (let ((t-clean (or text "")))
    (setq t-clean (replace-regexp-in-string "^\\*+\\s*" "" t-clean))
    (setq t-clean (replace-regexp-in-string "(sek\\.?[^)]+)" "" t-clean))
    (setq t-clean (replace-regexp-in-string ",\\s*\\(lab\\|wyk\\|sem\\|ćw\\|proj\\).*" "" t-clean))
    (string-trim (or (car (split-string t-clean "[,\n]" t)) t-clean))))

(defun plan-polsl-parser--extract-links (div type-pattern)
  "Extract text of <a> links in DIV matching TYPE-PATTERN in href attribute."
  (delq nil (mapcar (lambda (a)
                      (let ((href (or (dom-attr a 'href) "")))
                        (when (string-match-p type-pattern href)
                          (string-trim (dom-texts a)))))
                    (dom-by-tag div 'a))))

(defun plan-polsl-parser--extract-teachers-info (div)
  "Extract structured list of (:id ID :initials INITIALS) for teachers in DIV."
  (delq nil (mapcar (lambda (a)
                      (let ((href (or (dom-attr a 'href) ""))
                            (initials (string-trim (dom-texts a))))
                        (when (and (string-match "type=10&id=\\([0-9]+\\)" href)
                                   (> (length initials) 0))
                          (list :id (match-string 1 href)
                                :initials initials))))
                    (dom-by-tag div 'a))))

(defun plan-polsl-parser--extract-rooms-info (div)
  "Extract structured list of (:id ID :name NAME) for rooms in DIV."
  (delq nil (mapcar (lambda (a)
                      (let ((href (or (dom-attr a 'href) ""))
                            (rname (string-trim (dom-texts a))))
                        (when (and (string-match "type=20&id=\\([0-9]+\\)" href)
                                   (> (length rname) 0))
                          (list :id (match-string 1 href)
                                :name rname))))
                    (dom-by-tag div 'a))))

(defun plan-polsl-parser--extract-legend (html)
  "Extract legend mapping abbreviation to full course name from HTML."
  (let ((legend nil)
        (pos 0))
    (while (string-match "<strong>\\([^<]+\\)</strong>[ \t\n]*-[ \t\n]*\\([^<]+\\)" html pos)
      (let* ((abbrev (string-trim (match-string 1 html)))
             (raw-full (string-trim (match-string 2 html)))
             (clean-full (replace-regexp-in-string ",[ \t\n]*\\*-[ \t\n]*obieralny.*" "" raw-full))
             (clean-full (replace-regexp-in-string "^\\*+[ \t\n]*" "" clean-full))
             (clean-full (replace-regexp-in-string ",[ \t\n]*Zajęcia wg.*" "" clean-full))
             (end (match-end 0)))
        (push (cons abbrev (string-trim clean-full)) legend)
        (setq pos end)))
    (nreverse legend)))

(defun plan-polsl-parser--match-full-title (raw-text title legend)
  "Match RAW-TEXT or TITLE against LEGEND dictionary to find unabbreviated title."
  (or (cl-some (lambda (pair)
                 (when (string-prefix-p (car pair) raw-text)
                   (cdr pair)))
               legend)
      (cl-some (lambda (pair)
                 (let ((clean-k (replace-regexp-in-string "^\\*+[ \t\n]*" "" (car pair))))
                   (setq clean-k (car (split-string clean-k "[(,]" t)))
                   (when (string-equal (string-trim clean-k) (string-trim title))
                     (cdr pair))))
               legend)
      title))

(defun plan-polsl-parser-extract-metadata (html)
  "Extract schedule title and faculty path from HTML."
  (let ((title nil)
        (path nil))
    (with-temp-buffer
      (insert (or html ""))
      (goto-char (point-min))
      ;; title
      (when (re-search-forward "Plan zajęć[ \t\n]*-[ \t\n]*\\([^\r\n<]+\\)" nil t)
        (setq title (string-trim (match-string 1))))
      ;; faculty breadcrumb
      (goto-char (point-min))
      (when (re-search-forward "\\(?:Grupy\\|Nauczyciele\\|Sale\\)[ \t\n]*\\\\[ \t\n]*\\([^\r\n<]+\\)" nil t)
        (setq path (string-trim (match-string 1)))))
    (list :title (or title "Plan Zajęć")
          :path path)))

(defun plan-polsl-parser-parse-entries (html)
  "Parse HTML from plan.polsl.pl using DOM.
Returns list of structured class entries."
  (let* ((dom (with-temp-buffer
                (insert html)
                (libxml-parse-html-region (point-min) (point-max))))
         (legend (plan-polsl-parser--extract-legend html))
         (divs (dom-by-class dom "^coursediv$"))
         (entries nil))
    (dolist (div divs)
      (let* ((raw-text (dom-texts div " "))
             (trimmed (string-trim (or raw-text ""))))
        (when (> (length trimmed) 1)
          (let* ((style (or (dom-attr div 'style) ""))
                 (top (plan-polsl-parser--style-prop style "top" 0))
                 (left (plan-polsl-parser--style-prop style "left" 0))
                 (width (plan-polsl-parser--style-prop style "width" 338))
                 (height (plan-polsl-parser--style-prop style "height" 0))
                 (bg (plan-polsl-parser--style-prop style "background-color" "#ffffff"))
                 (cycle (plan-polsl-parser--detect-cycle width left))
                 (dates (plan-polsl-parser--extract-dates trimmed))
                 (groups (delete-dups (plan-polsl-parser--extract-links div "type=0\\|type=1\\b")))
                 (teachers (delete-dups (plan-polsl-parser--extract-links div "type=10")))
                 (teachers-info (plan-polsl-parser--extract-teachers-info div))
                 (rooms (delete-dups (plan-polsl-parser--extract-links div "type=20")))
                 (rooms-info (plan-polsl-parser--extract-rooms-info div))
                 (time-pair (plan-polsl-parser--coords-to-time top height))
                 (day-idx (plan-polsl-parser--coords-to-day left))
                 (day-name (aref plan-polsl-parser-day-names (1- day-idx)))
                 (sections (plan-polsl-parser--extract-sections trimmed))
                 (class-type (plan-polsl-parser--detect-type trimmed bg))
                 (biweekly (or (eq cycle 'odd) (eq cycle 'even) (string-match-p "\\*\\s*[A-Za-z]" trimmed)))
                 (title (plan-polsl-parser--clean-title trimmed))
                 (full-title (plan-polsl-parser--match-full-title trimmed title legend)))
            (push (list :day-index day-idx
                        :day-name day-name
                        :start-time (car time-pair)
                        :end-time (cdr time-pair)
                        :title title
                        :full-title full-title
                        :type class-type
                        :sections sections
                        :biweekly (and biweekly t)
                        :cycle cycle
                        :dates dates
                        :groups groups
                        :teachers teachers
                        :teachers-info teachers-info
                        :rooms rooms
                        :rooms-info rooms-info
                        :raw-text trimmed
                        :top top
                        :left left
                        :width width
                        :height height)
                  entries)))))

    ;; sort chronologically by day and start time
    (sort (nreverse entries)
          (lambda (a b)
            (if (= (plist-get a :day-index) (plist-get b :day-index))
                (string< (plist-get a :start-time) (plist-get b :start-time))
              (< (plist-get a :day-index) (plist-get b :day-index)))))))

(provide 'plan-polsl-parser)
;;; plan-polsl-parser.el ends here
