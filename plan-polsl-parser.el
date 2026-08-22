;;; plan-polsl-parser.el --- HTML and coordinate grid parser -*- lexical-binding: t; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, parser

;;; Commentary:
;; Parses plan.polsl.pl HTML course divs, extracts absolute geometry coordinates,
;; and maps them to academic timetable slots, days, and metadata.

;;; Code:

(require 'cl-lib)
(require 'dom)

(defconst plan-polsl-parser-day-names
  ["Poniedziałek" "Wtorek" "Środa" "Czwartek" "Piątek"]
  "Names of days corresponding to 0-indexed column headers.")

(defun plan-polsl-parser-coords-to-day (left)
  "Map LEFT coordinate in pixels to 1-based day of week (1=Mon .. 5=Fri)."
  (cond
   ((< left 430) 1)   ; Poniedziałek
   ((< left 780) 2)   ; Wtorek
   ((< left 1130) 3)  ; Środa
   ((< left 1480) 4)  ; Czwartek
   (t 5)))            ; Piątek

(defun plan-polsl-parser-snap-to-slot (minutes)
  "Snap raw MINUTES from midnight to standard PolSL class slot boundaries."
  (let ((slots '((510 . 510)   ; 08:30
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
                 )))
    (or (cdr (cl-find-if (lambda (s) (<= (abs (- minutes (car s))) 14)) slots))
        (* 5 (round (/ (float minutes) 5.0))))))

(defun plan-polsl-parser-coords-to-time (top height)
  "Convert TOP and HEIGHT pixel coordinates to (START-STR . END-STR)."
  (let* ((base-top 236)
         (px-per-min 0.75)
         (raw-start-min (+ (* 8 60) (round (/ (- top base-top) px-per-min))))
         (raw-dur-min (round (/ height px-per-min)))
         (start-min (plan-polsl-parser-snap-to-slot raw-start-min))
         (end-min (plan-polsl-parser-snap-to-slot (+ raw-start-min raw-dur-min)))
         (start-h (/ start-min 60))
         (start-m (% start-min 60))
         (end-h (/ end-min 60))
         (end-m (% end-min 60)))
    (cons (format "%02d:%02d" start-h start-m)
          (format "%02d:%02d" end-h end-m))))

(defun plan-polsl-parser-extract-sections (text)
  "Extract lab/group section numbers from TEXT (e.g. \"sek.10,11\" or \"sek4\" -> '(\"10\" \"11\"))."
  (if (string-match "(sek\\.?[ \t\n]*\\([0-9, ]+\\))" (or text ""))
      (split-string (match-string 1 text) "[, ]+" t)
    nil))

(defun plan-polsl-parser-extract-type (text bg-color)
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

(defun plan-polsl-parser-parse-entries (html)
  "Parse HTML from plan.polsl.pl using DOM and return list of structured class entries."
  (let* ((dom (with-temp-buffer
                (insert html)
                (libxml-parse-html-region (point-min) (point-max))))
         (divs (dom-by-class dom "^coursediv$"))
         (entries nil))
    (dolist (div divs)
      (let* ((raw-text (dom-texts div " "))
             (trimmed (string-trim (or raw-text ""))))
        (when (> (length trimmed) 1)
          (let* ((style (or (dom-attr div 'style) ""))
                 (top (if (string-match "top:[ \t\n]*\\([0-9]+\\)px" style)
                          (string-to-number (match-string 1 style)) 0))
                 (left (if (string-match "left:[ \t\n]*\\([0-9]+\\)px" style)
                           (string-to-number (match-string 1 style)) 0))
                 (height (if (string-match "height:[ \t\n]*\\([0-9]+\\)px" style)
                             (string-to-number (match-string 1 style)) 0))
                 (bg (if (string-match "background-color:[ \t\n]*\\(#[a-fA-F0-9]+\\)" style)
                         (match-string 1 style) "#ffffff"))
                 (links (dom-by-tag div 'a))
                 (teachers (delq nil (mapcar (lambda (a)
                                               (let ((href (or (dom-attr a 'href) "")))
                                                 (when (string-match-p "type=10" href)
                                                   (string-trim (dom-texts a)))))
                                             links)))
                 (rooms (delq nil (mapcar (lambda (a)
                                            (let ((href (or (dom-attr a 'href) "")))
                                              (when (string-match-p "type=20" href)
                                                (string-trim (dom-texts a)))))
                                          links)))
                 (time-pair (plan-polsl-parser-coords-to-time top height))
                 (day-idx (plan-polsl-parser-coords-to-day left))
                 (day-name (aref plan-polsl-parser-day-names (1- day-idx)))
                 (sections (plan-polsl-parser-extract-sections trimmed))
                 (class-type (plan-polsl-parser-extract-type trimmed bg))
                 (biweekly (string-match-p "\\*\\s*[A-Za-z]" trimmed))
                 (title (let ((t-clean trimmed))
                          (setq t-clean (replace-regexp-in-string "^\\*\\s*" "" t-clean))
                          (setq t-clean (replace-regexp-in-string "(sek\\.?[^)]+)" "" t-clean))
                          (setq t-clean (replace-regexp-in-string ",\\s*\\(lab\\|wyk\\|sem\\|ćw\\|proj\\).*" "" t-clean))
                          (string-trim (or (car (split-string t-clean "[,\n]" t)) t-clean)))))
            (push (list :day-index day-idx
                        :day-name day-name
                        :start-time (car time-pair)
                        :end-time (cdr time-pair)
                        :title (string-trim title)
                        :type class-type
                        :sections sections
                        :biweekly (and biweekly t)
                        :teachers (delete-dups teachers)
                        :rooms (delete-dups rooms)
                        :raw-text trimmed
                        :top top
                        :left left
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
