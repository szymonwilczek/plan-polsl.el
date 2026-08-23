;;; plan-polsl-view.el --- Timetable buffer viewer and navigation mode -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, view

;;; Commentary:
;; Dedicated buffer viewer and navigation mode for browsing Politechnika Śląska timetables.

;;; Code:

(require 'cl-lib)
(require 'time-date)
(require 'plan-polsl-http)
(require 'plan-polsl-parser)

;; external references for clean byte-compilation
(declare-function evil-define-key "evil-core")
(declare-function plan-polsl-sync "plan-polsl-ui")
(declare-function plan-polsl-search--get-teachers "plan-polsl-search")

(defgroup plan-polsl-faces nil
  "Faces for `plan-polsl-mode'."
  :group 'plan-polsl)

(defface plan-polsl-day-face
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1))
  "Face for day of the week headings."
  :group 'plan-polsl-faces)

(defface plan-polsl-time-face
  '((t :inherit font-lock-constant-face :weight medium))
  "Face for class time ranges."
  :group 'plan-polsl-faces)

(defface plan-polsl-title-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for course title."
  :group 'plan-polsl-faces)

(defface plan-polsl-lecture-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for lecture badges."
  :group 'plan-polsl-faces)

(defface plan-polsl-lab-face
  '((t :inherit font-lock-string-face :weight bold))
  "Face for laboratory badges."
  :group 'plan-polsl-faces)

(defface plan-polsl-seminar-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for seminar badges."
  :group 'plan-polsl-faces)

(defface plan-polsl-meta-face
  '((t :inherit font-lock-comment-face))
  "Face for room, teacher, and section metadata."
  :group 'plan-polsl-faces)

(defvar plan-polsl-cached-entries nil
  "In-memory cache of parsed timetable entries.")

(defvar plan-polsl-cached-meta nil
  "In-memory cache of schedule metadata.")

(defvar plan-polsl-cached-id nil
  "Identifier corresponding to `plan-polsl-cached-entries'.")

(defvar plan-polsl-cached-type nil
  "Type corresponding to `plan-polsl-cached-entries'.")

(defvar plan-polsl-view-active-monday nil
  "Active Monday timestamp for week-by-week timetable viewing.")

(defvar plan-polsl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "r") #'plan-polsl-refresh)
    (define-key map (kbd "s") #'plan-polsl-sync)
    (define-key map (kbd "t") #'plan-polsl-current-week)
    (define-key map (kbd "<") #'plan-polsl-prev-week)
    (define-key map (kbd ">") #'plan-polsl-next-week)
    (define-key map (kbd "RET") #'plan-polsl-view-show-detail)
    (define-key map (kbd "<return>") #'plan-polsl-view-show-detail)
    (define-key map (kbd "<mouse-2>") #'plan-polsl-view-show-detail)
    map)
  "Keymap for `plan-polsl-mode'.")

(with-eval-after-load 'evil
  (evil-define-key '(normal visual motion) plan-polsl-mode-map
    "q" #'quit-window
    "r" #'plan-polsl-refresh
    "s" #'plan-polsl-sync
    "t" #'plan-polsl-current-week
    "<" #'plan-polsl-prev-week
    ">" #'plan-polsl-next-week
    (kbd "RET") #'plan-polsl-view-show-detail
    (kbd "<return>") #'plan-polsl-view-show-detail))

(define-derived-mode plan-polsl-mode special-mode "Plan-PolSL"
  "Major mode for browsing PolSL university timetables."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defvar plan-polsl-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'plan-polsl-detail-quit)
    (define-key map (kbd "RET") #'plan-polsl-detail-open-teacher)
    (define-key map (kbd "<return>") #'plan-polsl-detail-open-teacher)
    (define-key map (kbd "<mouse-2>") #'plan-polsl-detail-open-teacher)
    map)
  "Keymap for `plan-polsl-detail-mode'.")

(with-eval-after-load 'evil
  (evil-define-key '(normal visual motion) plan-polsl-detail-mode-map
    "q" #'plan-polsl-detail-quit
    (kbd "RET") #'plan-polsl-detail-open-teacher
    (kbd "<return>") #'plan-polsl-detail-open-teacher))

(define-derived-mode plan-polsl-detail-mode special-mode "Plan-PolSL:Szczegóły"
  "Major mode for inspecting class details and navigating to instructor timetables."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun plan-polsl-detail-quit ()
  "Close detail popup window without quitting the main timetable buffer."
  (interactive)
  (let ((win (selected-window)))
    (if (one-window-p)
        (bury-buffer)
      (delete-window win))))

(defun plan-polsl-detail-open-teacher ()
  "Open the schedule of the teacher selected at point."
  (interactive)
  (if-let ((tid (get-text-property (point) 'plan-polsl-teacher-id)))
      (let ((tname (or (get-text-property (point) 'plan-polsl-teacher-name) tid)))
        (plan-polsl-detail-quit)
        (message "Otwieranie planu prowadzącego: %s..." tname)
        (plan-polsl tid 10 t))
    (message "Przesuń kursor na wiersz z prowadzącym i naciśnij [Enter].")))

(defun plan-polsl-view--get-monday (time-val)
  "Return time value for Monday of the week containing TIME-VAL."
  (let* ((decoded (decode-time time-val))
         (dow (nth 6 decoded)) ;; 0=Sun, 1=Mon, ..., 6=Sat
         (days-since-monday (if (= dow 0) 6 (1- dow))))
    (time-subtract time-val (days-to-time days-since-monday))))

(defun plan-polsl-view--determine-semester-start (target-time)
  "Determine semester anchor start date (Monday) for TARGET-TIME."
  (let* ((decoded (decode-time target-time))
         (year (nth 5 decoded))
         (month (nth 4 decoded)))
    (if (or (>= month 10) (<= month 2))
        (encode-time 0 0 0 5 10 (if (<= month 2) (1- year) year))
      (encode-time 0 0 0 2 3 year))))

(defun plan-polsl-view--week-info (monday-time)
  "Return plist (:week-num N :cycle `odd|`even :label STR) for MONDAY-TIME."
  (let* ((sem-start (plan-polsl-view--determine-semester-start monday-time))
         (sem-start-mon (plan-polsl-view--get-monday sem-start))
         (diff-sec (float-time (time-subtract monday-time sem-start-mon)))
         (week-num (1+ (floor (/ diff-sec (* 7 86400)))))
         (cycle (if (cl-oddp week-num) 'odd 'even))
         (cycle-name (if (eq cycle 'odd) "Nieparzysty" "Parzysty"))
         (sunday (time-add monday-time (days-to-time 6)))
         (date-range (format "%s - %s"
                             (format-time-string "%d.%m" monday-time)
                             (format-time-string "%d.%m.%Y" sunday))))
    (list :week-num week-num
          :cycle cycle
          :cycle-name cycle-name
          :date-range date-range
          :label (if (and (>= week-num 1) (<= week-num 16))
                     (format "%s (Tydzień %d, %s)" date-range week-num cycle-name)
                   (format "%s (Poza semestrem)" date-range)))))

(defun plan-polsl-view--entry-occurs-p (entry day-idx monday-time week-cycle)
  "Return non-nil if ENTRY occurs on DAY-IDX (1=Mon..5=Fri) during week."
  (let* ((day-time (time-add monday-time (days-to-time (1- day-idx))))
         (day-str (format-time-string "%d.%m" day-time))
         (dates (plist-get entry :dates))
         (cycle (plist-get entry :cycle)))
    (cond
     (dates (member day-str dates))
     ((eq cycle 'weekly) t)
     ((eq cycle 'odd) (eq week-cycle 'odd))
     ((eq cycle 'even) (eq week-cycle 'even))
     (t t))))

(defun plan-polsl-view--type-badge (type-str)
  "Format TYPE-STR with appropriate badge face."
  (let* ((ltype (downcase (or type-str "")))
         (face (cond
                ((string-match-p "wyk" ltype) 'plan-polsl-lecture-face)
                ((string-match-p "lab" ltype) 'plan-polsl-lab-face)
                ((string-match-p "sem" ltype) 'plan-polsl-seminar-face)
                (t 'font-lock-type-face))))
    (propertize (format "[%-12s]" (or type-str "Zajęcia")) 'face face)))

(defun plan-polsl-view--pad-column (str width)
  "Pad STR with spaces so that visual `string-width' matches WIDTH."
  (let* ((sw (string-width (or str "")))
         (padding (make-string (max 0 (- width sw)) ?\s)))
    (concat (or str "") padding)))

(defun plan-polsl-view--format-entry-line (entry subject-col-width)
  "Format propertized DISPLAY-STRING for ENTRY aligned with SUBJECT-COL-WIDTH."
  (let* ((start (plist-get entry :start-time))
         (end (plist-get entry :end-time))
         (title (plist-get entry :title))
         (type (plist-get entry :type))
         (sections (plist-get entry :sections))
         (groups (plist-get entry :groups))
         (rooms (plist-get entry :rooms))
         (teachers (plist-get entry :teachers))
         (biweekly (plist-get entry :biweekly))
         (time-str (propertize (format "%s - %s" start end) 'face 'plan-polsl-time-face))
         (badge (plan-polsl-view--type-badge type))
         (title-str (propertize (format "%s%s"
                                        (if (and biweekly (not (string-prefix-p "*" title))) "* " "")
                                        title)
                                'face 'plan-polsl-title-face))
         (sec-str (if sections
                      (propertize (format " (sek. %s)" (mapconcat #'identity sections ", "))
                                  'face 'font-lock-warning-face)
                    ""))
         (subj-full (concat title-str sec-str))
         (subj-padded (plan-polsl-view--pad-column subj-full subject-col-width))
         (meta-items nil))
    (when groups
      (push (format "Grupy: %s" (mapconcat #'identity groups ", ")) meta-items))
    (when rooms
      (push (format "Sala: %s" (mapconcat #'identity rooms ", ")) meta-items))
    (when teachers
      (push (format "Prow: %s" (mapconcat #'identity teachers ", ")) meta-items))
    (let ((meta-display (if meta-items
                            (propertize (concat " │ " (mapconcat #'identity (nreverse meta-items) " • "))
                                        'face 'plan-polsl-meta-face)
                          "")))
      (format "  %s  %s  %s%s" time-str badge subj-padded meta-display))))

(defun plan-polsl-view--teacher-name (tid initials)
  "Lookup full teacher name for TID with fallback to INITIALS."
  (let* ((teachers (ignore-errors (plan-polsl-search--get-teachers nil)))
         (match (cl-find-if (lambda (pair)
                              (string-equal (format "%s" (cdr pair)) (format "%s" tid)))
                            teachers)))
    (if match (car match) initials)))

(defun plan-polsl-view--display-detail-popup (entry)
  "Display floating pop-up window with detailed information for ENTRY."
  (let* ((buf (get-buffer-create "*Plan PolSL: Szczegóły*"))
         (full-title (or (plist-get entry :full-title)
                         (plist-get entry :title)
                         "Zajęcia"))
         (type (plist-get entry :type))
         (start (plist-get entry :start-time))
         (end (plist-get entry :end-time))
         (day-name (plist-get entry :day-name))
         (cycle (plist-get entry :cycle))
         (dates (plist-get entry :dates))
         (rooms (plist-get entry :rooms))
         (sections (plist-get entry :sections))
         (groups (plist-get entry :groups))
         (teachers-info (plist-get entry :teachers-info))
         (first-teacher-pos nil))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (plan-polsl-detail-mode)

        ;; header: full course name
        (insert (propertize (format "%s\n" full-title)
                            'face '(:weight bold :height 1.2 :foreground "#51afef")))
        (insert (propertize (make-string 70 ?─) 'face 'font-lock-comment-face) "\n\n")

        ;; class properties
        (insert (format "  %-14s %s\n"
                        (propertize "Typ zajęć:" 'face 'font-lock-comment-face)
                        (plan-polsl-view--type-badge type)))
        (insert (format "  %-14s %s, %s - %s\n"
                        (propertize "Termin:" 'face 'font-lock-comment-face)
                        day-name start end))
        (insert (format "  %-14s %s\n"
                        (propertize "Cykl:" 'face 'font-lock-comment-face)
                        (cond
                         (dates (format "Wybrane terminy (%s)" (mapconcat #'identity dates ", ")))
                         ((eq cycle 'weekly) "Cotygodniowy")
                         ((eq cycle 'odd) "Tydzień Nieparzysty (*)")
                         ((eq cycle 'even) "Tydzień Parzysty (*)")
                         (t "Zajęcia cykliczne"))))
        (when rooms
          (insert (format "  %-14s %s\n"
                          (propertize "Sala:" 'face 'font-lock-comment-face)
                          (propertize (mapconcat #'identity rooms ", ") 'face 'bold))))
        (when sections
          (insert (format "  %-14s %s\n"
                          (propertize "Sekcje:" 'face 'font-lock-comment-face)
                          (propertize (format "sek. %s" (mapconcat #'identity sections ", "))
                                      'face 'font-lock-warning-face))))
        (when groups
          (insert (format "  %-14s %s\n"
                          (propertize "Grupy:" 'face 'font-lock-comment-face)
                          (mapconcat #'identity groups ", "))))
        (insert "\n")

        ;; teachers section
        (if teachers-info
            (progn
              (insert (propertize "Prowadzący (wybierz i naciśnij [Enter] aby otworzyć plan):\n"
                                  'face '(:weight bold :underline t)))
              (dolist (tinfo teachers-info)
                (let* ((tid (plist-get tinfo :id))
                       (initials (plist-get tinfo :initials))
                       (full-name (plan-polsl-view--teacher-name tid initials))
                       (line-str (format "  -> %s (%s)\n" full-name initials))
                       (beg (point)))
                  (unless first-teacher-pos
                    (setq first-teacher-pos beg))
                  (insert (propertize line-str 'face 'font-lock-function-name-face))
                  (put-text-property beg (point) 'plan-polsl-teacher-id tid)
                  (put-text-property beg (point) 'plan-polsl-teacher-name full-name)
                  (put-text-property beg (point) 'mouse-face 'highlight))))
          (insert (propertize "  (Brak informacji o prowadzącym)\n" 'face 'font-lock-comment-face)))

        ;; footer
        (insert "\n" (propertize (make-string 70 ?─) 'face 'font-lock-comment-face) "\n")
        (insert (propertize "  [q] Zamknij okno    [Enter] Otwórz plan wybranego prowadzącego\n"
                            'face 'font-lock-comment-face))
        (goto-char (or first-teacher-pos (point-min)))))

    ;; display popup window below timetable
    (let ((win (display-buffer buf '(display-buffer-below-selected
                                     display-buffer-pop-up-window
                                     display-buffer-use-some-window))))
      (when win
        (select-window win)
        (fit-window-to-buffer win 18 8)))))

;;;###autoload
(defun plan-polsl-view-show-detail ()
  "Show interactive detail popup window for the class entry at point."
  (interactive)
  (if-let ((entry (get-text-property (point) 'plan-polsl-entry)))
      (plan-polsl-view--display-detail-popup entry)
    (user-error "Kursor nie znajduje się na linii zajęć")))

(defun plan-polsl-view--filter-week-entries (entries monday-time week-cycle)
  "Group ENTRIES into 5 day vectors for the week at MONDAY-TIME and WEEK-CYCLE."
  (let ((day-groups (make-vector 5 nil)))
    (dolist (e entries)
      (let* ((d-idx (plist-get e :day-index))
             (idx (1- d-idx)))
        (when (and (>= idx 0) (< idx 5)
                   (plan-polsl-view--entry-occurs-p e d-idx monday-time week-cycle))
          (aset day-groups idx (append (aref day-groups idx) (list e))))))
    day-groups))

(defun plan-polsl-view--compute-subject-width (day-groups)
  "Compute maximum subject title column width across all DAY-GROUPS."
  (let ((max-w 18))
    (dotimes (i 5)
      (dolist (e (aref day-groups i))
        (let* ((title (plist-get e :title))
               (biweekly (plist-get e :biweekly))
               (sections (plist-get e :sections))
               (sec-str (if sections (format " (sek. %s)" (mapconcat #'identity sections ", ")) ""))
               (len (string-width (format "%s%s%s"
                                          (if (and biweekly (not (string-prefix-p "*" title))) "* " "")
                                          title sec-str))))
          (setq max-w (max max-w len)))))
    max-w))

(defun plan-polsl-view--render-buffer (entries meta id type-val monday-time)
  "Render ENTRIES and META for ID, TYPE-VAL and MONDAY-TIME into `*Plan PolSL*' buffer."
  (let* ((buf (get-buffer-create "*Plan PolSL*"))
         (title (or (plist-get meta :title) (format "ID: %s" id)))
         (path (plist-get meta :path))
         (week-info (plan-polsl-view--week-info monday-time))
         (week-label (plist-get week-info :label))
         (week-cycle (plist-get week-info :cycle))
         (day-groups (plan-polsl-view--filter-week-entries entries monday-time week-cycle))
         (max-subj-w (plan-polsl-view--compute-subject-width day-groups))
         (rendered-days (make-vector 5 nil))
         (all-lines nil)
         (header-line-1 (if path (format "%s" path) ""))
         (header-line-2 (format "Plan Zajęć: %s (ID: %s)" title id))
         (header-line-3 (format "Tydzień: %s" week-label))
         (header-line-4 "  [q] Zamknij   [r] Odśwież   [s] Synchronizuj   [t] Dziś   [< / >] Zmiana tygodnia   [Enter] Szczegóły"))

    (when path (push header-line-1 all-lines))
    (push header-line-2 all-lines)
    (push header-line-3 all-lines)
    (push header-line-4 all-lines)

    (dotimes (i 5)
      (let ((day-lines nil))
        (dolist (e (aref day-groups i))
          (let ((line-str (plan-polsl-view--format-entry-line e max-subj-w)))
            (push line-str day-lines)
            (push (substring-no-properties line-str) all-lines)))
        (aset rendered-days i (nreverse day-lines))))

    (let* ((max-w (max 75 (apply #'max (mapcar #'string-width all-lines))))
           (sep-line (make-string max-w ?─)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (plan-polsl-mode)

          ;; header banner
          (when path
            (insert (propertize (format "%s\n" path) 'face 'font-lock-comment-face)))
          (insert (propertize (format "%s\n" header-line-2) 'face '(:weight bold :height 1.15)))
          (insert (propertize (format "%s\n\n" header-line-3) 'face '(:weight bold :foreground "#51afef")))
          (insert (propertize (format "%s\n" header-line-4) 'face 'font-lock-comment-face))
          (insert (propertize sep-line 'face 'font-lock-comment-face) "\n\n")

          ;; days
          (dotimes (i 5)
            (let* ((day-lines (aref rendered-days i))
                   (day-entries (aref day-groups i))
                   (day-time (time-add monday-time (days-to-time i)))
                   (day-date-str (format-time-string "%d.%m.%Y" day-time))
                   (day-names ["Poniedziałek" "Wtorek" "Środa" "Czwartek" "Piątek"])
                   (day-title (format "%s (%s)" (aref day-names i) day-date-str)))
              (insert (propertize (format "%s\n" day-title) 'face 'plan-polsl-day-face))
              (insert (propertize sep-line 'face 'font-lock-comment-face) "\n")
              (if day-lines
                  (cl-mapc (lambda (l e)
                             (let ((beg (point)))
                               (insert l "\n")
                               (put-text-property beg (point) 'plan-polsl-entry e)
                               (put-text-property beg (point) 'mouse-face 'highlight)))
                           day-lines day-entries)
                (insert (propertize "  (Brak zaplanowanych zajęć)\n" 'face 'font-lock-comment-face)))
              (insert "\n"))))
        (goto-char (point-min)))
      buf)))

(defun plan-polsl-view--display-window (buf)
  "Display BUF in a window with 65% width if split."
  (let ((win (display-buffer buf '(display-buffer-use-some-window
                                   display-buffer-pop-up-window
                                   display-buffer-same-window))))
    (when win
      (select-window win)
      (when (and (> (frame-width) 100) (not (one-window-p)))
        (let* ((target-width (floor (* (frame-width) 0.65)))
               (delta (- target-width (window-width win))))
          (ignore-errors (window-resize win delta t)))))))

;;;###autoload
(defun plan-polsl (&optional id type refresh monday)
  "Display the PolSL timetable in a dedicated in-memory `*Plan PolSL*' buffer.
ID defaults to `plan-polsl-id'.
TYPE defaults to `plan-polsl-type' (0=group, 10=teacher, 20=room).
If REFRESH is non-nil, forces re-fetching from network.
MONDAY specifies the active week's Monday (defaults to current week)."
  (interactive "P")
  (let* ((target-id (or id
                        (bound-and-true-p plan-polsl-id)
                        (read-string "Podaj ID planu PolSL (np. 343266256 lub ID nauczyciela): ")))
         (target-type (or type (bound-and-true-p plan-polsl-type) 0))
         (active-mon (or monday
                         plan-polsl-view-active-monday
                         (plan-polsl-view--get-monday (current-time)))))
    (when (string-blank-p target-id)
      (user-error "Nie podano identyfikatora planu"))
    (setq plan-polsl-view-active-monday active-mon)
    (if (and (not refresh)
             plan-polsl-cached-entries
             (string-equal (format "%s" plan-polsl-cached-id) (format "%s" target-id))
             (equal plan-polsl-cached-type target-type))
        (let ((buf (plan-polsl-view--render-buffer plan-polsl-cached-entries
                                                   plan-polsl-cached-meta
                                                   target-id target-type active-mon)))
          (plan-polsl-view--display-window buf))
      (message "Pobieranie planu z plan.polsl.pl (ID: %s)..." target-id)
      (let* ((html (plan-polsl-http-fetch-schedule target-id target-type))
             (meta (plan-polsl-parser-extract-metadata html))
             (entries (plan-polsl-parser-parse-entries html)))
        (unless entries
          (user-error "Nie znaleziono żadnych zajęć dla ID %s na plan.polsl.pl" target-id))
        (setq plan-polsl-cached-entries entries
              plan-polsl-cached-meta meta
              plan-polsl-cached-id target-id
              plan-polsl-cached-type target-type)
        (let ((buf (plan-polsl-view--render-buffer entries meta target-id target-type active-mon)))
          (plan-polsl-view--display-window buf)
          (message "Wyświetlono plan PolSL (%d zajęć)" (length entries)))))))

;;;###autoload
(defun plan-polsl-refresh ()
  "Force re-fetch timetable from network and update `*Plan PolSL*' buffer."
  (interactive)
  (plan-polsl (or plan-polsl-cached-id (bound-and-true-p plan-polsl-id))
              (or plan-polsl-cached-type (bound-and-true-p plan-polsl-type) 0)
              t
              plan-polsl-view-active-monday))

;;;###autoload
(defun plan-polsl-current-week ()
  "Reset timetable view to the current academic week in `*Plan PolSL*' buffer."
  (interactive)
  (unless plan-polsl-cached-entries
    (user-error "Brak załadowanego planu"))
  (let ((current-mon (plan-polsl-view--get-monday (current-time))))
    (setq plan-polsl-view-active-monday current-mon)
    (let ((buf (plan-polsl-view--render-buffer plan-polsl-cached-entries
                                               plan-polsl-cached-meta
                                               plan-polsl-cached-id
                                               plan-polsl-cached-type
                                               current-mon)))
      (plan-polsl-view--display-window buf))))

;;;###autoload
(defun plan-polsl-prev-week ()
  "Navigate to previous week in `*Plan PolSL*' buffer."
  (interactive)
  (unless plan-polsl-cached-entries
    (user-error "Brak załadowanego planu"))
  (let ((prev-mon (time-subtract (or plan-polsl-view-active-monday
                                     (plan-polsl-view--get-monday (current-time)))
                                 (days-to-time 7))))
    (setq plan-polsl-view-active-monday prev-mon)
    (let ((buf (plan-polsl-view--render-buffer plan-polsl-cached-entries
                                               plan-polsl-cached-meta
                                               plan-polsl-cached-id
                                               plan-polsl-cached-type
                                               prev-mon)))
      (plan-polsl-view--display-window buf))))

;;;###autoload
(defun plan-polsl-next-week ()
  "Navigate to next week in `*Plan PolSL*' buffer."
  (interactive)
  (unless plan-polsl-cached-entries
    (user-error "Brak załadowanego planu"))
  (let ((next-mon (time-add (or plan-polsl-view-active-monday
                                (plan-polsl-view--get-monday (current-time)))
                            (days-to-time 7))))
    (setq plan-polsl-view-active-monday next-mon)
    (let ((buf (plan-polsl-view--render-buffer plan-polsl-cached-entries
                                               plan-polsl-cached-meta
                                               plan-polsl-cached-id
                                               plan-polsl-cached-type
                                               next-mon)))
      (plan-polsl-view--display-window buf))))

(provide 'plan-polsl-view)
;;; plan-polsl-view.el ends here
