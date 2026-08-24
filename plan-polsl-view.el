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
(declare-function plan-polsl-search--get-teacher-by-id "plan-polsl-search")

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

(defvar-local plan-polsl-view-id nil
  "Buffer-local schedule identifier.")

(defvar-local plan-polsl-view-type 0
  "Buffer-local schedule type (0=group, 10=teacher, 20=room).")

(defvar-local plan-polsl-view-entries nil
  "Buffer-local list of parsed timetable entries.")

(defvar-local plan-polsl-view-meta nil
  "Buffer-local schedule metadata plist.")

(defvar-local plan-polsl-view-active-monday nil
  "Buffer-local active Monday timestamp for week navigation.")

(defvar plan-polsl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "r") #'plan-polsl-refresh)
    (define-key map (kbd "s") #'plan-polsl-sync)
    (define-key map (kbd "t") #'plan-polsl-current-week)
    (define-key map (kbd "w") #'plan-polsl-goto-week)
    (define-key map (kbd "<") #'plan-polsl-prev-week)
    (define-key map (kbd ">") #'plan-polsl-next-week)
    (define-key map (kbd "TAB") #'plan-polsl-next-entry)
    (define-key map (kbd "<tab>") #'plan-polsl-next-entry)
    (define-key map (kbd "<backtab>") #'plan-polsl-prev-entry)
    (define-key map (kbd "S-TAB") #'plan-polsl-prev-entry)
    (define-key map (kbd "RET") #'plan-polsl-view-show-detail)
    (define-key map (kbd "<return>") #'plan-polsl-view-show-detail)
    (define-key map (kbd "<mouse-2>") #'plan-polsl-view-show-detail)
    (define-key map (kbd "?") #'plan-polsl-help)
    (define-key map (kbd "h") #'plan-polsl-help)
    map)
  "Keymap for `plan-polsl-mode'.")

(with-eval-after-load 'evil
  (if (fboundp 'evil-define-key*)
      (progn
        (evil-define-key* '(normal visual motion) plan-polsl-mode-map
          "q" #'quit-window
          "r" #'plan-polsl-refresh
          "s" #'plan-polsl-sync
          "t" #'plan-polsl-current-week
          "w" #'plan-polsl-goto-week
          "<" #'plan-polsl-prev-week
          ">" #'plan-polsl-next-week
          (kbd "TAB") #'plan-polsl-next-entry
          (kbd "<tab>") #'plan-polsl-next-entry
          (kbd "<backtab>") #'plan-polsl-prev-entry
          (kbd "S-TAB") #'plan-polsl-prev-entry
          (kbd "RET") #'plan-polsl-view-show-detail
          (kbd "<return>") #'plan-polsl-view-show-detail
          "?" #'plan-polsl-help
          "h" #'plan-polsl-help)
        (evil-define-key* '(normal visual motion) plan-polsl-detail-mode-map
          "q" #'plan-polsl-detail-quit
          (kbd "RET") #'plan-polsl-detail-open-target
          (kbd "<return>") #'plan-polsl-detail-open-target))))

(define-derived-mode plan-polsl-mode special-mode "Plan-PolSL"
  "Major mode for browsing PolSL university timetables."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defvar plan-polsl-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'plan-polsl-detail-quit)
    (define-key map (kbd "RET") #'plan-polsl-detail-open-target)
    (define-key map (kbd "<return>") #'plan-polsl-detail-open-target)
    (define-key map (kbd "<mouse-2>") #'plan-polsl-detail-open-target)
    map)
  "Keymap for `plan-polsl-detail-mode'.")

(define-derived-mode plan-polsl-detail-mode special-mode "Plan-PolSL:Szczegóły"
  "Major mode for inspecting class details in a vertical split window."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun plan-polsl-detail-quit ()
  "Close detail popup window without quitting the main timetable buffer."
  (interactive)
  (let ((win (selected-window)))
    (if (one-window-p)
        (bury-buffer)
      (delete-window win))))

(defun plan-polsl-detail-open-target ()
  "Open schedule of teacher or room selected at point in a separate buffer."
  (interactive)
  (cond
   ((get-text-property (point) 'plan-polsl-teacher-id)
    (let* ((tid (get-text-property (point) 'plan-polsl-teacher-id))
           (tname (or (get-text-property (point) 'plan-polsl-teacher-name) tid)))
      (plan-polsl-detail-quit)
      (message "Otwieranie planu prowadzącego: %s..." tname)
      (plan-polsl tid 10 t)))
   ((get-text-property (point) 'plan-polsl-room-id)
    (let* ((rid (get-text-property (point) 'plan-polsl-room-id))
           (rname (or (get-text-property (point) 'plan-polsl-room-name) rid)))
      (plan-polsl-detail-quit)
      (message "Otwieranie planu sali: %s..." rname)
      (plan-polsl rid 20 t)))
   (t
    (message "Przesuń kursor na wiersz z prowadzącym lub salą i naciśnij [Enter]."))))

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
  "Lookup full teacher name for TID in O(1) time with fallback to INITIALS."
  (or (ignore-errors (plan-polsl-search--get-teacher-by-id tid))
      initials))

(defun plan-polsl-view--display-detail-popup (entry)
  "Display vertical split window on the right with detailed information for ENTRY."
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
         (rooms-info (plist-get entry :rooms-info))
         (sections (plist-get entry :sections))
         (groups (plist-get entry :groups))
         (teachers-info (plist-get entry :teachers-info))
         (first-target-pos nil))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (plan-polsl-detail-mode)

        ;; header: full course name
        (insert (propertize (format "%s\n" full-title)
                            'face '(:weight bold :height 1.15 :foreground "#51afef")))
        (insert (propertize (make-string 55 ?─) 'face 'font-lock-comment-face) "\n\n")

        ;; class properties
        (insert (format "  %-12s %s\n"
                        (propertize "Typ:" 'face 'font-lock-comment-face)
                        (plan-polsl-view--type-badge type)))
        (insert (format "  %-12s %s, %s - %s\n"
                        (propertize "Termin:" 'face 'font-lock-comment-face)
                        day-name start end))
        (insert (format "  %-12s %s\n"
                        (propertize "Cykl:" 'face 'font-lock-comment-face)
                        (cond
                         (dates (format "Wybrane terminy (%s)" (mapconcat #'identity dates ", ")))
                         ((eq cycle 'weekly) "Cotygodniowy")
                         ((eq cycle 'odd) "Tydzień Nieparzysty (*)")
                         ((eq cycle 'even) "Tydzień Parzysty (*)")
                         (t "Zajęcia cykliczne"))))
        (when sections
          (insert (format "  %-12s %s\n"
                          (propertize "Sekcje:" 'face 'font-lock-comment-face)
                          (propertize (format "sek. %s" (mapconcat #'identity sections ", "))
                                      'face 'font-lock-warning-face))))
        (when groups
          (insert (format "  %-12s %s\n"
                          (propertize "Grupy:" 'face 'font-lock-comment-face)
                          (mapconcat #'identity groups ", "))))
        (insert "\n")

        ;; teachers section
        (if teachers-info
            (progn
              (insert (propertize "Prowadzący (Enter = otwórz plan):\n"
                                  'face '(:weight bold :underline t)))
              (dolist (tinfo teachers-info)
                (let* ((tid (plist-get tinfo :id))
                       (initials (plist-get tinfo :initials))
                       (full-name (plan-polsl-view--teacher-name tid initials))
                       (line-str (format "  -> %s (%s)\n" full-name initials))
                       (beg (point)))
                  (unless first-target-pos
                    (setq first-target-pos beg))
                  (insert (propertize line-str 'face 'font-lock-function-name-face))
                  (put-text-property beg (point) 'plan-polsl-teacher-id tid)
                  (put-text-property beg (point) 'plan-polsl-teacher-name full-name)
                  (put-text-property beg (point) 'mouse-face 'highlight))))
          (insert (propertize "  (Brak informacji o prowadzącym)\n" 'face 'font-lock-comment-face)))

        ;; rooms section
        (insert "\n")
        (if rooms-info
            (progn
              (insert (propertize "Sale (Enter = otwórz plan sali):\n"
                                  'face '(:weight bold :underline t)))
              (dolist (rinfo rooms-info)
                (let* ((rid (plist-get rinfo :id))
                       (rname (plist-get rinfo :name))
                       (line-str (format "  %s\n" rname))
                       (beg (point)))
                  (unless first-target-pos
                    (setq first-target-pos beg))
                  (insert (propertize line-str 'face 'font-lock-type-face))
                  (put-text-property beg (point) 'plan-polsl-room-id rid)
                  (put-text-property beg (point) 'plan-polsl-room-name rname)
                  (put-text-property beg (point) 'mouse-face 'highlight))))
          (when rooms
            (insert (format "  %-12s %s\n"
                            (propertize "Sala:" 'face 'font-lock-comment-face)
                            (propertize (mapconcat #'identity rooms ", ") 'face 'bold)))))

        ;; footer
        (insert "\n" (propertize (make-string 55 ?─) 'face 'font-lock-comment-face) "\n")
        (insert (propertize "  [q] Zamknij okno\n  [Enter] Otwórz plan wybranego elementu\n"
                            'face 'font-lock-comment-face))
        (goto-char (or first-target-pos (point-min)))))

    ;; display vertical split window on the right side
    (let ((win (display-buffer buf
                               '((display-buffer-in-direction
                                  display-buffer-pop-up-window
                                  display-buffer-use-some-window)
                                 (direction . right)
                                 (window-width . 0.38)))))
      (when win
        (select-window win)))))

;;;###autoload
(defun plan-polsl-view-show-detail ()
  "Show interactive detail popup window for the class entry at point."
  (interactive)
  (if-let ((entry (get-text-property (point) 'plan-polsl-entry)))
      (plan-polsl-view--display-detail-popup entry)
    (user-error "Kursor nie znajduje się na linii zajęć")))

;;;###autoload
(defun plan-polsl-next-entry ()
  "Jump forward to the next scheduled class entry in the buffer."
  (interactive)
  (let ((pos (next-single-property-change (point) 'plan-polsl-entry)))
    (while (and pos (not (get-text-property pos 'plan-polsl-entry)))
      (setq pos (next-single-property-change pos 'plan-polsl-entry)))
    (if pos
        (goto-char pos)
      (user-error "Koniec listy zajęć"))))

;;;###autoload
(defun plan-polsl-prev-entry ()
  "Jump backward to the previous scheduled class entry in the buffer."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'plan-polsl-entry)))
    (while (and pos (not (get-text-property pos 'plan-polsl-entry)))
      (setq pos (previous-single-property-change pos 'plan-polsl-entry)))
    (if pos
        (progn
          (while (and (> pos (point-min))
                      (get-text-property (1- pos) 'plan-polsl-entry))
            (setq pos (1- pos)))
          (goto-char pos))
      (user-error "Początek listy zajęć"))))

;;;###autoload
(defun plan-polsl-help ()
  "Display quick keybindings cheat-sheet for `plan-polsl-mode'."
  (interactive)
  (message (concat
            (propertize "Plan PolSL Shortcuts: " 'face 'bold)
            "[< / >] Tygodnie | [t] Dziś | [w] Tydzień (1-16) | "
            "[TAB / S-TAB] Następne/poprzednie zajęcia | "
            "[Enter] Szczegóły | [r] Odśwież | [s] Sync | [q] Zamknij")))

(defun plan-polsl-view--buffer-name (id _type-val meta)
  "Generate appropriate buffer name for ID, _TYPE-VAL, and META."
  (let ((default-id (bound-and-true-p plan-polsl-id))
        (title (plist-get meta :title)))
    (if (and default-id (string-equal (format "%s" id) (format "%s" default-id)))
        "*Plan PolSL*"
      (if (and title (> (length title) 0))
          (format "*Plan PolSL: %s*" title)
        (format "*Plan PolSL: %s*" id)))))

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

(defun plan-polsl-view--render-buffer (entries meta id type-val monday-time &optional target-buf)
  "Render ENTRIES and META for ID, TYPE-VAL and MONDAY-TIME into TARGET-BUF."
  (let* ((buf-name (or target-buf (plan-polsl-view--buffer-name id type-val meta)))
         (buf (get-buffer-create buf-name))
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
         (header-line-4 "  [q] Zamknij   [r] Odśwież   [s] Synchronizuj   [t] Dziś   [w] Tydzień   [< / >] Tygodnie   [?] Pomoc"))

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
          (setq plan-polsl-view-id id
                plan-polsl-view-type type-val
                plan-polsl-view-entries entries
                plan-polsl-view-meta meta
                plan-polsl-view-active-monday monday-time)

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

(defun plan-polsl-view--find-live-buffer (target-id target-type)
  "Find an existing live buffer displaying TARGET-ID and TARGET-TYPE."
  (cl-find-if (lambda (buf)
                (with-current-buffer buf
                  (and (derived-mode-p 'plan-polsl-mode)
                       plan-polsl-view-entries
                       (string-equal (format "%s" (or plan-polsl-view-id ""))
                                     (format "%s" target-id))
                       (equal plan-polsl-view-type target-type))))
              (buffer-list)))

;;;###autoload
(defun plan-polsl (&optional id type refresh monday)
  "Display the PolSL timetable in a dedicated in-memory buffer.
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
                         (plan-polsl-view--get-monday (current-time))))
         (live-buf (unless refresh (plan-polsl-view--find-live-buffer target-id target-type))))
    (when (string-blank-p target-id)
      (user-error "Nie podano identyfikatora planu"))
    (if live-buf
        ;; instant switch to existing open buffer
        (plan-polsl-view--display-window live-buf)

      ;; non-blocking asynchronous network retrieval
      (message "Pobieranie planu z plan.polsl.pl (ID: %s)..." target-id)
      (plan-polsl-http-fetch-schedule-async
       target-id target-type
       (lambda (html)
         (let* ((meta (plan-polsl-parser-extract-metadata html))
                (entries (plan-polsl-parser-parse-entries html)))
           (if (null entries)
               (message "plan-polsl: Nie znaleziono żadnych zajęć dla ID %s na plan.polsl.pl" target-id)
             (let ((buf (plan-polsl-view--render-buffer entries meta target-id target-type active-mon)))
               (plan-polsl-view--display-window buf)
               (message "Wyświetlono plan PolSL (%d zajęć)" (length entries))))))
       (lambda (err)
         (message "plan-polsl błąd pobierania: %s" err))))))

;;;###autoload
(defun plan-polsl-refresh ()
  "Force re-fetch timetable from network and update current buffer."
  (interactive)
  (plan-polsl (or plan-polsl-view-id (bound-and-true-p plan-polsl-id))
              (or plan-polsl-view-type (bound-and-true-p plan-polsl-type) 0)
              t
              plan-polsl-view-active-monday))

;;;###autoload
(defun plan-polsl-current-week ()
  "Reset timetable view to the current academic week."
  (interactive)
  (unless plan-polsl-view-entries
    (user-error "Brak załadowanego planu"))
  (let ((current-mon (plan-polsl-view--get-monday (current-time))))
    (setq plan-polsl-view-active-monday current-mon)
    (let ((buf (plan-polsl-view--render-buffer plan-polsl-view-entries
                                               plan-polsl-view-meta
                                               plan-polsl-view-id
                                               plan-polsl-view-type
                                               current-mon
                                               (buffer-name))))
      (plan-polsl-view--display-window buf))))

;;;###autoload
(defun plan-polsl-goto-week (week-num)
  "Jump directly to WEEK-NUM (1-16) of the current academic semester."
  (interactive "nPrzejdź do tygodnia semestru (1-16): ")
  (unless plan-polsl-view-entries
    (user-error "Brak załadowanego planu"))
  (when (or (< week-num 1) (> week-num 30))
    (user-error "Numer tygodnia musi być z zakresu 1-30"))
  (let* ((sem-start (plan-polsl-view--determine-semester-start (current-time)))
         (sem-start-mon (plan-polsl-view--get-monday sem-start))
         (target-mon (time-add sem-start-mon (days-to-time (* (1- week-num) 7)))))
    (setq plan-polsl-view-active-monday target-mon)
    (let ((buf (plan-polsl-view--render-buffer plan-polsl-view-entries
                                               plan-polsl-view-meta
                                               plan-polsl-view-id
                                               plan-polsl-view-type
                                               target-mon
                                               (buffer-name))))
      (plan-polsl-view--display-window buf)
      (message "Przejście do tygodnia %d (%s)"
               week-num (format-time-string "%d.%m.%Y" target-mon)))))

;;;###autoload
(defun plan-polsl-prev-week ()
  "Navigate to previous week in current timetable buffer."
  (interactive)
  (unless plan-polsl-view-entries
    (user-error "Brak załadowanego planu"))
  (let ((prev-mon (time-subtract (or plan-polsl-view-active-monday
                                     (plan-polsl-view--get-monday (current-time)))
                                 (days-to-time 7))))
    (setq plan-polsl-view-active-monday prev-mon)
    (let ((buf (plan-polsl-view--render-buffer plan-polsl-view-entries
                                               plan-polsl-view-meta
                                               plan-polsl-view-id
                                               plan-polsl-view-type
                                               prev-mon
                                               (buffer-name))))
      (plan-polsl-view--display-window buf))))

;;;###autoload
(defun plan-polsl-next-week ()
  "Navigate to next week in current timetable buffer."
  (interactive)
  (unless plan-polsl-view-entries
    (user-error "Brak załadowanego planu"))
  (let ((next-mon (time-add (or plan-polsl-view-active-monday
                                (plan-polsl-view--get-monday (current-time)))
                            (days-to-time 7))))
    (setq plan-polsl-view-active-monday next-mon)
    (let ((buf (plan-polsl-view--render-buffer plan-polsl-view-entries
                                               plan-polsl-view-meta
                                               plan-polsl-view-id
                                               plan-polsl-view-type
                                               next-mon
                                               (buffer-name))))
      (plan-polsl-view--display-window buf))))

(provide 'plan-polsl-view)
;;; plan-polsl-view.el ends here
