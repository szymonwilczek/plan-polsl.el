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
(require 'plan-polsl-semester)
(require 'plan-polsl-usos)

;; external references for clean byte-compilation
(declare-function evil-define-key "evil-core")
(declare-function plan-polsl-sync "plan-polsl-ui")
(declare-function plan-polsl-note "plan-polsl-notes")
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

(defcustom plan-polsl-view-width 120
  "Maximum width of the timetable in columns.
Longer lines are wrapped between words. A narrower window showing the
timetable lowers the limit; when nil, only the window width counts."
  :type '(choice (const :tag "Window width" nil) integer)
  :group 'plan-polsl)

(defvar-local plan-polsl-view-id nil
  "Buffer-local schedule identifier.")

(defvar-local plan-polsl-view-type 0
  "Buffer-local schedule type (0=group, 10=teacher, 20=room, `usos').")

(defvar-local plan-polsl-view-entries nil
  "Buffer-local list of parsed timetable entries.")

(defvar-local plan-polsl-view-meta nil
  "Buffer-local schedule metadata plist.")

(defvar-local plan-polsl-view-active-monday nil
  "Buffer-local active Monday timestamp for week navigation.")

(defvar-local plan-polsl-view--rendered-width nil
  "Column limit the timetable in this buffer was last rendered for.")

(defvar-local plan-polsl-view-detail-entry nil
  "Timetable entry shown in the detail buffer.")

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
    (define-key map (kbd "n") #'plan-polsl-note)
    (define-key map (kbd "?") #'plan-polsl-help)
    (define-key map (kbd "h") #'plan-polsl-help)
    map)
  "Keymap for `plan-polsl-mode'.")

(define-derived-mode plan-polsl-mode special-mode "Plan-PolSL"
  "Major mode for browsing PolSL university timetables."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (add-hook 'window-size-change-functions #'plan-polsl-view--on-resize nil t))

(defun plan-polsl-view--on-resize (window)
  "Render the timetable in WINDOW again when its width changed."
  (with-current-buffer (window-buffer window)
    (when (and plan-polsl-view-active-monday
               plan-polsl-view--rendered-width
               (/= plan-polsl-view--rendered-width
                   (plan-polsl-view--width (current-buffer))))
      (let ((line (line-number-at-pos))
            (col (current-column))
            (start (line-number-at-pos (window-start window))))
        (plan-polsl-view--render-buffer
         plan-polsl-view-entries plan-polsl-view-meta plan-polsl-view-id
         plan-polsl-view-type plan-polsl-view-active-monday (current-buffer))
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column col)
        (set-window-point window (point))
        (set-window-start window (save-excursion
                                   (goto-char (point-min))
                                   (forward-line (1- start))
                                   (point))
                          t)))))

(defvar plan-polsl-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'plan-polsl-detail-quit)
    (define-key map (kbd "RET") #'plan-polsl-detail-open-target)
    (define-key map (kbd "<return>") #'plan-polsl-detail-open-target)
    (define-key map (kbd "<mouse-2>") #'plan-polsl-detail-open-target)
    (define-key map (kbd "n") #'plan-polsl-note)
    map)
  "Keymap for `plan-polsl-detail-mode'.")

(define-derived-mode plan-polsl-detail-mode special-mode "Plan-PolSL:Szczegóły"
  "Major mode for inspecting class details in a vertical split window."
  (setq buffer-read-only t)
  (setq truncate-lines t))

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
          "n" #'plan-polsl-note
          "?" #'plan-polsl-help
          "h" #'plan-polsl-help)
        (evil-define-key* '(normal visual motion) plan-polsl-detail-mode-map
          "q" #'plan-polsl-detail-quit
          (kbd "RET") #'plan-polsl-detail-open-target
          (kbd "<return>") #'plan-polsl-detail-open-target
          "n" #'plan-polsl-note))))

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
   ((get-text-property (point) 'plan-polsl-url)
    (browse-url (get-text-property (point) 'plan-polsl-url)))
   (t
    (message "Przesuń kursor na wiersz z prowadzącym lub salą i naciśnij [Enter]."))))

(defun plan-polsl-view--get-monday (time-val)
  "Return time value for Monday of the week containing TIME-VAL."
  (let* ((decoded (decode-time time-val))
         (dow (nth 6 decoded)) ;; 0=Sun, 1=Mon, ..., 6=Sat
         (days-since-monday (if (= dow 0) 6 (1- dow))))
    (time-subtract time-val (days-to-time days-since-monday))))

(defun plan-polsl-view--week-info (monday-time)
  "Return plist (:week-num N :cycle `odd|`even :label STR) for MONDAY-TIME."
  (let* ((sem-start-mon (plan-polsl-semester-first-monday monday-time))
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
  "Return non-nil if ENTRY occurs on DAY-IDX (1=Mon..7=Sun) during week."
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

(defun plan-polsl-view--wrap-meta (items width)
  "Lay out meta ITEMS as lines at most WIDTH columns wide.
Items are joined with \" • \" and broken between items or after the
commas of a list, such as lecturers. A single part wider than WIDTH
keeps its own line. When WIDTH is nil, everything stays on one line."
  (if (null width)
      (list (mapconcat #'identity items " • "))
    (let ((lines nil) (line nil))
      (dolist (item items)
        (let* ((parts (split-string item ", "))
               (n (length parts)))
          (cl-loop for part in parts
                   for k from 1
                   for text = (if (< k n) (concat part ",") part)
                   for sep = (if (= k 1) " • " " ")
                   do (cond
                       ((null line) (setq line text))
                       ((<= (string-width (concat line sep text)) width)
                        (setq line (concat line sep text)))
                       (t (push line lines)
                          (setq line text))))))
      (when line (push line lines))
      (nreverse lines))))

(defun plan-polsl-view--format-entry-line (entry subject-col-width &optional width)
  "Format propertized DISPLAY-STRING for ENTRY aligned with SUBJECT-COL-WIDTH.
Meta data that would not fit in WIDTH columns continues on further
lines, aligned under the first one."
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
         (prefix (format "  %s  %s  %s" time-str badge subj-padded))
         (indent (make-string (string-width prefix) ?\s))
         (meta-items nil))
    (when groups
      (push (format "Grupy: %s" (mapconcat #'identity groups ", ")) meta-items))
    (when rooms
      (push (format "Sala: %s" (mapconcat #'identity rooms ", ")) meta-items))
    (when teachers
      (push (format "Prow: %s" (mapconcat #'identity teachers ", ")) meta-items))
    (if (null meta-items)
        prefix
      (let ((lines (plan-polsl-view--wrap-meta
                    (nreverse meta-items)
                    (and width (max 20 (- width (string-width prefix) 3))))))
        (concat prefix
                (mapconcat (lambda (l) (propertize (concat " │ " l) 'face 'plan-polsl-meta-face))
                           lines
                           (concat "\n" indent)))))))

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
         (teachers (plist-get entry :teachers))
         (date (plist-get entry :date))
         (building (plist-get entry :building))
         (url (plist-get entry :url))
         (first-target-pos nil))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (plan-polsl-detail-mode)
        (setq plan-polsl-view-detail-entry entry)

        ;; header: full course name
        (insert (propertize (format "%s\n" full-title)
                            'face '(:weight bold :height 1.15 :foreground "#51afef")))
        (insert (propertize (make-string 55 ?─) 'face 'font-lock-comment-face) "\n\n")

        ;; class properties
        (insert (format "  %-12s %s\n"
                        (propertize "Typ:" 'face 'font-lock-comment-face)
                        (plan-polsl-view--type-badge type)))
        (insert (format "  %-12s %s%s, %s - %s\n"
                        (propertize "Termin:" 'face 'font-lock-comment-face)
                        day-name
                        (if date
                            (format-time-string " %d.%m.%Y" (date-to-time (concat date " 12:00")))
                          "")
                        start end))
        (insert (format "  %-12s %s\n"
                        (propertize "Cykl:" 'face 'font-lock-comment-face)
                        (cond
                         ((and date (eq cycle 'weekly)) "Cotygodniowy")
                         ((and date (eq cycle 'odd)) "Tydzień Nieparzysty (*)")
                         ((and date (eq cycle 'even)) "Tydzień Parzysty (*)")
                         ((and date (plist-get entry :biweekly)) "Co 2 tygodnie (*)")
                         (date "Pojedynczy termin")
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
          (if teachers
              (progn
                (insert (propertize "Prowadzący:\n" 'face '(:weight bold :underline t)))
                (dolist (name teachers)
                  (insert (propertize (format "  -> %s\n" name)
                                      'face 'font-lock-function-name-face))))
            (insert (propertize "  (Brak informacji o prowadzącym)\n" 'face 'font-lock-comment-face))))

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
        (when building
          (insert (format "  %-12s %s\n"
                          (propertize "Budynek:" 'face 'font-lock-comment-face)
                          building)))

        ;; link to the class page (USOS)
        (when url
          (insert "\n")
          (let ((beg (point)))
            (unless first-target-pos
              (setq first-target-pos beg))
            (insert (propertize "  -> Strona zajęć w USOSweb\n" 'face 'link))
            (put-text-property beg (point) 'plan-polsl-url url)
            (put-text-property beg (point) 'mouse-face 'highlight)))

        ;; footer
        (insert "\n" (propertize (make-string 55 ?─) 'face 'font-lock-comment-face) "\n")
        (insert (propertize "  [q] Zamknij okno\n  [Enter] Otwórz plan wybranego elementu\n  [n] Notatka do przedmiotu\n"
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
  (if-let* ((entry (get-text-property (point) 'plan-polsl-entry)))
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
            "[Enter] Szczegóły | [n] Notatka | [r] Odśwież | [s] Sync | [q] Zamknij")))

(defun plan-polsl-view--buffer-name (id type-val meta)
  "Generate appropriate buffer name for ID, TYPE-VAL, and META."
  (let ((default-id (bound-and-true-p plan-polsl-id))
        (title (plist-get meta :title)))
    (cond
     ((eq type-val 'usos) "*Plan PolSL: USOS*")
     ((and default-id (string-equal (format "%s" id) (format "%s" default-id)))
      "*Plan PolSL*")
     ((and title (> (length title) 0))
      (format "*Plan PolSL: %s*" title))
     (t (format "*Plan PolSL: %s*" id)))))

(defun plan-polsl-view--filter-week-entries (entries monday-time week-cycle)
  "Group ENTRIES into 7 day vectors for the week at MONDAY-TIME and WEEK-CYCLE."
  (let ((day-groups (make-vector 7 nil)))
    (dolist (e entries)
      (let* ((d-idx (plist-get e :day-index))
             (idx (1- d-idx)))
        (when (and (>= idx 0) (< idx 7)
                   (plan-polsl-view--entry-occurs-p e d-idx monday-time week-cycle))
          (aset day-groups idx (append (aref day-groups idx) (list e))))))
    day-groups))

(defun plan-polsl-view--compute-subject-width (day-groups)
  "Compute maximum subject title column width across all DAY-GROUPS."
  (let ((max-w 18))
    (dotimes (i 7)
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

(defun plan-polsl-view--width (buf)
  "Return the column limit for rendering BUF.
That is the body width of a window showing BUF or the selected window,
minus one column for the continuation glyph, but at most
`plan-polsl-view-width'."
  (let ((window-width (1- (window-body-width
                           (or (get-buffer-window buf t) (selected-window))))))
    (if plan-polsl-view-width
        (min plan-polsl-view-width window-width)
      window-width)))

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
         (width (plan-polsl-view--width buf))
         (rendered-days (make-vector 7 nil))
         (all-lines nil)
         (header-line-1 (if path (format "%s" path) ""))
         (header-line-2 (if (eq type-val 'usos)
                            (format "Plan Zajęć: %s (USOS)" title)
                          (format "Plan Zajęć: %s (ID: %s)" title id)))
         (header-line-3 (format "Tydzień: %s" week-label))
         (header-line-4 "  [q] Zamknij   [r] Odśwież   [s] Synchronizuj   [t] Dziś   [w] Tydzień   [< / >] Tygodnie   [n] Notatka   [?] Pomoc"))

    (when path (push header-line-1 all-lines))
    (push header-line-2 all-lines)
    (push header-line-3 all-lines)
    (push header-line-4 all-lines)

    (dotimes (i 7)
      (let ((day-lines nil))
        (dolist (e (aref day-groups i))
          (let ((line-str (plan-polsl-view--format-entry-line e max-subj-w width)))
            (push line-str day-lines)
            (dolist (l (split-string (substring-no-properties line-str) "\n"))
              (push l all-lines))))
        (aset rendered-days i (nreverse day-lines))))

    (let* ((max-w (min (max 75 (apply #'max (mapcar #'string-width all-lines)))
                       width))
           (sep-line (make-string max-w ?─)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (plan-polsl-mode)
          (setq plan-polsl-view-id id
                plan-polsl-view-type type-val
                plan-polsl-view-entries entries
                plan-polsl-view-meta meta
                plan-polsl-view-active-monday monday-time
                plan-polsl-view--rendered-width width)

          ;; header banner
          (when path
            (insert (propertize (format "%s\n" path) 'face 'font-lock-comment-face)))
          (insert (propertize (format "%s\n" header-line-2) 'face '(:weight bold :height 1.15)))
          (insert (propertize (format "%s\n\n" header-line-3) 'face '(:weight bold :foreground "#51afef")))
          (insert (propertize (format "%s\n" header-line-4) 'face 'font-lock-comment-face))
          (insert (propertize sep-line 'face 'font-lock-comment-face) "\n\n")

          ;; days, weekend only when it has classes
          (dotimes (i 7)
            (let* ((day-lines (aref rendered-days i))
                   (day-entries (aref day-groups i))
                   (day-time (time-add monday-time (days-to-time i)))
                   (day-date-str (format-time-string "%d.%m.%Y" day-time))
                   (day-names ["Poniedziałek" "Wtorek" "Środa" "Czwartek" "Piątek"
                               "Sobota" "Niedziela"])
                   (day-title (format "%s (%s)" (aref day-names i) day-date-str)))
              (when (or (< i 5) day-lines)
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
		(insert "\n")))))
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
                       (or plan-polsl-view-entries (eq plan-polsl-view-type 'usos))
                       (string-equal (format "%s" (or plan-polsl-view-id ""))
                                     (format "%s" target-id))
                       (equal plan-polsl-view-type target-type))))
              (buffer-list)))

;;;###autoload
(defun plan-polsl (&optional id type refresh monday)
  "Display the PolSL timetable in a dedicated in-memory buffer.
When called without ID and TYPE while logged in to USOS, shows the
personal USOS timetable (see `plan-polsl-usos-default').
ID defaults to `plan-polsl-id'.
TYPE defaults to `plan-polsl-type' (0=group, 10=teacher, 20=room).
TYPE `usos' shows the personal USOS timetable via `plan-polsl-usos'.
If REFRESH is non-nil, forces re-fetching from network.
MONDAY specifies the active week's Monday (defaults to current week).
Interactively, a prefix argument skips USOS and opens the
plan.polsl.pl timetable of `plan-polsl-id' (prompting when unset)."
  (interactive
   (when current-prefix-arg
     (list (or (bound-and-true-p plan-polsl-id)
               (read-string "Podaj ID planu PolSL (np. 343266256 lub ID nauczyciela): ")))))
  (if (or (eq type 'usos)
          (and (null id) (null type)
               plan-polsl-usos-default
               (plan-polsl-usos-logged-in-p)))
      (plan-polsl-usos refresh monday)
    (plan-polsl-view--open-polsl id type refresh monday)))

(defun plan-polsl-view--open-polsl (id type refresh monday)
  "Show the plan.polsl.pl timetable for ID and TYPE.
See `plan-polsl' for REFRESH and MONDAY."
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

(defun plan-polsl-view--require-plan ()
  "Signal `user-error' unless the current buffer shows a loaded timetable."
  (unless (and (derived-mode-p 'plan-polsl-mode) plan-polsl-view-id)
    (user-error "Brak załadowanego planu")))

(defun plan-polsl-view--show-week (monday)
  "Re-render the current timetable buffer for the week starting at MONDAY."
  (plan-polsl-view--require-plan)
  (if (eq plan-polsl-view-type 'usos)
      (plan-polsl-view--usos-show-week (current-buffer) monday)
    (setq plan-polsl-view-active-monday monday)
    (plan-polsl-view--display-window
     (plan-polsl-view--render-buffer plan-polsl-view-entries
                                     plan-polsl-view-meta
                                     plan-polsl-view-id
                                     plan-polsl-view-type
                                     monday
                                     (buffer-name)))))

(defun plan-polsl-view--week-key (monday)
  "Return the cache key of the week starting at MONDAY."
  (format-time-string "%Y-%m-%d" monday))

(defun plan-polsl-view--usos-meta ()
  "Return fresh metadata for a USOS timetable buffer.
The :weeks hash table caches fetched entries per week."
  (list :title (or (plist-get (plan-polsl-usos--load-token) :user-name)
                   "Mój plan")
        :path "USOS Politechniki Śląskiej"
        :weeks (make-hash-table :test #'equal)))

(defun plan-polsl-view--usos-report-error (err)
  "Show a message describing USOS fetch error ERR."
  (message "plan-polsl: %s%s"
           (plan-polsl-usos-error-message err)
           (if (eq (car err) 'plan-polsl-usos-unauthorized)
               " - zaloguj się ponownie: M-x plan-polsl-usos-login"
             "")))

(defun plan-polsl-view--usos-show-week (buf-or-name monday &optional meta)
  "Show USOS week starting at MONDAY in BUF-OR-NAME, fetching it if needed.
META defaults to the metadata of the existing buffer; its :weeks table
caches entries so revisiting a week needs no network request."
  (let* ((buf (get-buffer buf-or-name))
         (meta (or meta
                   (and buf (buffer-local-value 'plan-polsl-view-meta buf))
                   (plan-polsl-view--usos-meta)))
         (weeks (plist-get meta :weeks))
         (key (plan-polsl-view--week-key monday))
         (name (if (bufferp buf-or-name) (buffer-name buf-or-name) buf-or-name))
         (show (lambda (entries)
                 (plan-polsl-view--display-window
                  (plan-polsl-view--render-buffer entries meta "usos" 'usos
                                                  monday name)))))
    (pcase (gethash key weeks 'missing)
      ('missing
       (message "Pobieranie planu z USOS (tydzień od %s)..."
                (format-time-string "%d.%m.%Y" monday))
       (plan-polsl-usos-fetch-week-async
        monday
        (lambda (entries)
          (puthash key entries weeks)
          (funcall show entries)
          (message "Wyświetlono plan z USOS (%d zajęć w tygodniu)" (length entries)))
        #'plan-polsl-view--usos-report-error))
      (entries (funcall show entries)))))

;;;###autoload
(defun plan-polsl-usos (&optional refresh monday)
  "Display the personal timetable from USOS in a dedicated buffer.
Requires logging in first with `plan-polsl-usos-login'. If REFRESH is
non-nil, drop cached weeks and fetch again. MONDAY specifies the week
to show (defaults to the current week)."
  (interactive)
  (unless (plan-polsl-usos-logged-in-p)
    (user-error "Nie jesteś zalogowany do USOS (M-x plan-polsl-usos-login)"))
  (let ((monday (or monday (plan-polsl-view--get-monday (current-time))))
        (live-buf (plan-polsl-view--find-live-buffer "usos" 'usos)))
    (if (and live-buf (not refresh))
        (plan-polsl-view--display-window live-buf)
      (plan-polsl-view--usos-show-week (plan-polsl-view--buffer-name "usos" 'usos nil)
                                       monday
                                       (plan-polsl-view--usos-meta)))))

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
  (plan-polsl-view--show-week (plan-polsl-view--get-monday (current-time))))

;;;###autoload
(defun plan-polsl-goto-week (week-num)
  "Jump directly to WEEK-NUM (1-16) of the current academic semester."
  (interactive "nPrzejdź do tygodnia semestru (1-16): ")
  (plan-polsl-view--require-plan)
  (when (or (< week-num 1) (> week-num 30))
    (user-error "Numer tygodnia musi być z zakresu 1-30"))
  (let* ((sem-start-mon (plan-polsl-semester-first-monday))
         (target-mon (time-add sem-start-mon (days-to-time (* (1- week-num) 7)))))
    (plan-polsl-view--show-week target-mon)
    (message "Przejście do tygodnia %d (%s)"
             week-num (format-time-string "%d.%m.%Y" target-mon))))

;;;###autoload
(defun plan-polsl-prev-week ()
  "Navigate to previous week in current timetable buffer."
  (interactive)
  (plan-polsl-view--show-week
   (time-subtract (or plan-polsl-view-active-monday
                      (plan-polsl-view--get-monday (current-time)))
                  (days-to-time 7))))

;;;###autoload
(defun plan-polsl-next-week ()
  "Navigate to next week in current timetable buffer."
  (interactive)
  (plan-polsl-view--show-week
   (time-add (or plan-polsl-view-active-monday
                 (plan-polsl-view--get-monday (current-time)))
             (days-to-time 7))))

(provide 'plan-polsl-view)
;;; plan-polsl-view.el ends here
