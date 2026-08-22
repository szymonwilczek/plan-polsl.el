;;; plan-polsl-view.el --- Timetable buffer and mode -*- lexical-binding: t; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, view

;;; Commentary:
;; Buffer view for browsing Politechnika Śląska timetables.

;;; Code:

(require 'cl-lib)
(require 'plan-polsl-http)
(require 'plan-polsl-parser)

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

(defvar plan-polsl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'plan-polsl-refresh)
    (define-key map (kbd "r") #'plan-polsl-refresh)
    (define-key map (kbd "s") #'plan-polsl-sync)
    map)
  "Keymap for `plan-polsl-mode'.")

(with-eval-after-load 'evil
  (evil-define-key '(normal visual motion) plan-polsl-mode-map
    "q" #'quit-window
    "g" #'plan-polsl-refresh
    "r" #'plan-polsl-refresh
    "s" #'plan-polsl-sync))

(define-derived-mode plan-polsl-mode special-mode "Plan-PolSL"
  "Major mode for browsing PolSL university timetable in a dedicated buffer."
  (setq buffer-read-only t)
  (setq truncate-lines t))

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
  "Pad STR with spaces so that its visual `string-width' is exactly WIDTH."
  (let* ((sw (string-width (or str "")))
         (padding (make-string (max 0 (- width sw)) ?\s)))
    (concat (or str "") padding)))

(defun plan-polsl-view--format-entry-line (entry subject-col-width)
  "Return propertized DISPLAY-STRING for ENTRY aligned with SUBJECT-COL-WIDTH."
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
         (title-str (propertize (format "%s%s" (if biweekly "* " "") title)
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

(defun plan-polsl-view--render-buffer (entries meta id type-val)
  "Render ENTRIES and META for ID and TYPE-VAL into `*Plan PolSL*' buffer."
  (let ((buf (get-buffer-create "*Plan PolSL*"))
        (title (or (plist-get meta :title) (format "ID: %s" id)))
        (path (plist-get meta :path))

        ;; group entries by day
        (day-groups (make-vector 5 nil)))
    (dolist (e entries)
      (let ((idx (1- (plist-get e :day-index))))
        (when (and (>= idx 0) (< idx 5))
          (aset day-groups idx (append (aref day-groups idx) (list e))))))

    ;; compute maximum subject column width
    (let ((max-subj-w 18))
      (dolist (e entries)
        (let* ((title (plist-get e :title))
               (biweekly (plist-get e :biweekly))
               (sections (plist-get e :sections))
               (sec-str (if sections (format " (sek. %s)" (mapconcat #'identity sections ", ")) ""))
               (s-len (string-width (format "%s%s%s" (if biweekly "* " "") title sec-str))))
          (setq max-subj-w (max max-subj-w s-len))))

      ;; format all entry lines and collect rendered day lines
      (let ((rendered-days (make-vector 5 nil))
            (all-rendered-lines nil)
            (header-line-1 (if path (format "%s" path) ""))
            (header-line-2 (format "Plan Zajęć: %s" title))
            (header-line-3 "  [q] Zamknij   [g/r] Odśwież   [s] Synchronizuj z Org-Agenda"))
        (when path (push header-line-1 all-rendered-lines))
        (push header-line-2 all-rendered-lines)
        (push header-line-3 all-rendered-lines)
        (dotimes (i 5)
          (let ((day-entries (aref day-groups i))
                (day-lines nil))
            (dolist (e day-entries)
              (let ((line-str (plan-polsl-view--format-entry-line e max-subj-w)))
                (push line-str day-lines)
                (push (substring-no-properties line-str) all-rendered-lines)))
            (aset rendered-days i (nreverse day-lines))))

        ;; compute exact maximum line width in the entire buffer
        (let* ((max-w (max 75 (apply #'max (mapcar #'string-width all-rendered-lines))))
               (sep-line (make-string max-w ?─)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (plan-polsl-mode)

              ;; header banner
              (when path
                (insert (propertize (format "%s\n" path) 'face 'font-lock-comment-face)))
              (insert (propertize (format "%s\n" header-line-2)
                                  'face '(:weight bold :height 1.15)))
              (insert (propertize (format "%s\n" header-line-3)
                                  'face 'font-lock-comment-face))
              (insert (propertize sep-line 'face 'font-lock-comment-face) "\n\n")

              ;; days
              (dotimes (i 5)
                (let ((day-lines (aref rendered-days i))
                      (day-name (aref ["Poniedziałek" "Wtorek" "Środa" "Czwartek" "Piątek"] i)))
                  (insert (propertize (format "%s\n" day-name) 'face 'plan-polsl-day-face))
                  (insert (propertize sep-line 'face 'font-lock-comment-face) "\n")
                  (if day-lines
                      (dolist (l day-lines)
                        (insert l "\n"))
                    (insert (propertize "  (Brak zaplanowanych zajęć)\n" 'face 'font-lock-comment-face)))
                  (insert "\n"))))
            (goto-char (point-min)))
          buf)))))

(defun plan-polsl-view--display-window (buf)
  "Display BUF in a window with 65% width if split."
  (let ((win (display-buffer buf '(display-buffer-use-some-window
                                   display-buffer-pop-up-window
                                   display-buffer-same-window))))
    (when win
      (select-window win)

      ;; resize window
      (when (and (> (frame-width) 100) (not (one-window-p)))
        (let* ((target-width (floor (* (frame-width) 0.65)))
               (delta (- target-width (window-width win))))
          (ignore-errors (window-resize win delta t)))))))

;;;###autoload
(defun plan-polsl (&optional id type refresh)
  "Display the PolSL timetable in a dedicated in-memory `*Plan PolSL*' buffer.
ID defaults to `plan-polsl-id'.
TYPE defaults to `plan-polsl-type' (0=group, 10=teacher, 20=room).
If REFRESH is non-nil, forces re-fetching from network."
  (interactive "P")
  (let* ((target-id (or id
                        (bound-and-true-p plan-polsl-id)
                        (read-string "Podaj ID planu PolSL (np. 343266256 lub ID nauczyciela): ")))
         (target-type (or type (bound-and-true-p plan-polsl-type) 0)))
    (when (string-blank-p target-id)
      (user-error "Nie podano identyfikatora planu"))
    (if (and (not refresh)
             plan-polsl-cached-entries
             (string-equal (format "%s" plan-polsl-cached-id) (format "%s" target-id))
             (equal plan-polsl-cached-type target-type))
        (let ((buf (plan-polsl-view--render-buffer plan-polsl-cached-entries
                                                   plan-polsl-cached-meta
                                                   target-id target-type)))
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
        (let ((buf (plan-polsl-view--render-buffer entries meta target-id target-type)))
          (plan-polsl-view--display-window buf)
          (message "Wyświetlono plan PolSL (%d zajęć)" (length entries)))))))

;;;###autoload
(defun plan-polsl-refresh ()
  "Force re-fetch timetable from network and update `*Plan PolSL*' buffer."
  (interactive)
  (plan-polsl (or plan-polsl-cached-id (bound-and-true-p plan-polsl-id))
              (or plan-polsl-cached-type (bound-and-true-p plan-polsl-type) 0)
              t))

(provide 'plan-polsl-view)
;;; plan-polsl-view.el ends here
