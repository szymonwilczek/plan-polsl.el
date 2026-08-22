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
  '((t :inherit font-lock-keyword-face :weight bold :height 1.15))
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

(defvar plan-polsl-cached-group nil
  "Group ID corresponding to `plan-polsl-cached-entries'.")

(defvar plan-polsl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'plan-polsl-refresh)
    (define-key map (kbd "r") #'plan-polsl-refresh)
    (define-key map (kbd "s") #'plan-polsl-sync)
    map)
  "Keymap for `plan-polsl-mode'.")

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

(defun plan-polsl-view--render-buffer (entries group-id)
  "Render ENTRIES for GROUP-ID into `*Plan PolSL*' buffer."
  (let ((buf (get-buffer-create "*Plan PolSL*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (plan-polsl-mode)

        ;; header banner
        (insert (propertize (format "Plan Zajęć Politechniki Śląskiej - Grupa: %s\n" group-id)
                            'face '(:weight bold :height 1.2)))
        (insert (propertize "   [q] Zamknij  |  [g/r] Odśwież z sieci  |  [s] Zsynchronizuj z Org-Agenda\n"
                            'face 'font-lock-comment-face))
        (insert (propertize (make-string 85 ?─) 'face 'font-lock-comment-face) "\n\n")

        ;; group entries by day
        (let ((by-day (make-vector 5 nil)))
          (dolist (e entries)
            (let ((idx (1- (plist-get e :day-index))))
              (when (and (>= idx 0) (< idx 5))
                (aset by-day idx (append (aref by-day idx) (list e))))))
          (dotimes (i 5)
            (let ((day-entries (aref by-day i))
                  (day-name (aref ["Poniedziałek" "Wtorek" "Środa" "Czwartek" "Piątek"] i)))
              (insert (propertize (format "%s\n" day-name) 'face 'plan-polsl-day-face))
              (insert (propertize (make-string 85 ?┄) 'face 'font-lock-comment-face) "\n")
              (if day-entries
                  (dolist (e day-entries)
                    (let* ((start (plist-get e :start-time))
                           (end (plist-get e :end-time))
                           (title (plist-get e :title))
                           (type (plist-get e :type))
                           (sections (plist-get e :sections))
                           (rooms (plist-get e :rooms))
                           (teachers (plist-get e :teachers))
                           (biweekly (plist-get e :biweekly))
                           (time-str (propertize (format "%s - %s" start end) 'face 'plan-polsl-time-face))
                           (badge (plan-polsl-view--type-badge type))
                           (title-str (propertize (format "%s%s" (if biweekly "* " "") title)
                                                  'face 'plan-polsl-title-face))
                           (sec-str (if sections
                                        (propertize (format " (sek. %s)" (mapconcat #'identity sections ", "))
                                                    'face 'font-lock-warning-face)
                                      ""))
                           (meta-items nil))
                      (when rooms
                        (push (format "Sala: %s" (mapconcat #'identity rooms ", ")) meta-items))
                      (when teachers
                        (push (format "Prow: %s" (mapconcat #'identity teachers ", ")) meta-items))
                      (let ((meta-str (if meta-items
                                          (propertize (concat " │ " (mapconcat #'identity (nreverse meta-items) " • "))
                                                      'face 'plan-polsl-meta-face)
                                        "")))
                        (insert (format "  %-13s %s %-20s%s%s\n"
                                        time-str badge (concat title-str sec-str) "" meta-str)))))
                (insert (propertize "  (Brak zaplanowanych zajęć)\n" 'face 'font-lock-comment-face)))
              (insert "\n")))))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;###autoload
(defun plan-polsl (&optional group-id refresh)
  "Display the timetable in in-memory `*Plan PolSL*' buffer.
GROUP-ID defaults to `plan-polsl-group-id'.
If REFRESH is non-nil, forces re-fetching from network."
  (interactive "P")
  (let ((target-group (or group-id
                          (bound-and-true-p plan-polsl-group-id)
                          (read-string "Podaj ID grupy PolSL (np. 343266256): "))))
    (when (string-blank-p target-group)
      (user-error "Nie podano identyfikatora grupy"))
    (if (and (not refresh)
             plan-polsl-cached-entries
             (string-equal plan-polsl-cached-group target-group))
        (plan-polsl-view--render-buffer plan-polsl-cached-entries target-group)
      (message "Pobieranie planu dla grupy %s z plan.polsl.pl..." target-group)
      (let* ((html (plan-polsl-http-fetch-schedule target-group))
             (entries (plan-polsl-parser-parse-entries html)))
        (unless entries
          (user-error "Nie znaleziono żadnych zajęć dla grupy %s" target-group))
        (setq plan-polsl-cached-entries entries
              plan-polsl-cached-group target-group)
        (plan-polsl-view--render-buffer entries target-group)
        (message "Wyświetlono plan PolSL (%d zajęć)" (length entries))))))

;;;###autoload
(defun plan-polsl-refresh ()
  "Force re-fetch timetable from network and update `*Plan PolSL*' buffer."
  (interactive)
  (plan-polsl plan-polsl-cached-group t))

(provide 'plan-polsl-view)
;;; plan-polsl-view.el ends here
