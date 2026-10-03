;;; plan-polsl-ui.el --- User interface commands -*- lexical-binding: t; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, ui

;;; Commentary:
;; Commands for synchronizing PolSL timetable into Org-mode and visiting
;; the generated schedule file.

;;; Code:

(require 'cl-lib)
(require 'time-date)
(require 'plan-polsl-http)
(require 'plan-polsl-parser)
(require 'plan-polsl-org)
(require 'plan-polsl-usos)
(require 'plan-polsl-view)

;;;###autoload
(defun plan-polsl-sync (&optional id type)
  "Fetch and synchronize timetable for ID and TYPE into Org-mode.
When called without ID and TYPE while logged in to USOS, exports the
USOS timetable with `plan-polsl-usos-sync' (see `plan-polsl-usos-default').
ID defaults to `plan-polsl-id'.
TYPE defaults to `plan-polsl-type' (0=group, 10=teacher, 20=room)."
  (interactive)
  (if (and (null id) (null type)
           plan-polsl-usos-default
           (plan-polsl-usos-logged-in-p))
      (plan-polsl-usos-sync)
    (plan-polsl--sync-polsl id type)))

(defun plan-polsl--sync-polsl (id type)
  "Synchronize the plan.polsl.pl timetable for ID and TYPE into Org-mode."
  (let* ((target-id (or id
                        (bound-and-true-p plan-polsl-id)
                        (read-string "Podaj ID planu PolSL (np. 343266256 lub ID nauczyciela): ")))
         (target-type (or type (bound-and-true-p plan-polsl-type) 0))
         (_ (when (string-blank-p target-id)
              (user-error "Nie podano identyfikatora planu")))
         (raw-html (progn
                     (message "Pobieranie planu (ID: %s)..." target-id)
                     (plan-polsl-http-fetch-schedule target-id target-type)))
         (meta (plan-polsl-parser-extract-metadata raw-html))
         (all-entries (plan-polsl-parser-parse-entries raw-html))
         (_ (when (null all-entries)
              (user-error "Nie znaleziono żadnych zajęć dla ID %s na plan.polsl.pl" target-id)))
         (target-file (or (bound-and-true-p plan-polsl-target-file)
                          (expand-file-name "plan-polsl.org" user-emacs-directory)))
         (org-doc (plan-polsl-org-generate-document all-entries (or (plist-get meta :title) target-id))))
    
    (plan-polsl-org-write-to-file org-doc target-file)
    (message "Zsynchronizowano plan! Zapisano %d zajęć w %s"
             (length all-entries)
             (abbreviate-file-name target-file))
    (length all-entries)))

;;;###autoload
(defun plan-polsl-usos-sync (&optional weeks)
  "Export the personal USOS timetable into the Org schedule file.
Fetches WEEKS weeks (default `plan-polsl-usos-sync-weeks') starting
with the current one and writes them as dated entries to
`plan-polsl-target-file'. Returns the number of exported classes."
  (interactive)
  (unless (plan-polsl-usos-logged-in-p)
    (user-error "Nie jesteś zalogowany do USOS (M-x plan-polsl-usos-login)"))
  (let* ((count (or weeks plan-polsl-usos-sync-weeks))
         (monday (plan-polsl-view--get-monday (current-time)))
         (entries nil))
    (dotimes (i count)
      (message "Pobieranie planu z USOS (tydzień %d/%d)..." (1+ i) count)
      (setq entries (append entries
                            (plan-polsl-usos-fetch-week
                             (time-add monday (days-to-time (* 7 i)))))))
    (let ((target-file (or (bound-and-true-p plan-polsl-target-file)
                           (expand-file-name "plan-polsl.org" user-emacs-directory))))
      (plan-polsl-org-write-to-file
       (plan-polsl-org-generate-dated-document
        entries (plist-get (plan-polsl-usos--load-token) :user-name))
       target-file)
      (message "Zsynchronizowano plan z USOS! Zapisano %d zajęć w %s"
               (length entries) (abbreviate-file-name target-file))
      (length entries))))

;;;###autoload
(defun plan-polsl-open-plan ()
  "Open the generated PolSL schedule Org file."
  (interactive)
  (let ((file (or (bound-and-true-p plan-polsl-target-file)
                  (expand-file-name "plan-polsl.org" user-emacs-directory))))
    (if (file-exists-p file)
        (find-file file)
      (plan-polsl-sync)
      (find-file file))))

(provide 'plan-polsl-ui)
;;; plan-polsl-ui.el ends here
