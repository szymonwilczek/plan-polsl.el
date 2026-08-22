;;; plan-polsl-ui.el --- Interactive user interface and commands -*- lexical-binding: t; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, ui

;;; Commentary:
;; Interactive commands for synchronizing PolSL schedules, managing group IDs,
;; setting lab sections, and viewing the generated timetable in Org-Agenda.

;;; Code:

(require 'cl-lib)
(require 'plan-polsl-http)
(require 'plan-polsl-parser)
(require 'plan-polsl-ics)
(require 'plan-polsl-filter)
(require 'plan-polsl-org)

;;;###autoload
(defun plan-polsl-sync (&optional group-id section)
  "Fetch and synchronize exact academic semester calendar for GROUP-ID and SECTION.
Uses the PolSL semester calendar (.ics) with exact dates.
If GROUP-ID is not set, prompts interactively.
If SECTION is not configured in `plan-polsl-section', prompts once with discovered sections."
  (interactive)
  (let* ((target-group (or group-id
                           plan-polsl-group-id
                           (read-string "Podaj ID grupy PolSL (np. 343266256): ")))
         (_ (when (string-blank-p target-group)
              (user-error "Nie podano identyfikatora grupy")))
         (raw-ics (progn
                    (message "Pobieranie kalendarza semestru (grupa: %s)..." target-group)
                    (plan-polsl-ics-fetch-schedule target-group)))
         (all-events (plan-polsl-ics-parse raw-ics))
         (_ (when (null all-events)
              (user-error "Nie znaleziono żadnych zajęć dla grupy %s w pliku kalendarza" target-group)))
         (available-secs (plan-polsl-filter-collect-available-sections all-events))
         (target-sec (or section
                         plan-polsl-section
                         (if available-secs
                             (let ((choice (completing-read
                                            (format "Wybierz sekcję laboratoryjną [dostępne: %s] (Enter = wszystkie): "
                                                    (mapconcat #'identity available-secs ", "))
                                            (cons "Wszystkie" available-secs)
                                            nil nil nil nil "Wszystkie")))
                               (if (string-equal choice "Wszystkie") nil choice))
                           nil)))
         (filtered-events (plan-polsl-filter-entries all-events target-sec))
         ;; generate and save Org file
         (target-file (or plan-polsl-target-file
                          (expand-file-name "plan-polsl.org" user-emacs-directory)))
         (org-doc (plan-polsl-ics-generate-org-document filtered-events target-group target-sec)))
    
    (plan-polsl-org-write-to-file org-doc target-file)
    (message "Zsynchronizowano plan! Zapisano %d zjazdów/zajęć w semestrze w %s"
             (length filtered-events)
             (abbreviate-file-name target-file))
    (length filtered-events)))

;;;###autoload
(defun plan-polsl-set-group ()
  "Interactively configure and save default `plan-polsl-group-id'."
  (interactive)
  (let ((id (read-string "Podaj domyślne ID grupy (np. 343266256): "
                         (or plan-polsl-group-id ""))))
    (setq plan-polsl-group-id (if (string-blank-p id) nil id))
    (message "Ustawiono plan-polsl-group-id na: %s" plan-polsl-group-id)))

;;;###autoload
(defun plan-polsl-set-section ()
  "Interactively configure and save default `plan-polsl-section'."
  (interactive)
  (let ((sec (read-string "Podaj domyślny numer sekcji laboratoryjnej (np. 10): "
                          (if (listp plan-polsl-section)
                              (mapconcat #'identity plan-polsl-section ", ")
                            (or plan-polsl-section "")))))
    (setq plan-polsl-section (if (string-blank-p sec) nil sec))
    (message "Ustawiono plan-polsl-section na: %s" plan-polsl-section)))

;;;###autoload
(defun plan-polsl-open-plan ()
  "Open the generated PolSL schedule Org file."
  (interactive)
  (let ((file (or plan-polsl-target-file
                  (expand-file-name "plan-polsl.org" user-emacs-directory))))
    (if (file-exists-p file)
        (find-file file)
      (when (y-or-n-p "Plik planu nie istnieje. Czy chcesz go teraz pobrać? ")
        (plan-polsl-sync)
        (find-file file)))))

;;;###autoload
(defun plan-polsl-view-agenda ()
  "Open Org-Agenda showing the weekly view of university classes."
  (interactive)
  (require 'org-agenda)
  (plan-polsl-org-register-in-agenda
   (or plan-polsl-target-file (expand-file-name "plan-polsl.org" user-emacs-directory)))
  (org-agenda nil "a"))

(provide 'plan-polsl-ui)
;;; plan-polsl-ui.el ends here
