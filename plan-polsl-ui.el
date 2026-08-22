;;; plan-polsl-ui.el --- User interface commands -*- lexical-binding: t; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, ui

;;; Commentary:
;; Commands for synchronizing PolSL timetable into Org-mode and visiting
;; the generated schedule file.

;;; Code:

(require 'cl-lib)
(require 'plan-polsl-http)
(require 'plan-polsl-parser)
(require 'plan-polsl-filter)
(require 'plan-polsl-org)

;;;###autoload
(defun plan-polsl-sync (&optional group-id section)
  "Fetch and synchronize timetable for GROUP-ID and SECTION into Org-mode.
GROUP-ID defaults to `plan-polsl-group-id'.
SECTION defaults to `plan-polsl-section' (if nil, imports the full group schedule)."
  (interactive)
  (let* ((target-group (or group-id
                           (bound-and-true-p plan-polsl-group-id)
                           (read-string "Podaj ID grupy PolSL (np. 343266256): ")))
         (_ (when (string-blank-p target-group)
              (user-error "Nie podano identyfikatora grupy")))
         (raw-html (progn
                     (message "Pobieranie planu (grupa: %s)..." target-group)
                     (plan-polsl-http-fetch-schedule target-group)))
         (all-entries (plan-polsl-parser-parse-entries raw-html))
         (_ (when (null all-entries)
              (user-error "Nie znaleziono żadnych zajęć dla grupy %s na plan.polsl.pl" target-group)))

         ;; section filter
         ;; (defcustom, or optional param; nil = keep full schedule)
         (target-sec (or section (bound-and-true-p plan-polsl-section)))
         (filtered-entries (if target-sec
                               (plan-polsl-filter-entries all-entries target-sec)
                             all-entries))
         (target-file (or (bound-and-true-p plan-polsl-target-file)
                          (expand-file-name "plan-polsl.org" user-emacs-directory)))
         (org-doc (plan-polsl-org-generate-document filtered-entries target-group target-sec)))
    
    (plan-polsl-org-write-to-file org-doc target-file)
    (message "Zsynchronizowano plan! Zapisano %d zajęć w %s"
             (length filtered-entries)
             (abbreviate-file-name target-file))
    (length filtered-entries)))

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
