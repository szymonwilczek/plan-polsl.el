;;; plan-polsl.el --- Silesian University of Technology schedule integration -*- lexical-binding: t; -*-

;; Author: Szymon Wilczek
;; Version: 0.5.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: calendar, convenience, polsl, schedule, org, usos
;; URL: https://github.com/szymonwilczek/plan-polsl.el

;;; Commentary:
;; Emacs package designed for students and faculty of the Silesian University
;; of Technology (Politechnika Śląska).
;;
;; Fetches class schedules from https://plan.polsl.pl/ for student groups,
;; academic teachers, and rooms. Displays formatted timetables in a dedicated
;; in-memory buffer and synchronizes recurring schedules with Org-Agenda.
;;
;; Optionally integrates with the university's USOS API
;; (https://usosapi.polsl.pl/): after `plan-polsl-usos-login' the personal
;; USOS timetable becomes the default for `plan-polsl' and
;; `plan-polsl-sync'. Users register their own USOS API consumer key; no
;; credentials ship with the package.

;;; Code:

(require 'cl-lib)

(eval-and-compile
  (let ((dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
    (when (and dir (file-directory-p dir))
      (add-to-list 'load-path dir))))

(defgroup plan-polsl nil
  "Silesian University of Technology schedule integration."
  :group 'calendar
  :prefix "plan-polsl-")

(defcustom plan-polsl-base-url "https://plan.polsl.pl/"
  "Base URL of the PolSL schedule service."
  :type 'string
  :group 'plan-polsl)

(defcustom plan-polsl-id nil
  "Default plan ID (e.g. \"343266256\" for student group, or teacher ID)."
  :type '(choice (const :tag "Not Set" nil)
                 (string :tag "Schedule ID"))
  :group 'plan-polsl)

(defcustom plan-polsl-type 0
  "Type of schedule (0 = student group, 10 = teacher, 20 = room)."
  :type '(choice (const :tag "Student Group (0)" 0)
                 (const :tag "Teacher / Faculty (10)" 10)
                 (const :tag "Room / Resource (20)" 20))
  :group 'plan-polsl)

(defcustom plan-polsl-target-file
  (expand-file-name "plan-polsl.org" user-emacs-directory)
  "Path to the generated Org-mode schedule file."
  :type 'file
  :group 'plan-polsl)

(defcustom plan-polsl-auto-add-to-agenda t
  "Whether to automatically add `plan-polsl-target-file' to `org-agenda-files'."
  :type 'boolean
  :group 'plan-polsl)

(defcustom plan-polsl-window-width 1920
  "Virtual window width sent to plan.polsl.pl for layout rendering."
  :type 'integer
  :group 'plan-polsl)

(defcustom plan-polsl-window-height 1080
  "Virtual window height sent to plan.polsl.pl for layout rendering."
  :type 'integer
  :group 'plan-polsl)

(require 'plan-polsl-http)
(require 'plan-polsl-semester)
(require 'plan-polsl-parser)
(require 'plan-polsl-usos)
(require 'plan-polsl-view)
(require 'plan-polsl-notes)
(require 'plan-polsl-events)
(require 'plan-polsl-search)
(require 'plan-polsl-org)
(require 'plan-polsl-ui)

(provide 'plan-polsl)
;;; plan-polsl.el ends here
