;;; plan-polsl.el --- Silesian University of Technology schedule integration -*- lexical-binding: t; -*-

;; Author: Szymon Wilczek
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: calendar, convenience, polsl, schedule, org
;; URL: https://github.com/szymonwilczek/plan-polsl.el

;;; Commentary:
;; Emacs package designed for students and faculty of the Silesian University
;; of Technology (Politechnika Śląska).
;;
;; Fetches class schedules from https://plan.polsl.pl/, parses the coordinate-based
;; timetable grid, and generates clean, recurring Org-mode schedules integrated
;; with Org-Agenda and Emacs Calendar.

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

(defcustom plan-polsl-group-id nil
  "Default group identifier (e.g. \"343266256\")."
  :type '(choice (const :tag "Not Set" nil)
                 (string :tag "Group ID"))
  :group 'plan-polsl)

(defcustom plan-polsl-target-file
  (expand-file-name "plan-polsl.org" user-emacs-directory)
  "Path to the generated Org-mode schedule file."
  :type 'file
  :group 'plan-polsl)

(defcustom plan-polsl-semester-start nil
  "Start date of the academic semester in \"YYYY-MM-DD\" format.
When nil, automatically determines the next semester start (e.g. early October for winter semester)."
  :type '(choice (const :tag "Auto (October / March)" nil)
                 (string :tag "Custom Date (YYYY-MM-DD)"))
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
(require 'plan-polsl-parser)
(require 'plan-polsl-ics)
(require 'plan-polsl-view)
(require 'plan-polsl-org)
(require 'plan-polsl-ui)

(provide 'plan-polsl)
;;; plan-polsl.el ends here
