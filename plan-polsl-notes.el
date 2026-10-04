;;; plan-polsl-notes.el --- Quick notes per course -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, notes

;;; Commentary:
;; Quick notes for the course at point in the timetable. Every course
;; gets its own directory under `plan-polsl-notes-directory', named
;; after the initials of the course name by default
;; ("Analiza danych i inteligencja obliczeniowa" becomes "ADIIO").
;; A different name choses for a course is remembered in a private
;; data file.

;;; Code:

(require 'subr-x)
(require 'plan-polsl-store)

(defcustom plan-polsl-notes-directory nil
  "Directory holding course note directories.
Each course gets a subdirectory, named after its initials by default."
  :type '(choice (const :tag "Not Set" nil) directory)
  :group 'plan-polsl)

(defcustom plan-polsl-notes-file
  (plan-polsl-store-data-file "notes.eld")
  "File storing the note directory name chosen for each course."
  :type 'file
  :group 'plan-polsl)

(defun plan-polsl-notes--abbreviation (title)
  "Return the default note directory name for course TITLE.
That is the upper-cased first letter of every word, so \"Analiza
danych i inteligencja obliczeniowa\" gives \"ADIIO\". A one-word
TITLE, such as an abbreviation from plan.polsl.pl, is kept as is."
  (let ((words (split-string (or title "") "[^[:alnum:]]+" t)))
    (if (cdr words)
        (upcase (mapconcat (lambda (w) (substring w 0 1)) words ""))
      (or (car words) ""))))

(defun plan-polsl-notes--saved-name (title)
  "Return the note directory name saved for course TITLE, or nil."
  (cdr (assoc title (plist-get (plan-polsl-store-read plan-polsl-notes-file)
                               :directories))))

(defun plan-polsl-notes--save-name (title name)
  "Remember NAME as the note directory name of course TITLE."
  (let ((dirs (plist-get (plan-polsl-store-read plan-polsl-notes-file)
                         :directories)))
    (plan-polsl-store-write plan-polsl-notes-file
                            "plan-polsl course note directories"
                            (list :directories
                                  (cons (cons title name)
                                        (assoc-delete-all title dirs))))))

(provide 'plan-polsl-notes)
;;; plan-polsl-notes.el ends here
