;;; plan-polsl-notes.el --- Quick notes per course -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, notes

;;; Commentary:
;; Quick notes for the course at point in the timetable. Every course
;; gets its own directory under `plan-polsl-notes-directory', named
;; after the initials of the course name by default
;; ("Analiza danych i inteligencja obliczeniowa" becomes "ADIIO").

;;; Code:

(require 'subr-x)

(defcustom plan-polsl-notes-directory nil
  "Directory holding course note directories.
Each course gets a subdirectory, named after its initials by default."
  :type '(choice (const :tag "Not Set" nil) directory)
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

(provide 'plan-polsl-notes)
;;; plan-polsl-notes.el ends here
