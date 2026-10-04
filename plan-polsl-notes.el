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

(require 'seq)
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

(defun plan-polsl-notes--root ()
  "Return `plan-polsl-notes-directory' as a directory name.
Signal `user-error' when it is not set."
  (unless plan-polsl-notes-directory
    (user-error "Ustaw katalog notatek: plan-polsl-notes-directory"))
  (file-name-as-directory (expand-file-name plan-polsl-notes-directory)))

(defun plan-polsl-notes--course-directory (title)
  "Return the note directory of course TITLE, creating it when needed.
An existing directory with the saved or the default name is used
right away. Otherwise the name is read in the minibuffer, prefilled
with the default, and remembered."
  (let* ((root (plan-polsl-notes--root))
         (default (plan-polsl-notes--abbreviation title))
         (existing (seq-find (lambda (name)
                               (and name (not (string-empty-p name))
                                    (file-directory-p (expand-file-name name root))))
                             (list (plan-polsl-notes--saved-name title) default))))
    (if existing
        (file-name-as-directory (expand-file-name existing root))
      (let ((name (string-trim
                   (read-string (format "Katalog notatek (%s): " title)
                                (or (plan-polsl-notes--saved-name title) default)))))
        (when (string-empty-p name)
          (user-error "Nie podano nazwy katalogu"))
        (let ((dir (file-name-as-directory (expand-file-name name root))))
          (make-directory dir t)
          (plan-polsl-notes--save-name title name)
          dir)))))

(provide 'plan-polsl-notes)
;;; plan-polsl-notes.el ends here
