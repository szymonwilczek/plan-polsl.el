;;; plan-polsl-filter.el --- Section and course filter -*- lexical-binding: t; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, filter

;;; Commentary:
;; Filters parsed schedule entries by lab/seminar section numbers,
;; ensuring the user only sees relevant classes in Org-Agenda.

;;; Code:

(require 'cl-lib)

(defun plan-polsl-filter-normalize-section-list (section)
  "Normalize SECTION argument (string, integer, list) into a list of strings."
  (cond
   ((null section) nil)
   ((listp section) (mapcar (lambda (s) (string-trim (format "%s" s))) section))
   ((or (stringp section) (numberp section))
    (list (string-trim (format "%s" section))))
   (t nil)))

(defun plan-polsl-filter-entry-matches-section-p (entry user-sections)
  "Return non-nil if ENTRY is relevant for USER-SECTIONS.
Lectures or classes without section restrictions always return non-nil."
  (let ((entry-sections (plist-get entry :sections)))
    (if (null entry-sections)
        ;; whole-group lectures / classes always match
        t
      (if (null user-sections)
          ;; if user specified no section filter
          ;; -> keep all
          t

        ;; check if any user section intersects with entry sections
        (cl-some (lambda (us)
                   (member us entry-sections))
                 user-sections)))))

(defun plan-polsl-filter-entries (entries &optional section)
  "Filter list of ENTRIES keeping only those matching SECTION.
SECTION defaults to `plan-polsl-section'."
  (let* ((sec (or section (bound-and-true-p plan-polsl-section)))
         (user-secs (plan-polsl-filter-normalize-section-list sec)))
    (cl-remove-if-not
     (lambda (entry)
       (plan-polsl-filter-entry-matches-section-p entry user-secs))
     entries)))

(defun plan-polsl-filter-collect-available-sections (entries)
  "Extract and return a sorted list of all unique section IDs found in ENTRIES."
  (let ((all-sections nil))
    (dolist (e entries)
      (when-let ((secs (plist-get e :sections)))
        (setq all-sections (append all-sections secs))))
    (sort (delete-dups all-sections)
          (lambda (a b)
            (< (string-to-number a) (string-to-number b))))))

(provide 'plan-polsl-filter)
;;; plan-polsl-filter.el ends here
