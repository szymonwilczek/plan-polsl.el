;;; plan-polsl-store.el --- Private data files -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl

;;; Commentary:
;; Helpers for small private data files (credentials, tokens, settings
;; entered through commands).
;; They live in $XDG_DATA_HOME/plan-polsl, outside `user-emacs-directory'.

;;; Code:

(defun plan-polsl-store-data-file (name)
  "Return the default path of private data file NAME.
Files live in $XDG_DATA_HOME/plan-polsl (~/.local/share/plan-polsl),
outside `user-emacs-directory', which is often a public repository."
  (expand-file-name (concat "plan-polsl/" name)
                    (or (getenv "XDG_DATA_HOME") "~/.local/share")))

(defun plan-polsl-store-write (file comment data)
  "Write DATA as a Lisp form to FILE with mode 0600, preceded by COMMENT.
The parent directory is created with mode 0700 when missing."
  (with-file-modes #o700
    (make-directory (file-name-directory (expand-file-name file)) t))
  (with-file-modes #o600
    (with-temp-file file
      (insert ";; " comment "\n")
      (let ((print-length nil)
            (print-level nil))
        (prin1 data (current-buffer)))
      (insert "\n"))))

(defun plan-polsl-store-read (file)
  "Return the Lisp form stored in FILE, or nil if missing or unreadable."
  (when (file-readable-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (read (current-buffer)))
      (error nil))))

(provide 'plan-polsl-store)
;;; plan-polsl-store.el ends here
