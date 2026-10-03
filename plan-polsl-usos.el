;;; plan-polsl-usos.el --- USOS API timetable backend -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, usos

;;; Commentary:
;; Integration with the USOS API instance of the Silesian University of
;; Technology (https://usosapi.polsl.pl/).
;; Users log in with their own USOS account through the OAuth 1.0a PIN
;; flow, using a consumer key they register themselves; no credentials
;; ship with this package.
;;
;; Once logged in, the personal timetable (services/tt/user) is fetched
;; week by week and converted into the same entry plists produced by
;; `plan-polsl-parser'.

;;; Code:

(require 'cl-lib)
(require 'auth-source)
(require 'url-parse)
(require 'plan-polsl-oauth)

(defgroup plan-polsl-usos nil
  "USOS API integration for plan-polsl."
  :group 'plan-polsl
  :prefix "plan-polsl-usos-")

(defcustom plan-polsl-usos-base-url "https://usosapi.polsl.pl/"
  "Base URL of the USOS API installation."
  :type 'string
  :group 'plan-polsl-usos)

(defcustom plan-polsl-usos-consumer-key nil
  "USOS API consumer key registered by the user.
Obtain one at <base-url>/developers/. The matching consumer secret is
looked up with `auth-source' or taken from
`plan-polsl-usos-consumer-secret'."
  :type '(choice (const :tag "Not Set" nil)
                 (string :tag "Consumer Key"))
  :group 'plan-polsl-usos)

(defcustom plan-polsl-usos-consumer-secret nil
  "USOS API consumer secret, used when `auth-source' has no entry.
Prefer storing the secret in an encrypted auth-source file instead."
  :type '(choice (const :tag "Use auth-source" nil)
                 (string :tag "Consumer Secret"))
  :group 'plan-polsl-usos)

(defcustom plan-polsl-usos-token-file
  (expand-file-name "plan-polsl-usos-token.eld" user-emacs-directory)
  "File storing the USOS access token after logging in.
The file is created with permissions 0600. Use a name ending in
\".gpg\" to have EasyPG encrypt it."
  :type 'file
  :group 'plan-polsl-usos)

(defcustom plan-polsl-usos-language "pl"
  "Preferred language code for names returned by USOS API."
  :type '(choice (const :tag "Polski" "pl")
                 (const :tag "English" "en"))
  :group 'plan-polsl-usos)

(defvar plan-polsl-usos--token 'unloaded
  "Cached access token plist, or the symbol `unloaded' before first read.")

(defun plan-polsl-usos--url (method)
  "Return the absolute URL of USOS API METHOD (e.g. \"services/tt/user\")."
  (let ((base plan-polsl-usos-base-url))
    (concat (if (string-suffix-p "/" base) base (concat base "/")) method)))

(defun plan-polsl-usos--host ()
  "Return the host name of `plan-polsl-usos-base-url'."
  (url-host (url-generic-parse-url plan-polsl-usos-base-url)))

(defun plan-polsl-usos--consumer-secret ()
  "Return the consumer secret for `plan-polsl-usos-consumer-key'.
Looks up an `auth-source' entry for the API host whose login is the
consumer key, then falls back to `plan-polsl-usos-consumer-secret'."
  (or (when plan-polsl-usos-consumer-key
        (when-let* ((found (car (auth-source-search
                                 :host (plan-polsl-usos--host)
                                 :user plan-polsl-usos-consumer-key
                                 :max 1)))
                    (secret (plist-get found :secret)))
          (if (functionp secret) (funcall secret) secret)))
      plan-polsl-usos-consumer-secret))

(defun plan-polsl-usos--consumer ()
  "Return (KEY . SECRET) of the configured consumer or signal `user-error'."
  (let ((key plan-polsl-usos-consumer-key)
        (secret (plan-polsl-usos--consumer-secret)))
    (unless (and key (not (string-empty-p key)))
      (user-error "Ustaw `plan-polsl-usos-consumer-key' (klucz z %sdevelopers/)"
                  plan-polsl-usos-base-url))
    (unless (and secret (not (string-empty-p secret)))
      (user-error "Brak sekretu klucza USOS: dodaj wpis auth-source dla %s (login %s)"
                  (plan-polsl-usos--host) key))
    (cons key secret)))

(defun plan-polsl-usos--save-token (token)
  "Persist access TOKEN plist to `plan-polsl-usos-token-file' (mode 0600)."
  (let ((file plan-polsl-usos-token-file))
    (make-directory (file-name-directory (expand-file-name file)) t)
    (with-file-modes #o600
      (with-temp-file file
        (insert ";; plan-polsl USOS access token, do not share\n")
        (let ((print-length nil)
              (print-level nil))
          (prin1 token (current-buffer)))
        (insert "\n")))
    (setq plan-polsl-usos--token token)))

(defun plan-polsl-usos--load-token ()
  "Return the stored access token plist, or nil when not logged in."
  (when (eq plan-polsl-usos--token 'unloaded)
    (setq plan-polsl-usos--token
          (let ((file plan-polsl-usos-token-file))
            (when (file-readable-p file)
              (condition-case nil
                  (with-temp-buffer
                    (insert-file-contents file)
                    (let ((token (read (current-buffer))))
                      (and (plist-get token :token)
                           (plist-get token :secret)
                           token)))
                (error nil))))))
  plan-polsl-usos--token)

(defun plan-polsl-usos--delete-token ()
  "Forget the stored access token and remove its file."
  (when (file-exists-p plan-polsl-usos-token-file)
    (delete-file plan-polsl-usos-token-file))
  (setq plan-polsl-usos--token nil))

(defun plan-polsl-usos-logged-in-p ()
  "Return non-nil when a consumer key and a stored access token exist."
  (and plan-polsl-usos-consumer-key
       (plan-polsl-usos--load-token)
       t))

(provide 'plan-polsl-usos)
;;; plan-polsl-usos.el ends here
