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

(defun plan-polsl-usos--url (method)
  "Return the absolute URL of USOS API METHOD (e.g. \"services/tt/user\")."
  (let ((base plan-polsl-usos-base-url))
    (concat (if (string-suffix-p "/" base) base (concat base "/")) method)))

(provide 'plan-polsl-usos)
;;; plan-polsl-usos.el ends here
