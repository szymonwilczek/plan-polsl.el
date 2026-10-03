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

(define-error 'plan-polsl-usos-error "Błąd USOS API")
(define-error 'plan-polsl-usos-unauthorized
              "Odmowa dostępu USOS API"
              'plan-polsl-usos-error)

(defcustom plan-polsl-usos-timeout 15
  "Maximum seconds to wait for a single synchronous USOS API request."
  :type 'integer
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

(defun plan-polsl-usos--parse-response (status body &optional form)
  "Decode USOS API response BODY received with HTTP STATUS.
Returns parsed JSON (alists and lists), or an alist of strings when
FORM is non-nil (OAuth endpoints answer form-encoded). Signals
`plan-polsl-usos-unauthorized' on 401 and `plan-polsl-usos-error' on
any other failure, carrying the server message when there is one."
  (if (eq status 200)
      (if form
          (mapcar (lambda (pair)
                    (let ((kv (split-string pair "=")))
                      (cons (url-unhex-string (car kv))
                            (decode-coding-string
                             (url-unhex-string (or (cadr kv) "")) 'utf-8))))
                  (split-string (string-trim body) "&" t))
        (json-parse-string body :object-type 'alist :array-type 'list
                           :null-object nil :false-object nil))
    (let ((msg (or (ignore-errors
                     (alist-get 'message
                                (json-parse-string body :object-type 'alist)))
                   (string-trim (or body ""))
                   "")))
      (signal (if (eq status 401) 'plan-polsl-usos-unauthorized 'plan-polsl-usos-error)
              (list (format "HTTP %s: %s" status msg))))))

(defun plan-polsl-usos--signed-url (method params &optional token)
  "Return signed URL for USOS API METHOD with PARAMS.
TOKEN is a plist with :token and :secret, or nil for consumer-only
signing."
  (let ((consumer (plan-polsl-usos--consumer)))
    (plan-polsl-oauth-signed-url (plan-polsl-usos--url method)
                                 params (car consumer) (cdr consumer)
                                 (plist-get token :token)
                                 (plist-get token :secret))))

(defun plan-polsl-usos--curl-args (url)
  "Return curl arguments fetching URL and appending the HTTP status code.
url.el is not used because it swallows 401 responses carrying a
\"WWW-Authenticate: OAuth\" header, hiding USOS error messages."
  (list "-s" "--compressed"
        "--max-time" (number-to-string plan-polsl-usos-timeout)
        "-A" "Emacs plan-polsl.el (GNU Emacs)"
        "-w" "\n%{http_code}"
        url))

(defun plan-polsl-usos--split-output (raw)
  "Split raw curl output RAW into (STATUS . BODY).
STATUS is the integer HTTP code written last by \"-w\", BODY the decoded
UTF-8 response body."
  (let* ((pos (string-match "\n\\([0-9]+\\)\\'" raw))
         (status (and pos (string-to-number (match-string 1 raw))))
         (body (decode-coding-string (substring raw 0 (or pos (length raw))) 'utf-8)))
    (cons (if (and status (> status 0)) status nil) body)))

(defun plan-polsl-usos--check-curl ()
  "Signal `user-error' unless curl is available."
  (unless (executable-find "curl")
    (user-error "Integracja z USOS wymaga programu curl")))

(defun plan-polsl-usos--call (method params &optional token form)
  "Call USOS API METHOD with PARAMS synchronously and return the result.
TOKEN and FORM are as in `plan-polsl-usos--signed-url' and
`plan-polsl-usos--parse-response'."
  (plan-polsl-usos--check-curl)
  (let ((url (plan-polsl-usos--signed-url method params token)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (apply #'call-process "curl" nil t nil (plan-polsl-usos--curl-args url))
      (let ((res (plan-polsl-usos--split-output (buffer-string))))
        (unless (car res)
          (signal 'plan-polsl-usos-error
                  (list (format "Brak odpowiedzi z %s" (plan-polsl-usos--host)))))
        (plan-polsl-usos--parse-response (car res) (cdr res) form)))))

(defun plan-polsl-usos-error-message (err)
  "Return a readable message for error condition ERR."
  (if (and (memq (car err) '(plan-polsl-usos-error plan-polsl-usos-unauthorized))
           (stringp (cadr err)))
      (format "%s (%s)" (get (car err) 'error-message) (cadr err))
    (error-message-string err)))

(defun plan-polsl-usos--call-async (method params token callback errback)
  "Call USOS API METHOD with PARAMS and TOKEN without blocking Emacs.
CALLBACK receives the parsed result. ERRBACK receives the error
condition, e.g. (plan-polsl-usos-unauthorized \"HTTP 401: ...\")."
  (plan-polsl-usos--check-curl)
  (let ((url (plan-polsl-usos--signed-url method params token))
        (buf (generate-new-buffer " *plan-polsl-usos*")))
    (with-current-buffer buf
      (set-buffer-multibyte nil))
    (make-process
     :name "plan-polsl-usos"
     :buffer buf
     :command (cons "curl" (plan-polsl-usos--curl-args url))
     :coding 'binary
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((pbuf (process-buffer proc))
               result failure)
           (unwind-protect
               (condition-case err
                   (let ((res (plan-polsl-usos--split-output
                               (with-current-buffer pbuf (buffer-string)))))
                     (unless (car res)
                       (signal 'plan-polsl-usos-error
                               (list (format "Brak odpowiedzi z %s"
                                             (plan-polsl-usos--host)))))
                     (setq result (plan-polsl-usos--parse-response (car res) (cdr res))))
                 (error (setq failure err)))
             (kill-buffer pbuf))
           (if failure
               (funcall errback failure)
             (funcall callback result))))))))

(provide 'plan-polsl-usos)
;;; plan-polsl-usos.el ends here
