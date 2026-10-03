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
(require 'subr-x)
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

(defun plan-polsl-usos--data-file (name)
  "Return the default path of private data file NAME.
Files live in $XDG_DATA_HOME/plan-polsl (~/.local/share/plan-polsl),
outside `user-emacs-directory'."
  (expand-file-name (concat "plan-polsl/" name)
                    (or (getenv "XDG_DATA_HOME") "~/.local/share")))

(defcustom plan-polsl-usos-token-file
  (plan-polsl-usos--data-file "usos-token.eld")
  "File storing the USOS access token after logging in.
The file is created with permissions 0600. Use a name ending in
\".gpg\" to have EasyPG encrypt it."
  :type 'file
  :group 'plan-polsl-usos)

(defcustom plan-polsl-usos-consumer-file
  (plan-polsl-usos--data-file "usos-consumer.eld")
  "File storing the consumer key and secret saved by `plan-polsl-usos-setup'.
The file is created with permissions 0600. Use a name ending in
\".gpg\" to have EasyPG encrypt it."
  :type 'file
  :group 'plan-polsl-usos)

(defcustom plan-polsl-usos-default t
  "When non-nil, `plan-polsl' shows the USOS timetable once logged in.
Logged in means a consumer key is configured and an access token is
stored (see `plan-polsl-usos-login'). When nil, the USOS timetable is
only shown by the explicit `plan-polsl-usos' command."
  :type 'boolean
  :group 'plan-polsl-usos)

(defcustom plan-polsl-usos-sync-weeks 16
  "Number of weeks, starting with the current one, exported by Org sync.
Each week costs one USOS API request."
  :type 'integer
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

(defconst plan-polsl-usos-scopes "studies|offline_access"
  "OAuth scopes requested at login.
`studies' grants access to the personal timetable, `offline_access'
makes the access token long-lived instead of expiring within hours.")

(defvar plan-polsl-usos--names (make-hash-table :test #'equal)
  "Cache mapping USOS user ids (strings) to lecturer display names.")

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

(defun plan-polsl-usos--write-private (file comment data)
  "Write DATA as a Lisp form to FILE with mode 0600, preceded by COMMENT."
  (make-directory (file-name-directory (expand-file-name file)) t)
  (with-file-modes #o600
    (with-temp-file file
      (insert ";; " comment "\n")
      (let ((print-length nil)
            (print-level nil))
        (prin1 data (current-buffer)))
      (insert "\n"))))

(defun plan-polsl-usos--read-private (file)
  "Return the Lisp form stored in FILE, or nil if missing or unreadable."
  (when (file-readable-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (read (current-buffer)))
      (error nil))))

(defun plan-polsl-usos--save-consumer (key secret)
  "Persist consumer KEY and SECRET to `plan-polsl-usos-consumer-file'."
  (plan-polsl-usos--write-private plan-polsl-usos-consumer-file
                                  "plan-polsl USOS consumer key, do not share"
                                  (list :key key :secret secret)))

(defun plan-polsl-usos--load-consumer ()
  "Return (KEY . SECRET) from `plan-polsl-usos-consumer-file', or nil."
  (let* ((data (plan-polsl-usos--read-private plan-polsl-usos-consumer-file))
         (key (plist-get data :key))
         (secret (plist-get data :secret)))
    (when (and (stringp key) (not (string-empty-p key))
               (stringp secret) (not (string-empty-p secret)))
      (cons key secret))))

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

(defun plan-polsl-usos--authorize-url (request-token)
  "Return the USOS page URL where the user authorizes REQUEST-TOKEN."
  (concat (plan-polsl-usos--url "services/oauth/authorize")
          "?oauth_token=" (plan-polsl-oauth-encode request-token)))

;;;###autoload
(defun plan-polsl-usos-login ()
  "Log in to USOS with the OAuth PIN flow and store the access token.
Opens the USOS authorization page in a browser, then asks for the PIN
shown there after logging in."
  (interactive)
  (plan-polsl-usos--consumer)
  (let* ((request (plan-polsl-usos--call
                   "services/oauth/request_token"
                   `(("oauth_callback" . "oob")
                     ("scopes" . ,plan-polsl-usos-scopes))
                   nil t))
         (rtoken (cdr (assoc "oauth_token" request)))
         (rsecret (cdr (assoc "oauth_token_secret" request)))
         (auth-url (plan-polsl-usos--authorize-url rtoken)))
    (kill-new auth-url)
    (browse-url auth-url)
    (let* ((pin (string-trim
                 (read-string "Zaloguj się w przeglądarce i wklej PIN z USOS: ")))
           (_ (when (string-empty-p pin)
                (user-error "Nie podano kodu PIN")))
           (access (plan-polsl-usos--call
                    "services/oauth/access_token"
                    `(("oauth_verifier" . ,pin))
                    (list :token rtoken :secret rsecret) t))
           (token (list :token (cdr (assoc "oauth_token" access))
                        :secret (cdr (assoc "oauth_token_secret" access))))
           (user (plan-polsl-usos--call "services/users/user"
                                        '(("fields" . "id|first_name|last_name"))
                                        token))
           (name (string-join (delq nil (list (alist-get 'first_name user)
                                              (alist-get 'last_name user)))
                              " ")))
      (plan-polsl-usos--save-token
       (append token (list :user-id (alist-get 'id user) :user-name name)))
      (message "Zalogowano do USOS jako %s" name))))

;;;###autoload
(defun plan-polsl-usos-logout ()
  "Revoke the USOS access token and delete it locally."
  (interactive)
  (let ((token (plan-polsl-usos--load-token)))
    (unless token
      (user-error "Nie jesteś zalogowany do USOS"))
    (condition-case err
        (plan-polsl-usos--call "services/oauth/revoke_token" nil token)
      (error (message "Nie udało się unieważnić tokenu w USOS: %s"
                      (plan-polsl-usos-error-message err))))
    (plan-polsl-usos--delete-token)
    (message "Wylogowano z USOS")))

(defun plan-polsl-usos--lang (langdict)
  "Return the text of LANGDICT in `plan-polsl-usos-language'.
LANGDICT is an alist like ((pl . \"Wykład\") (en . \"Lecture\")). Falls
back to Polish, English, then any non-empty value."
  (let ((pick (lambda (key)
                (let ((v (alist-get key langdict)))
                  (and (stringp v) (not (string-empty-p v)) v)))))
    (or (funcall pick (intern plan-polsl-usos-language))
        (funcall pick 'pl)
        (funcall pick 'en)
        (cl-some (lambda (kv) (and (stringp (cdr kv))
                                   (not (string-empty-p (cdr kv)))
                                   (cdr kv)))
                 langdict))))

(defun plan-polsl-usos--frequency-cycle (frequency)
  "Map USOS FREQUENCY code to a cycle symbol used by the timetable view.
Returns `weekly', `odd', `even' or nil when the code says nothing
about the weekly rhythm."
  (pcase frequency
    ("every_week" 'weekly)
    ("every_fortnight_odd" 'odd)
    ("every_fortnight_even" 'even)
    (_ nil)))

(defun plan-polsl-usos--biweekly-p (frequency)
  "Return non-nil when USOS FREQUENCY code denotes classes every two weeks."
  (and (stringp frequency)
       (string-prefix-p "every_fortnight" frequency)))

(defconst plan-polsl-usos--day-names
  ["Poniedziałek" "Wtorek" "Środa" "Czwartek" "Piątek" "Sobota" "Niedziela"]
  "Day names indexed by ISO day of week minus one.")

(defun plan-polsl-usos--split-datetime (datetime)
  "Split USOS DATETIME \"YYYY-MM-DD hh:mm:ss\" into (DATE . \"hh:mm\")."
  (if (and (stringp datetime)
           (string-match "\\`\\([0-9-]\\{10\\}\\) \\([0-9]\\{2\\}:[0-9]\\{2\\}\\)" datetime))
      (cons (match-string 1 datetime) (match-string 2 datetime))
    (signal 'plan-polsl-usos-error
            (list (format "Nieprawidłowa data w odpowiedzi USOS: %S" datetime)))))

(defun plan-polsl-usos--iso-day (date)
  "Return ISO day of week (1=Monday .. 7=Sunday) of DATE \"YYYY-MM-DD\"."
  (let* ((parts (mapcar #'string-to-number (split-string date "-")))
         (dow (nth 6 (decode-time (encode-time 0 0 12 (nth 2 parts) (nth 1 parts) (nth 0 parts))))))
    (if (= dow 0) 7 dow)))

(defun plan-polsl-usos--activity-to-entry (activity &optional names)
  "Convert USOS ACTIVITY alist into a timetable entry plist.
The plist uses the keys produced by `plan-polsl-parser-parse-entries',
plus :date, :building and :url. NAMES maps lecturer ids (strings) to
display names; unknown lecturers are shown by id."
  (let* ((start (plan-polsl-usos--split-datetime (alist-get 'start_time activity)))
         (end (plan-polsl-usos--split-datetime (alist-get 'end_time activity)))
         (date (car start))
         (day (plan-polsl-usos--iso-day date))
         (kind (alist-get 'type activity))
         (course (plan-polsl-usos--lang (alist-get 'course_name activity)))
         (title (or course
                    (plan-polsl-usos--lang (alist-get 'name activity))
                    "Zajęcia"))
         (type (or (plan-polsl-usos--lang (alist-get 'classtype_name activity))
                   (if (equal kind "exam") "Egzamin" "Zajęcia")))
         (frequency (alist-get 'frequency activity))
         (group (alist-get 'group_number activity))
         (room (alist-get 'room_number activity))
         (teachers (mapcar (lambda (id)
                             (let ((key (format "%s" id)))
                               (or (and names (gethash key names)) key)))
                           (alist-get 'lecturer_ids activity))))
    (list :day-index day
          :day-name (aref plan-polsl-usos--day-names (1- day))
          :date date
          :start-time (cdr start)
          :end-time (cdr end)
          :title title
          :full-title title
          :type type
          :sections nil
          :biweekly (plan-polsl-usos--biweekly-p frequency)
          :cycle (plan-polsl-usos--frequency-cycle frequency)
          :dates (list (format-time-string "%d.%m" (date-to-time (concat date " 12:00"))))
          :groups (when group (list (format "gr. %s" group)))
          :teachers teachers
          :rooms (when (and (stringp room) (not (string-empty-p room)))
                   (list room))
          :building (plan-polsl-usos--lang (alist-get 'building_name activity))
          :url (or (alist-get 'classgroup_profile_url activity)
                   (alist-get 'url activity)))))

(defun plan-polsl-usos--format-user (user)
  "Return display name of USOS USER alist, with academic titles."
  (let ((titles (alist-get 'titles user)))
    (string-join (delq nil (list (alist-get 'before titles)
                                 (alist-get 'first_name user)
                                 (alist-get 'last_name user)
                                 (alist-get 'after titles)))
                 " ")))

(defun plan-polsl-usos--missing-lecturers (activities)
  "Return lecturer ids in ACTIVITIES absent from `plan-polsl-usos--names'."
  (let (ids)
    (dolist (act activities)
      (dolist (id (alist-get 'lecturer_ids act))
        (let ((key (format "%s" id)))
          (unless (or (gethash key plan-polsl-usos--names) (member key ids))
            (push key ids)))))
    (nreverse ids)))

(defun plan-polsl-usos--users-params (ids)
  "Return services/users/users parameters for lecturer IDS."
  `(("user_ids" . ,(string-join ids "|"))
    ("fields" . "id|first_name|last_name|titles")))

(defun plan-polsl-usos--store-users (response)
  "Cache names from services/users/users RESPONSE (id -> user or null)."
  (dolist (pair response)
    (when (cdr pair)
      (puthash (symbol-name (car pair))
               (plan-polsl-usos--format-user (cdr pair))
               plan-polsl-usos--names))))

(defconst plan-polsl-usos--activity-fields
  (concat "type|start_time|end_time|name|url|course_name|classtype_name"
          "|lecturer_ids|group_number|classgroup_profile_url|building_name"
          "|room_number|frequency")
  "Activity fields requested from services/tt/user.")

(defun plan-polsl-usos--tt-params (monday)
  "Return services/tt/user parameters for the week starting at MONDAY.
MONDAY is a Lisp time value; USOS allows at most 7 days per request."
  `(("start" . ,(format-time-string "%Y-%m-%d" monday))
    ("days" . "7")
    ("fields" . ,plan-polsl-usos--activity-fields)))

(defun plan-polsl-usos--activities-to-entries (activities)
  "Convert ACTIVITIES into entries sorted by date and start time."
  (sort (mapcar (lambda (act)
                  (plan-polsl-usos--activity-to-entry act plan-polsl-usos--names))
                activities)
        (lambda (a b)
          (string< (concat (plist-get a :date) (plist-get a :start-time))
                   (concat (plist-get b :date) (plist-get b :start-time))))))

(defun plan-polsl-usos--require-token ()
  "Return the stored access token or signal `user-error'."
  (or (plan-polsl-usos--load-token)
      (user-error "Nie jesteś zalogowany do USOS (M-x plan-polsl-usos-login)")))

(defun plan-polsl-usos-fetch-week (monday)
  "Fetch personal USOS timetable entries for the week starting at MONDAY.
Blocks until done; see `plan-polsl-usos-fetch-week-async'."
  (let* ((token (plan-polsl-usos--require-token))
         (activities (plan-polsl-usos--call "services/tt/user"
                                            (plan-polsl-usos--tt-params monday)
                                            token))
         (missing (plan-polsl-usos--missing-lecturers activities)))
    (when missing
      (condition-case nil
          (plan-polsl-usos--store-users
           (plan-polsl-usos--call "services/users/users"
                                  (plan-polsl-usos--users-params missing)
                                  token))
        (plan-polsl-usos-error nil)))
    (plan-polsl-usos--activities-to-entries activities)))

(defun plan-polsl-usos-fetch-week-async (monday callback errback)
  "Fetch USOS timetable entries for the week at MONDAY asynchronously.
CALLBACK receives the entry list, ERRBACK the error condition. A failed
lecturer name lookup is not an error; ids are shown instead."
  (let ((token (plan-polsl-usos--require-token)))
    (plan-polsl-usos--call-async
     "services/tt/user" (plan-polsl-usos--tt-params monday) token
     (lambda (activities)
       (let ((missing (plan-polsl-usos--missing-lecturers activities))
             (finish (lambda (&rest _)
                       (funcall callback
                                (plan-polsl-usos--activities-to-entries activities)))))
         (if (null missing)
             (funcall finish)
           (plan-polsl-usos--call-async
            "services/users/users" (plan-polsl-usos--users-params missing) token
            (lambda (users)
              (plan-polsl-usos--store-users users)
              (funcall finish))
            finish))))
     errback)))

(provide 'plan-polsl-usos)
;;; plan-polsl-usos.el ends here
