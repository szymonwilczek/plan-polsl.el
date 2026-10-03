;;; plan-polsl-usos-test.el --- Tests for plan-polsl-usos -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the USOS API backend. No test talks to the network.

;;; Code:

(require 'ert)
(require 'plan-polsl-usos)

;; never touch the user's real private files
(setq plan-polsl-usos-consumer-file
      (expand-file-name "plan-polsl-test-no-consumer.eld" temporary-file-directory)
      plan-polsl-usos-token-file
      (expand-file-name "plan-polsl-test-no-token.eld" temporary-file-directory))

(ert-deftest plan-polsl-usos-test-url ()
  (let ((plan-polsl-usos-base-url "https://usosapi.polsl.pl/"))
    (should (equal (plan-polsl-usos--url "services/tt/user")
                   "https://usosapi.polsl.pl/services/tt/user")))
  (let ((plan-polsl-usos-base-url "https://usosapi.polsl.pl"))
    (should (equal (plan-polsl-usos--url "services/tt/user")
                   "https://usosapi.polsl.pl/services/tt/user"))))

(ert-deftest plan-polsl-usos-test-consumer-secret-from-auth-source ()
  (let* ((netrc (make-temp-file "plan-polsl-netrc"))
         (auth-sources (list netrc))
         (auth-source-do-cache nil)
         (plan-polsl-usos-base-url "https://usosapi.polsl.pl/")
         (plan-polsl-usos-consumer-key "mykey")
         (plan-polsl-usos-consumer-secret "fallback"))
    (unwind-protect
        (progn
          (with-temp-file netrc
            (insert "machine usosapi.polsl.pl login mykey password s3cret\n"))
          (should (equal (plan-polsl-usos--consumer) '("mykey" . "s3cret"))))
      (delete-file netrc))))

(ert-deftest plan-polsl-usos-test-consumer-secret-fallback ()
  (let ((auth-sources nil)
        (plan-polsl-usos-consumer-key "mykey")
        (plan-polsl-usos-consumer-secret "fallback"))
    (should (equal (plan-polsl-usos--consumer) '("mykey" . "fallback")))))

(ert-deftest plan-polsl-usos-test-consumer-missing ()
  (let ((auth-sources nil)
        (plan-polsl-usos-consumer-key nil)
        (plan-polsl-usos-consumer-secret nil))
    (should-error (plan-polsl-usos--consumer) :type 'user-error))
  (let ((auth-sources nil)
        (plan-polsl-usos-consumer-key "mykey")
        (plan-polsl-usos-consumer-secret nil))
    (should-error (plan-polsl-usos--consumer) :type 'user-error)))

(defmacro plan-polsl-usos-test--with-token-file (&rest body)
  "Run BODY with a fresh temporary `plan-polsl-usos-token-file'."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "plan-polsl-usos" t))
          (plan-polsl-usos-token-file (expand-file-name "token.eld" dir))
          (plan-polsl-usos--token 'unloaded))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(ert-deftest plan-polsl-usos-test-token-roundtrip ()
  (plan-polsl-usos-test--with-token-file
   (let ((plan-polsl-usos-consumer-key "mykey")
         (plan-polsl-usos-consumer-secret "mysecret"))
     (should-not (plan-polsl-usos-logged-in-p))
     (plan-polsl-usos--save-token '(:token "t" :secret "s" :user-id "42"))
     (should (= (file-modes plan-polsl-usos-token-file) #o600))
     (setq plan-polsl-usos--token 'unloaded)
     (should (equal (plan-polsl-usos--load-token)
                    '(:token "t" :secret "s" :user-id "42")))
     (should (plan-polsl-usos-logged-in-p))
     (plan-polsl-usos--delete-token)
     (should-not (file-exists-p plan-polsl-usos-token-file))
     (should-not (plan-polsl-usos-logged-in-p)))))

(ert-deftest plan-polsl-usos-test-token-requires-consumer-key ()
  (plan-polsl-usos-test--with-token-file
   (let ((plan-polsl-usos-consumer-key nil)
         (plan-polsl-usos-consumer-secret nil)
         (auth-sources nil))
     (plan-polsl-usos--save-token '(:token "t" :secret "s"))
     (should-not (plan-polsl-usos-logged-in-p)))))

(ert-deftest plan-polsl-usos-test-token-file-garbage ()
  (plan-polsl-usos-test--with-token-file
   (with-temp-file plan-polsl-usos-token-file
     (insert "(:token"))
   (should-not (plan-polsl-usos--load-token))))

(ert-deftest plan-polsl-usos-test-parse-json ()
  (should (equal (plan-polsl-usos--parse-response
                  200 "[{\"name\": {\"pl\": \"Wykład\"}, \"room_id\": null}]")
                 '(((name (pl . "Wykład")) (room_id))))))

(ert-deftest plan-polsl-usos-test-parse-form ()
  (should (equal (plan-polsl-usos--parse-response
                  200 "oauth_token=abc&oauth_token_secret=x%2By" t)
                 '(("oauth_token" . "abc") ("oauth_token_secret" . "x+y")))))

(ert-deftest plan-polsl-usos-test-parse-errors ()
  (let ((err (should-error (plan-polsl-usos--parse-response
                            401 "{\"message\": \"Invalid access token.\"}")
                           :type 'plan-polsl-usos-unauthorized)))
    (should (string-match-p "Invalid access token" (cadr err))))
  (should-error (plan-polsl-usos--parse-response 400 "{\"message\": \"bad\"}")
                :type 'plan-polsl-usos-error)
  (should-error (plan-polsl-usos--parse-response 500 "<html>")
                :type 'plan-polsl-usos-error))

(ert-deftest plan-polsl-usos-test-split-output ()
  (should (equal (plan-polsl-usos--split-output
                  (encode-coding-string "{\"a\": \"ż\"}\n200" 'utf-8))
                 '(200 . "{\"a\": \"ż\"}")))
  (should (equal (plan-polsl-usos--split-output "{}\n401") '(401 . "{}")))
  ;; curl writes 000 when the connection fails
  (should (equal (plan-polsl-usos--split-output "\n000") '(nil . "")))
  (should (equal (plan-polsl-usos--split-output "") '(nil . ""))))

(ert-deftest plan-polsl-usos-test-error-message ()
  (should (equal (plan-polsl-usos-error-message
                  '(plan-polsl-usos-unauthorized "HTTP 401: Invalid consumer."))
                 "Odmowa dostępu USOS API (HTTP 401: Invalid consumer.)"))
  (should (equal (plan-polsl-usos-error-message '(user-error "Brak"))
                 "Brak")))

(ert-deftest plan-polsl-usos-test-login-flow ()
  (plan-polsl-usos-test--with-token-file
   (let ((plan-polsl-usos-consumer-key "ck")
         (plan-polsl-usos-consumer-secret "cs")
         (calls nil)
         (opened nil)
         (kill-ring nil))
     (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url)))
               ((symbol-function 'read-string) (lambda (&rest _) " 12345 "))
               ((symbol-function 'plan-polsl-usos--call)
                (lambda (method params &optional token _form)
                  (push (list method params token) calls)
                  (pcase method
                    ("services/oauth/request_token"
                     '(("oauth_token" . "rt") ("oauth_token_secret" . "rs")))
                    ("services/oauth/access_token"
                     '(("oauth_token" . "at") ("oauth_token_secret" . "as")))
                    ("services/users/user"
                     '((id . "777") (first_name . "Jan") (last_name . "Kowalski")))))))
       (plan-polsl-usos-login))
     (should (equal opened "https://usosapi.polsl.pl/services/oauth/authorize?oauth_token=rt"))
     (setq calls (nreverse calls))
     (should (equal (cadr (nth 0 calls))
                    '(("oauth_callback" . "oob") ("scopes" . "studies|offline_access"))))
     (should (equal (nth 1 calls)
                    '("services/oauth/access_token" (("oauth_verifier" . "12345"))
                      (:token "rt" :secret "rs"))))
     (should (equal (nth 2 (nth 2 calls)) '(:token "at" :secret "as")))
     (setq plan-polsl-usos--token 'unloaded)
     (should (equal (plan-polsl-usos--load-token)
                    '(:token "at" :secret "as" :user-id "777" :user-name "Jan Kowalski"))))))

(ert-deftest plan-polsl-usos-test-logout ()
  (plan-polsl-usos-test--with-token-file
   (let ((plan-polsl-usos-consumer-key "ck")
         (revoked nil))
     (plan-polsl-usos--save-token '(:token "at" :secret "as"))
     ;; local token is removed even when the server call fails
     (cl-letf (((symbol-function 'plan-polsl-usos--call)
                (lambda (method _params token &rest _)
                  (setq revoked (list method token))
                  (signal 'plan-polsl-usos-error '("HTTP 500: down")))))
       (plan-polsl-usos-logout))
     (should (equal revoked '("services/oauth/revoke_token" (:token "at" :secret "as"))))
     (should-not (plan-polsl-usos-logged-in-p))
     (should-error (plan-polsl-usos-logout) :type 'user-error))))

(ert-deftest plan-polsl-usos-test-lang ()
  (let ((plan-polsl-usos-language "pl"))
    (should (equal (plan-polsl-usos--lang '((pl . "Wykład") (en . "Lecture"))) "Wykład"))
    (should (equal (plan-polsl-usos--lang '((pl . "") (en . "Lecture"))) "Lecture"))
    (should-not (plan-polsl-usos--lang '((pl) (en))))
    (should-not (plan-polsl-usos--lang nil)))
  (let ((plan-polsl-usos-language "en"))
    (should (equal (plan-polsl-usos--lang '((pl . "Wykład") (en . "Lecture"))) "Lecture"))))

(ert-deftest plan-polsl-usos-test-frequency ()
  (should (eq (plan-polsl-usos--frequency-cycle "every_week") 'weekly))
  (should (eq (plan-polsl-usos--frequency-cycle "every_fortnight_odd") 'odd))
  (should (eq (plan-polsl-usos--frequency-cycle "every_fortnight_even") 'even))
  (should-not (plan-polsl-usos--frequency-cycle "once"))
  (should-not (plan-polsl-usos--frequency-cycle nil))
  (should (plan-polsl-usos--biweekly-p "every_fortnight"))
  (should (plan-polsl-usos--biweekly-p "every_fortnight_odd"))
  (should-not (plan-polsl-usos--biweekly-p "every_week"))
  (should-not (plan-polsl-usos--biweekly-p nil)))

(ert-deftest plan-polsl-usos-test-split-datetime ()
  (should (equal (plan-polsl-usos--split-datetime "2026-10-05 08:30:00")
                 '("2026-10-05" . "08:30")))
  (should-error (plan-polsl-usos--split-datetime nil) :type 'plan-polsl-usos-error)
  (should-error (plan-polsl-usos--split-datetime "05.10.2026") :type 'plan-polsl-usos-error))

(ert-deftest plan-polsl-usos-test-iso-day ()
  (should (= (plan-polsl-usos--iso-day "2026-10-05") 1))
  (should (= (plan-polsl-usos--iso-day "2026-10-09") 5))
  (should (= (plan-polsl-usos--iso-day "2026-10-11") 7)))

(defconst plan-polsl-usos-test--activities-json
  "[{\"type\": \"classgroup\",
     \"start_time\": \"2026-10-05 08:30:00\", \"end_time\": \"2026-10-05 10:00:00\",
     \"name\": {\"pl\": \"Analiza matematyczna - Wykład\", \"en\": \"Calculus - Lecture\"},
     \"url\": \"https://usosweb.polsl.pl/x\",
     \"course_name\": {\"pl\": \"Analiza matematyczna\", \"en\": \"Calculus\"},
     \"classtype_name\": {\"pl\": \"Wykład\", \"en\": \"Lecture\"},
     \"lecturer_ids\": [101, 102], \"group_number\": 1,
     \"classgroup_profile_url\": \"https://usosweb.polsl.pl/group\",
     \"building_name\": {\"pl\": \"Wydział AEiI\", \"en\": \"Faculty AEiI\"},
     \"room_number\": \"301\", \"room_id\": 5, \"frequency\": \"every_fortnight_odd\"},
    {\"type\": \"exam\",
     \"start_time\": \"2026-10-10 12:00:00\", \"end_time\": \"2026-10-10 14:00:00\",
     \"name\": {\"pl\": \"Egzamin z fizyki\", \"en\": \"Physics exam\"},
     \"url\": null, \"building_name\": {}, \"room_number\": \"\"}]"
  "USOS services/tt/user response fixture following the API reference.")

(defun plan-polsl-usos-test--activities ()
  "Return the parsed activity fixture."
  (plan-polsl-usos--parse-response 200 plan-polsl-usos-test--activities-json))

(ert-deftest plan-polsl-usos-test-activity-to-entry ()
  (let* ((plan-polsl-usos-language "pl")
         (names (make-hash-table :test #'equal))
         (_ (puthash "101" "dr Jan Kowalski" names))
         (entry (plan-polsl-usos--activity-to-entry
                 (car (plan-polsl-usos-test--activities)) names)))
    (should (= (plist-get entry :day-index) 1))
    (should (equal (plist-get entry :day-name) "Poniedziałek"))
    (should (equal (plist-get entry :date) "2026-10-05"))
    (should (equal (plist-get entry :start-time) "08:30"))
    (should (equal (plist-get entry :end-time) "10:00"))
    (should (equal (plist-get entry :title) "Analiza matematyczna"))
    (should (equal (plist-get entry :type) "Wykład"))
    (should (eq (plist-get entry :cycle) 'odd))
    (should (plist-get entry :biweekly))
    (should (equal (plist-get entry :dates) '("05.10")))
    (should (equal (plist-get entry :groups) '("gr. 1")))
    (should (equal (plist-get entry :teachers) '("dr Jan Kowalski" "102")))
    (should (equal (plist-get entry :rooms) '("301")))
    (should (equal (plist-get entry :building) "Wydział AEiI"))
    (should (equal (plist-get entry :url) "https://usosweb.polsl.pl/group"))))

(ert-deftest plan-polsl-usos-test-exam-to-entry ()
  (let* ((plan-polsl-usos-language "pl")
         (entry (plan-polsl-usos--activity-to-entry
                 (cadr (plan-polsl-usos-test--activities)))))
    (should (= (plist-get entry :day-index) 6))
    (should (equal (plist-get entry :title) "Egzamin z fizyki"))
    (should (equal (plist-get entry :type) "Egzamin"))
    (should-not (plist-get entry :rooms))
    (should-not (plist-get entry :building))
    (should-not (plist-get entry :teachers))
    (should-not (plist-get entry :biweekly))))

(ert-deftest plan-polsl-usos-test-lecturer-names ()
  (let ((plan-polsl-usos--names (make-hash-table :test #'equal)))
    (puthash "102" "known" plan-polsl-usos--names)
    (should (equal (plan-polsl-usos--missing-lecturers (plan-polsl-usos-test--activities))
                   '("101")))
    (should (equal (plan-polsl-usos--users-params '("101" "103"))
                   '(("user_ids" . "101|103") ("fields" . "id|first_name|last_name|titles"))))
    (plan-polsl-usos--store-users
     (plan-polsl-usos--parse-response
      200 "{\"101\": {\"id\": \"101\", \"first_name\": \"Jan\", \"last_name\": \"Kowalski\",
                     \"titles\": {\"before\": \"dr inż.\", \"after\": null}},
           \"103\": null}"))
    (should (equal (gethash "101" plan-polsl-usos--names) "dr inż. Jan Kowalski"))
    (should-not (gethash "103" plan-polsl-usos--names))
    (should-not (plan-polsl-usos--missing-lecturers (plan-polsl-usos-test--activities)))))

(ert-deftest plan-polsl-usos-test-tt-params ()
  (should (equal (butlast (plan-polsl-usos--tt-params (encode-time 0 0 0 5 10 2026)))
                 '(("start" . "2026-10-05") ("days" . "7")))))

(ert-deftest plan-polsl-usos-test-fetch-week ()
  (plan-polsl-usos-test--with-token-file
   (let ((plan-polsl-usos-language "pl")
         (plan-polsl-usos--names (make-hash-table :test #'equal))
         (calls nil))
     (plan-polsl-usos--save-token '(:token "at" :secret "as"))
     (cl-letf (((symbol-function 'plan-polsl-usos--call)
                (lambda (method params token &rest _)
                  (push (list method (cdr (assoc "user_ids" params)) token) calls)
                  (pcase method
                    ("services/tt/user" (reverse (plan-polsl-usos-test--activities)))
                    ("services/users/users"
                     '((\101 (first_name . "Jan") (last_name . "Kowalski"))))))))
       (let ((entries (plan-polsl-usos-fetch-week (encode-time 0 0 0 5 10 2026))))
         ;; sorted chronologically although the API order was reversed
         (should (equal (mapcar (lambda (e) (plist-get e :date)) entries)
                        '("2026-10-05" "2026-10-10")))
         (should (equal (plist-get (car entries) :teachers) '("Jan Kowalski" "102")))))
     (should (equal (nreverse calls)
                    '(("services/tt/user" nil (:token "at" :secret "as"))
                      ("services/users/users" "101|102" (:token "at" :secret "as"))))))))

(ert-deftest plan-polsl-usos-test-fetch-week-async-survives-name-failure ()
  (plan-polsl-usos-test--with-token-file
   (let ((plan-polsl-usos-language "pl")
         (plan-polsl-usos--names (make-hash-table :test #'equal))
         (result nil))
     (plan-polsl-usos--save-token '(:token "at" :secret "as"))
     (cl-letf (((symbol-function 'plan-polsl-usos--call-async)
                (lambda (method _params _token callback errback)
                  (pcase method
                    ("services/tt/user" (funcall callback (plan-polsl-usos-test--activities)))
                    ("services/users/users"
                     (funcall errback '(plan-polsl-usos-error "HTTP 500: down")))))))
       (plan-polsl-usos-fetch-week-async (current-time)
                                         (lambda (entries) (setq result entries))
                                         (lambda (err) (setq result err))))
     (should (= (length result) 2))
     (should (equal (plist-get (car result) :teachers) '("101" "102"))))))

(ert-deftest plan-polsl-usos-test-fetch-week-requires-login ()
  (plan-polsl-usos-test--with-token-file
   (should-error (plan-polsl-usos-fetch-week (current-time)) :type 'user-error)))

(ert-deftest plan-polsl-usos-test-consumer-file-roundtrip ()
  (let* ((dir (make-temp-file "plan-polsl-usos" t))
         (plan-polsl-usos-consumer-file (expand-file-name "sub/consumer.eld" dir)))
    (unwind-protect
        (progn
          (should-not (plan-polsl-usos--load-consumer))
          (plan-polsl-usos--save-consumer "ck" "cs")
          (should (= (file-modes plan-polsl-usos-consumer-file) #o600))
          (should (= (file-modes (file-name-directory plan-polsl-usos-consumer-file)) #o700))
          (should (equal (plan-polsl-usos--load-consumer) '("ck" . "cs")))
          (plan-polsl-usos--save-consumer "ck" "")
          (should-not (plan-polsl-usos--load-consumer)))
      (delete-directory dir t))))

(ert-deftest plan-polsl-usos-test-consumer-file-wins ()
  (let* ((dir (make-temp-file "plan-polsl-usos" t))
         (plan-polsl-usos-consumer-file (expand-file-name "consumer.eld" dir))
         (auth-sources nil)
         (plan-polsl-usos-consumer-key "from-config")
         (plan-polsl-usos-consumer-secret "config-secret"))
    (unwind-protect
        (progn
          (should (equal (plan-polsl-usos--consumer) '("from-config" . "config-secret")))
          (plan-polsl-usos--save-consumer "from-file" "file-secret")
          (should (equal (plan-polsl-usos--consumer) '("from-file" . "file-secret")))
          (let ((plan-polsl-usos-consumer-key nil)
                (plan-polsl-usos-consumer-secret nil))
            (should (equal (plan-polsl-usos--consumer) '("from-file" . "file-secret")))))
      (delete-directory dir t))))

(ert-deftest plan-polsl-usos-test-setup ()
  (plan-polsl-usos-test--with-token-file
   (let* ((plan-polsl-usos-consumer-file
           (expand-file-name "consumer.eld" (file-name-directory plan-polsl-usos-token-file)))
          (answers nil))
     (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (pop answers)))
               ((symbol-function 'read-passwd) (lambda (&rest _) (pop answers))))
       (setq answers '(" ck1 " "cs1"))
       (plan-polsl-usos-setup)
       (should (equal (plan-polsl-usos--load-consumer) '("ck1" . "cs1")))
       (plan-polsl-usos--save-token '(:token "t" :secret "s"))
       ;; same key, new secret: login kept
       (setq answers '("ck1" "cs2"))
       (plan-polsl-usos-setup)
       (should (plan-polsl-usos-logged-in-p))
       ;; different key: old token is useless and gets dropped
       (setq answers '("ck2" "cs3"))
       (plan-polsl-usos-setup)
       (should (equal (plan-polsl-usos--load-consumer) '("ck2" . "cs3")))
       (should-not (plan-polsl-usos-logged-in-p))
       (setq answers '("ck3" ""))
       (should-error (plan-polsl-usos-setup) :type 'user-error)
       (should (equal (plan-polsl-usos--load-consumer) '("ck2" . "cs3")))))))

(ert-deftest plan-polsl-usos-test-login-runs-setup ()
  (plan-polsl-usos-test--with-token-file
   (let ((plan-polsl-usos-consumer-file
          (expand-file-name "consumer.eld" (file-name-directory plan-polsl-usos-token-file)))
         (plan-polsl-usos-consumer-key nil)
         (plan-polsl-usos-consumer-secret nil)
         (auth-sources nil)
         (setup-called nil))
     (cl-letf (((symbol-function 'plan-polsl-usos-setup)
                (lambda ()
                  (setq setup-called t)
                  (plan-polsl-usos--save-consumer "ck" "cs")))
               ((symbol-function 'plan-polsl-usos--call)
                (lambda (&rest _) (signal 'plan-polsl-usos-error '("stop")))))
       (should-error (plan-polsl-usos-login) :type 'plan-polsl-usos-error))
     (should setup-called))))

(ert-deftest plan-polsl-usos-test-logout-forget-consumer ()
  (plan-polsl-usos-test--with-token-file
   (let ((plan-polsl-usos-consumer-file
          (expand-file-name "consumer.eld" (file-name-directory plan-polsl-usos-token-file)))
         (revoked nil))
     (plan-polsl-usos--save-consumer "ck" "cs")
     (plan-polsl-usos--save-token '(:token "at" :secret "as"))
     (cl-letf (((symbol-function 'plan-polsl-usos--call)
                (lambda (&rest _) (setq revoked t) nil)))
       (plan-polsl-usos-logout t))
     (should revoked)
     (should-not (file-exists-p plan-polsl-usos-consumer-file))
     (should-not (file-exists-p plan-polsl-usos-token-file))
     ;; forgetting the consumer also works when already logged out
     (plan-polsl-usos--save-consumer "ck" "cs")
     (plan-polsl-usos-logout t)
     (should-not (file-exists-p plan-polsl-usos-consumer-file)))))

(provide 'plan-polsl-usos-test)
;;; plan-polsl-usos-test.el ends here
