;;; plan-polsl-usos-test.el --- Tests for plan-polsl-usos -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the USOS API backend. No test talks to the network.

;;; Code:

(require 'ert)
(require 'plan-polsl-usos)

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
   (let ((plan-polsl-usos-consumer-key "mykey"))
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
   (let ((plan-polsl-usos-consumer-key nil))
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

(provide 'plan-polsl-usos-test)
;;; plan-polsl-usos-test.el ends here
