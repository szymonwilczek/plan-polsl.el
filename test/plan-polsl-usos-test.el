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

(provide 'plan-polsl-usos-test)
;;; plan-polsl-usos-test.el ends here
