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

(provide 'plan-polsl-usos-test)
;;; plan-polsl-usos-test.el ends here
