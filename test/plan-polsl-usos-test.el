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

(provide 'plan-polsl-usos-test)
;;; plan-polsl-usos-test.el ends here
