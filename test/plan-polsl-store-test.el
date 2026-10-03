;;; plan-polsl-store-test.el --- Tests for plan-polsl-store -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for private data file helpers.

;;; Code:

(require 'ert)
(require 'plan-polsl-store)

(ert-deftest plan-polsl-store-test-data-file ()
  (should (equal (let ((process-environment (cons "XDG_DATA_HOME=/xdg" process-environment)))
                   (plan-polsl-store-data-file "usos-token.eld"))
                 "/xdg/plan-polsl/usos-token.eld"))
  (should (equal (let ((process-environment (cons "XDG_DATA_HOME" process-environment)))
                   (plan-polsl-store-data-file "x.eld"))
                 (expand-file-name "~/.local/share/plan-polsl/x.eld"))))

(ert-deftest plan-polsl-store-test-roundtrip ()
  (let* ((dir (make-temp-file "plan-polsl-store" t))
         (file (expand-file-name "sub/data.eld" dir)))
    (unwind-protect
        (progn
          (should-not (plan-polsl-store-read file))
          (plan-polsl-store-write file "test" '(:a "b"))
          (should (equal (plan-polsl-store-read file) '(:a "b")))
          (should (= (file-modes file) #o600))
          (should (= (file-modes (file-name-directory file)) #o700))
          (with-temp-file file (insert "(:a"))
          (should-not (plan-polsl-store-read file)))
      (delete-directory dir t))))

(provide 'plan-polsl-store-test)
;;; plan-polsl-store-test.el ends here
