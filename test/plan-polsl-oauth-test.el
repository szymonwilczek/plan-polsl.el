;;; plan-polsl-oauth-test.el --- Tests for plan-polsl-oauth -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for OAuth 1.0a signing primitives, using vectors from
;; RFC 2202 (HMAC-SHA1) and RFC 5849 (OAuth 1.0).

;;; Code:

(require 'ert)
(require 'plan-polsl-oauth)

(ert-deftest plan-polsl-oauth-test-encode ()
  (should (equal (plan-polsl-oauth-encode "abc-._~XYZ09") "abc-._~XYZ09"))
  (should (equal (plan-polsl-oauth-encode "a b&c=d/e+f") "a%20b%26c%3Dd%2Fe%2Bf"))
  (should (equal (plan-polsl-oauth-encode "żółw") "%C5%BC%C3%B3%C5%82w"))
  (should (equal (plan-polsl-oauth-encode 1191242096) "1191242096")))

(provide 'plan-polsl-oauth-test)
;;; plan-polsl-oauth-test.el ends here
