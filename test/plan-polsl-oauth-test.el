;;; plan-polsl-oauth-test.el --- Tests for plan-polsl-oauth -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for OAuth 1.0a signing primitives, using vectors from
;; RFC 2202 (HMAC-SHA1) and RFC 5849 (OAuth 1.0).

;;; Code:

(require 'ert)
(require 'hex-util)
(require 'plan-polsl-oauth)

(ert-deftest plan-polsl-oauth-test-encode ()
  (should (equal (plan-polsl-oauth-encode "abc-._~XYZ09") "abc-._~XYZ09"))
  (should (equal (plan-polsl-oauth-encode "a b&c=d/e+f") "a%20b%26c%3Dd%2Fe%2Bf"))
  (should (equal (plan-polsl-oauth-encode "żółw") "%C5%BC%C3%B3%C5%82w"))
  (should (equal (plan-polsl-oauth-encode 1191242096) "1191242096")))

(ert-deftest plan-polsl-oauth-test-hmac-sha1 ()
  ;; RFC 2202 test case 2
  (should (equal (encode-hex-string
                  (plan-polsl-oauth-hmac-sha1 "Jefe" "what do ya want for nothing?"))
                 "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79"))
  ;; RFC 2202 test case 6: key longer than block size is hashed first
  (should (equal (encode-hex-string
                  (plan-polsl-oauth-hmac-sha1
                   (apply #'unibyte-string (make-list 80 #xaa))
                   "Test Using Larger Than Block-Size Key - Hash Key First"))
                 "aa4ae5e15272d00e95705637ce8a3b55ed402112")))

(defconst plan-polsl-oauth-test--rfc5849-params
  '(("file" . "vacation.jpg")
    ("size" . "original")
    ("oauth_consumer_key" . "dpf43f3p2l4k3l03")
    ("oauth_token" . "nnch734d00sl2jdk")
    ("oauth_signature_method" . "HMAC-SHA1")
    ("oauth_timestamp" . "1191242096")
    ("oauth_nonce" . "kllo9940pd9333jh")
    ("oauth_version" . "1.0"))
  "Request parameters of the RFC 5849 section 1.2 example.")

(ert-deftest plan-polsl-oauth-test-normalize-params ()
  (should (equal (plan-polsl-oauth-normalize-params
                  '(("b" . "2") ("a" . "x y") ("a" . "1")))
                 "a=1&a=x%20y&b=2")))

(ert-deftest plan-polsl-oauth-test-base-string ()
  (should (equal (plan-polsl-oauth-base-string
                  "get" "http://photos.example.net/photos"
                  plan-polsl-oauth-test--rfc5849-params)
                 (concat "GET&http%3A%2F%2Fphotos.example.net%2Fphotos&"
                         "file%3Dvacation.jpg%26oauth_consumer_key%3Ddpf43f3p2l4k3l03"
                         "%26oauth_nonce%3Dkllo9940pd9333jh"
                         "%26oauth_signature_method%3DHMAC-SHA1"
                         "%26oauth_timestamp%3D1191242096"
                         "%26oauth_token%3Dnnch734d00sl2jdk"
                         "%26oauth_version%3D1.0%26size%3Doriginal"))))

(ert-deftest plan-polsl-oauth-test-sign ()
  ;; RFC 5849 section 1.2 example signature
  (let ((signed (plan-polsl-oauth-sign
                 "GET" "http://photos.example.net/photos"
                 '(("file" . "vacation.jpg") ("size" . "original"))
                 "dpf43f3p2l4k3l03" "kd94hf93k423kf44"
                 "nnch734d00sl2jdk" "pfkkdhi9sl3r4s00"
                 "kllo9940pd9333jh" 1191242096)))
    (should (equal (cdr (assoc "oauth_signature" signed))
                   "tR3+Ty81lMeYAr/Fid0kMTYa/WM="))
    (should (equal (cdr (assoc "file" signed)) "vacation.jpg"))
    (should (equal (cdr (assoc "oauth_token" signed)) "nnch734d00sl2jdk"))))

(ert-deftest plan-polsl-oauth-test-sign-without-token ()
  (let ((signed (plan-polsl-oauth-sign "GET" "https://example.com/x" nil "key" "secret")))
    (should-not (assoc "oauth_token" signed))
    (should (= (length (cdr (assoc "oauth_nonce" signed))) 32))
    (should (assoc "oauth_signature" signed))))

(provide 'plan-polsl-oauth-test)
;;; plan-polsl-oauth-test.el ends here
