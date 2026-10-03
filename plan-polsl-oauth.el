;;; plan-polsl-oauth.el --- OAuth 1.0a request signing -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, oauth

;;; Commentary:
;; Minimal OAuth 1.0a (RFC 5849) client primitives used by the USOS API
;; integration: RFC 3986 percent-encoding, HMAC-SHA1, signature base string
;; construction and signed request URLs.
;; No consumer keys or secrets are stored here; every credential is
;; supplied by the caller.

;;; Code:

(require 'url-util)

(defconst plan-polsl-oauth--unreserved-chars
  (append "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~" nil)
  "Characters left unescaped by RFC 3986 percent-encoding.")

(defun plan-polsl-oauth-encode (value)
  "Percent-encode VALUE as required by RFC 5849 section 3.6.
VALUE may be a string or a number; strings are encoded as UTF-8 first."
  (url-hexify-string (format "%s" value) plan-polsl-oauth--unreserved-chars))

(defun plan-polsl-oauth--to-bytes (value)
  "Return VALUE as a unibyte UTF-8 string."
  (if (multibyte-string-p value)
      (encode-coding-string value 'utf-8 t)
    value))

(defun plan-polsl-oauth-hmac-sha1 (key message)
  "Return raw HMAC-SHA1 digest (RFC 2104) of MESSAGE using KEY.
Both KEY and MESSAGE are encoded as UTF-8 when multibyte."
  (let* ((block-size 64)
         (key (plan-polsl-oauth--to-bytes key))
         (message (plan-polsl-oauth--to-bytes message))
         (key (if (> (length key) block-size)
                  (secure-hash 'sha1 key nil nil t)
                key))
         (key (concat key (make-string (- block-size (length key)) 0)))
         (ipad (apply #'unibyte-string (mapcar (lambda (c) (logxor c #x36)) key)))
         (opad (apply #'unibyte-string (mapcar (lambda (c) (logxor c #x5c)) key))))
    (secure-hash 'sha1
                 (concat opad (secure-hash 'sha1 (concat ipad message) nil nil t))
                 nil nil t)))

(provide 'plan-polsl-oauth)
;;; plan-polsl-oauth.el ends here
