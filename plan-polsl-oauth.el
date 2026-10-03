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

(defun plan-polsl-oauth-normalize-params (params)
  "Return PARAMS alist normalized per RFC 5849 section 3.4.1.3.2.
Names and values are percent-encoded, sorted by name then value, and
joined as name=value pairs separated by ampersands."
  (let ((pairs (mapcar (lambda (p)
                         (cons (plan-polsl-oauth-encode (car p))
                               (plan-polsl-oauth-encode (cdr p))))
                       params)))
    (mapconcat (lambda (p) (concat (car p) "=" (cdr p)))
               (sort pairs (lambda (a b)
                             (if (string= (car a) (car b))
                                 (string< (cdr a) (cdr b))
                               (string< (car a) (car b)))))
               "&")))

(defun plan-polsl-oauth-base-string (method url params)
  "Return the signature base string for METHOD, URL and PARAMS.
URL must not contain a query string; PARAMS is an alist holding both
request and oauth_* parameters (RFC 5849 section 3.4.1)."
  (concat (upcase method)
          "&" (plan-polsl-oauth-encode url)
          "&" (plan-polsl-oauth-encode (plan-polsl-oauth-normalize-params params))))

(provide 'plan-polsl-oauth)
;;; plan-polsl-oauth.el ends here
