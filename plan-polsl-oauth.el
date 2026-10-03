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

(provide 'plan-polsl-oauth)
;;; plan-polsl-oauth.el ends here
