;;; plan-polsl-http.el --- HTTP client for plan.polsl.pl -*- lexical-binding: t; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, http

;;; Commentary:
;; HTTP retrieval and charset decoding module for plan.polsl.pl schedules.
;; Uses curl with Connection: close and legacy TLS support for fast responses.

;;; Code:

(require 'url)
(require 'url-http)

(defcustom plan-polsl-http-timeout 15
  "Maximum seconds to wait for a single HTTP request to plan.polsl.pl."
  :type 'integer
  :group 'plan-polsl)

(defun plan-polsl-http-build-url (group-id &optional type win-w win-h)
  "Build full plan.polsl.pl request URL for GROUP-ID and TYPE.
TYPE defaults to 0 (groups). WIN-W and WIN-H default to configured geometry."
  (let ((type-val (or type 0))
        (w (or win-w (bound-and-true-p plan-polsl-window-width) 1920))
        (h (or win-h (bound-and-true-p plan-polsl-window-height) 1080))
        (base (or (bound-and-true-p plan-polsl-base-url) "https://plan.polsl.pl/")))
    (format "%splan.php?type=%s&id=%s&winW=%d&winH=%d"
            (if (string-suffix-p "/" base) base (concat base "/"))
            type-val
            (url-hexify-string (format "%s" group-id))
            w
            h)))

(defun plan-polsl-http-fetch (url)
  "Fetch HTML content from URL synchronously, decoding Latin-2 / UTF-8 properly.
Uses `curl` with Connection: close to negotiate legacy TLS and avoid keep-alive delays."
  (if (executable-find "curl")
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (let ((timeout (or (bound-and-true-p plan-polsl-http-timeout) 15)))
          (call-process "curl" nil t nil
                        "-s" "-k" "--http1.1"
                        "-H" "Connection: close"
                        "--connect-timeout" (number-to-string (min timeout 5))
                        "--max-time" (number-to-string timeout)
                        "--ciphers" "DEFAULT@SECLEVEL=1"
                        "-A" "Emacs plan-polsl.el/0.1.0 (GNU Emacs)"
                        url)
          (if (> (buffer-size) 10)
              (let* ((raw-bytes (buffer-string))
                     (decoded (or (condition-case nil
                                      (decode-coding-string raw-bytes 'iso-8859-2)
                                    (error nil))
                                  (condition-case nil
                                      (decode-coding-string raw-bytes 'utf-8)
                                    (error raw-bytes)))))
                decoded)
            (error "plan-polsl: Failed to fetch content from %s" url))))

    ;; fallback to built-in url.el
    (let* ((url-request-method "GET")
           (url-request-extra-headers
            '(("User-Agent" . "Emacs plan-polsl.el/0.1.0 (GNU Emacs)")
              ("Connection" . "close")
              ("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
              ("Accept-Language" . "pl,en-US;q=0.7,en;q=0.3")))
           (buffer (url-retrieve-synchronously url t t 15)))
      (unless buffer
        (error "plan-polsl: Failed to connect to %s" url))
      (with-current-buffer buffer
        (unwind-protect
            (progn
              (goto-char (point-min))
              (if (re-search-forward "\r?\n\r?\n" nil t)
                  (let* ((raw-bytes (buffer-substring-no-properties (point) (point-max)))
                         (decoded (or (condition-case nil
                                          (decode-coding-string raw-bytes 'iso-8859-2)
                                        (error nil))
                                      (condition-case nil
                                          (decode-coding-string raw-bytes 'utf-8)
                                        (error raw-bytes)))))
                    decoded)
                (error "plan-polsl: Malformed HTTP response from server")))
          (kill-buffer buffer))))))

(defun plan-polsl-http-fetch-schedule (group-id &optional type)
  "Fetch timetable HTML for GROUP-ID (TYPE defaults to 0 for student group)."
  (let ((url (plan-polsl-http-build-url group-id type)))
    (plan-polsl-http-fetch url)))

(provide 'plan-polsl-http)
;;; plan-polsl-http.el ends here
