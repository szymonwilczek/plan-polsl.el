;;; plan-polsl-http.el --- HTTP client for plan.polsl.pl -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, http

;;; Commentary:
;; HTTP client for plan.polsl.pl endpoints.
;; Handles legacy TLS parameters, HTTP keep-alive timeouts via Connection:
;; close, non-blocking asynchronous requests (`make-process`), parallel batch
;; crawling (`curl -Z`), and Latin-2 (ISO-8859-2) / UTF-8 charset decoding.

;;; Code:

(require 'url)
(require 'url-http)

(defcustom plan-polsl-http-timeout 15
  "Maximum seconds to wait for a single HTTP request to plan.polsl.pl."
  :type 'integer
  :group 'plan-polsl)

(defconst plan-polsl-http--curl-common-args
  '("-s" "-k" "--http1.1" "--compressed"
    "-H" "Connection: close"
    "--ciphers" "DEFAULT@SECLEVEL=1"
    "-A" "Emacs plan-polsl.el (GNU Emacs)")
  "Shared curl arguments for fast and reliable PolSL server communication.")

(defun plan-polsl-http--decode-response (raw-bytes)
  "Decode RAW-BYTES attempting ISO-8859-2 (Polish Latin-2) then UTF-8."
  (or (condition-case nil
          (decode-coding-string raw-bytes 'iso-8859-2)
        (error nil))
      (condition-case nil
          (decode-coding-string raw-bytes 'utf-8)
        (error raw-bytes))))

(defun plan-polsl-http-build-url (id &optional type win-w win-h)
  "Build full plan.polsl.pl request URL for schedule ID and TYPE.
TYPE defaults to 0 (groups). WIN-W and WIN-H default to configured geometry."
  (let ((type-val (or type 0))
        (w (or win-w (bound-and-true-p plan-polsl-window-width) 1920))
        (h (or win-h (bound-and-true-p plan-polsl-window-height) 1080))
        (base (or (bound-and-true-p plan-polsl-base-url) "https://plan.polsl.pl/")))
    (format "%splan.php?type=%s&id=%s&winW=%d&winH=%d"
            (if (string-suffix-p "/" base) base (concat base "/"))
            type-val
            (url-hexify-string (format "%s" id))
            w
            h)))

(defun plan-polsl-http-fetch (url &optional timeout)
  "Fetch HTML content from URL synchronously.
Uses `curl` with legacy TLS negotiation and Connection: close for optimal speed."
  (let ((to (or timeout (bound-and-true-p plan-polsl-http-timeout) 15)))
    (if (executable-find "curl")
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (apply #'call-process "curl" nil t nil
                 (append plan-polsl-http--curl-common-args
                         (list "--connect-timeout" (number-to-string (min to 5))
                               "--max-time" (number-to-string to)
                               url)))
          (if (> (buffer-size) 10)
              (plan-polsl-http--decode-response (buffer-string))
            (error "plan-polsl: Failed to fetch content from %s" url)))

      ;; fallback to built-in url.el when curl is unavailable
      (let* ((url-request-method "GET")
             (url-request-extra-headers
              '(("User-Agent" . "Emacs plan-polsl.el (GNU Emacs)")
                ("Connection" . "close")
                ("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
                ("Accept-Language" . "pl,en-US;q=0.7,en;q=0.3")))
             (buf (url-retrieve-synchronously url t t to)))
        (unless buf
          (error "plan-polsl: Failed to connect to %s" url))
        (with-current-buffer buf
          (unwind-protect
              (progn
                (goto-char (point-min))
                (if (re-search-forward "\r?\n\r?\n" nil t)
                    (plan-polsl-http--decode-response
                     (buffer-substring-no-properties (point) (point-max)))
                  (error "plan-polsl: Malformed HTTP response from server")))
            (kill-buffer buf)))))))

(defun plan-polsl-http-fetch-async (url callback &optional error-callback timeout)
  "Fetch HTML content from URL asynchronously without blocking Emacs UI.
Runs CALLBACK with the decoded HTML string upon success, or ERROR-CALLBACK upon error."
  (if (not (executable-find "curl"))
      ;; fallback to synchronous fetch in separate callback
      (condition-case err
          (funcall callback (plan-polsl-http-fetch url timeout))
        (error (if error-callback
                   (funcall error-callback (error-message-string err))
                 (message "plan-polsl error: %s" (error-message-string err)))))
    (let* ((to (or timeout (bound-and-true-p plan-polsl-http-timeout) 15))
           (out-buf (generate-new-buffer " *plan-polsl-async*"))
           (args (append (list "curl")
                         plan-polsl-http--curl-common-args
                         (list "--connect-timeout" (number-to-string (min to 5))
                               "--max-time" (number-to-string to)
                               url))))
      (make-process
       :name "plan-polsl-http-async"
       :buffer out-buf
       :command args
       :noquery t
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (let* ((pbuf (process-buffer proc))
                  (exit-code (process-exit-status proc)))
             (unwind-protect
                 (if (and (buffer-live-p pbuf) (> (buffer-size pbuf) 100))
                     (let* ((raw (with-current-buffer pbuf (buffer-string)))
                            (html (plan-polsl-http--decode-response raw)))
                       (funcall callback html))
                   (let ((err-msg (format "Błąd pobierania danych z serwera (kod %d)" exit-code)))
                     (if error-callback
                         (funcall error-callback err-msg)
                       (message "plan-polsl: %s" err-msg))))
               (when (buffer-live-p pbuf)
                 (kill-buffer pbuf))))))))))

(defun plan-polsl-http-fetch-parallel (urls &optional max-jobs timeout)
  "Fetch multiple URLS in parallel using curl -Z.
Returns the concatenated decoded response string."
  (unless (executable-find "curl")
    (error "plan-polsl: Parallel fetching requires curl executable"))
  (let ((jobs (or max-jobs 20))
        (to (or timeout (bound-and-true-p plan-polsl-http-timeout) 15)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (apply #'call-process "curl" nil t nil
             (append (list "-Z" "--parallel-max" (number-to-string jobs))
                     plan-polsl-http--curl-common-args
                     (list "--connect-timeout" (number-to-string (min to 5))
                           "--max-time" (number-to-string to))
                     urls))
      (plan-polsl-http--decode-response (buffer-string)))))

(defun plan-polsl-http-fetch-schedule (id &optional type timeout)
  "Fetch HTML timetable for schedule ID and TYPE synchronously."
  (plan-polsl-http-fetch (plan-polsl-http-build-url id type) timeout))

(defun plan-polsl-http-fetch-schedule-async (id type callback &optional error-callback timeout)
  "Fetch HTML timetable for schedule ID and TYPE asynchronously without UI freeze."
  (plan-polsl-http-fetch-async (plan-polsl-http-build-url id type)
                               callback error-callback timeout))

(provide 'plan-polsl-http)
;;; plan-polsl-http.el ends here
