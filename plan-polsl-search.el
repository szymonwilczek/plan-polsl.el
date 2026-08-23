;;; plan-polsl-search.el --- Interactive schedule and teacher search -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, search

;;; Commentary:
;; Interactive search for Politechnika Śląska timetables:
;; - Teachers: live fetch & fuzzy search across all university faculty
;; - Student groups / Rooms: step-by-step interactive tree navigation

;;; Code:

(require 'cl-lib)
(require 'plan-polsl-http)

(defvar plan-polsl-search--teachers-cache nil
  "In-memory cached alist of (TEACHER-NAME . TEACHER-ID).")

(defcustom plan-polsl-search-cache-file
  (expand-file-name "plan-polsl-teachers.cache"
                    (if (fboundp 'locate-user-emacs-file)
                        (locate-user-emacs-file "plan-polsl/")
                      user-emacs-directory))
  "File path used to persist the dynamically fetched teachers directory."
  :type 'file
  :group 'plan-polsl)

(defconst plan-polsl-search--type-alist
  '(("Grupa studencka" . 1)
    ("Nauczyciel" . :teacher)
    ("Sala / zasób" . 3))
  "Mapping from category names to search handler.")

(defconst plan-polsl-search--leaf-type-alist
  '(("0"  . 0)
    ("2"  . 0)
    ("10" . 10)
    ("12" . 10)
    ("20" . 20))
  "Mapping from plan.php?type= parameter to `plan-polsl-type' values.")

(defun plan-polsl-search--fetch-feed (url)
  "Fetch URL content with proper encoding."
  (plan-polsl-http-fetch url))

(defun plan-polsl-search--parse-root (html)
  "Parse root-level branch() calls from left_menu.php HTML.
Returns list of (NAME . ID) pairs."
  (let ((entries nil)
        (pos 0))
    (while (string-match
            "branch(\\([0-9]+\\),\\([0-9]+\\),\\([0-9]+\\),'\\([^']+\\)')"
            html pos)
      (let ((id (match-string 2 html))
            (name (match-string 4 html))
            (match-end-pos (match-end 0)))
        (push (cons name id) entries)
        (setq pos match-end-pos)))
    (nreverse entries)))

(defun plan-polsl-search--parse-feed (html)
  "Parse left_menu_feed.php HTML response.
Returns a list of entries, each one of:
  (:branch ID NAME) - a subtree that can be expanded further
  (:leaf TYPE ID NAME) - a final plan link (type=0/10/20 etc.)"
  (let ((branches nil)
        (leaves nil)
        (branch-ids (make-hash-table :test 'equal))
        (pos 0))
    ;; extract branches from get_left_tree_branch('ID', ...)
    (while (string-match
            "get_left_tree_branch([ \t\n]*'\\([0-9]+\\)'"
            html pos)
      (let* ((bid (match-string 1 html))
             (match-end-pos (match-end 0))
             ;; find the name
             (chunk (substring html match-end-pos (min (length html) (+ match-end-pos 500))))
             (name (cond
                    ;; name in <a href="plan.php...">NAME</a>
                    ((string-match ">\\([^<]+\\)</a>" chunk)
                     (string-trim (match-string 1 chunk)))

                    ;; plain text
                    ((string-match ">[ \t]*\\([^<]+[^ <\t]\\)[ \t]*<div" chunk)
                     (string-trim (match-string 1 chunk)))
                    (t (format "ID:%s" bid)))))
        (push (list :branch bid name) branches)
        (puthash bid t branch-ids)
        (setq pos match-end-pos)))

    ;; extract leaf plan.php links
    (setq pos 0)
    (while (string-match
            "href=\"plan\\.php\\?type=\\([0-9]+\\)&amp;id=\\([0-9]+\\)\"[^>]*>\\([^<]+\\)</a>"
            html pos)
      (let ((ltype (match-string 1 html))
            (lid (match-string 2 html))
            (lname (string-trim (match-string 3 html)))
            (match-end-pos (match-end 0)))
        (push (list :leaf ltype lid lname) leaves)
        (setq pos match-end-pos)))
    ;; Return branches first, then standalone leaves
    (let ((result nil))
      (dolist (b (nreverse branches))
        (push b result))
      (dolist (l (nreverse leaves))
        (let ((lid (nth 2 l))
              (ltype (nth 1 l)))
          (when (and (member ltype '("0" "10" "20"))
                     (not (gethash lid branch-ids)))
            (push l result))))
      (nreverse result))))

(defun plan-polsl-search--format-choice (entry)
  "Format ENTRY for display in `completing-read'."
  (pcase (car entry)
    (:branch (format "%s" (nth 2 entry)))
    (:leaf   (format "%s" (nth 3 entry)))))

(defun plan-polsl-search--fetch-root-nodes (menu-type)
  "Fetch root-level nodes for MENU-TYPE (1=groups, 3=rooms).
Returns list of (:branch ID NAME) entries."
  (let* ((base (or (bound-and-true-p plan-polsl-base-url) "https://plan.polsl.pl/"))
         (url (format "%sleft_menu.php?type=%d"
                      (if (string-suffix-p "/" base) base (concat base "/"))
                      menu-type))
         (html (plan-polsl-search--fetch-feed url))
         (pairs (plan-polsl-search--parse-root html)))
    (mapcar (lambda (pair) (list :branch (cdr pair) (car pair))) pairs)))

(defun plan-polsl-search--fetch-children (menu-type branch-id)
  "Fetch child nodes for BRANCH-ID under MENU-TYPE.
Returns mixed list of (:branch ...) and (:leaf ...) entries."
  (let* ((base (or (bound-and-true-p plan-polsl-base-url) "https://plan.polsl.pl/"))
         (url (format "%sleft_menu_feed.php?type=%d&branch=%s&link=0&bOne=1"
                      (if (string-suffix-p "/" base) base (concat base "/"))
                      menu-type
                      branch-id))
         (html (plan-polsl-search--fetch-feed url)))
    (plan-polsl-search--parse-feed html)))

(defun plan-polsl-search--fetch-all-teachers ()
  "Dynamically fetch and index all teachers from plan.polsl.pl in parallel."
  (let* ((base (or (bound-and-true-p plan-polsl-base-url) "https://plan.polsl.pl/"))
         (url-faculties (format "%sleft_menu.php?type=2"
                                (if (string-suffix-p "/" base) base (concat base "/"))))
         (raw-faculties (plan-polsl-search--fetch-feed url-faculties))
         (faculties nil)
         (pos 0))
    (while (string-match "branch(2,\\([0-9]+\\),[0-9]+,'\\([^']+\\)')" raw-faculties pos)
      (let ((fid (match-string 1 raw-faculties))
            (end (match-end 0)))
        (push fid faculties)
        (setq pos end)))

    ;; fetch all faculty feeds in parallel
    (let* ((f-urls (mapcar (lambda (fid)
                             (format "%sleft_menu_feed.php?type=2&branch=%s&link=0&bOne=1"
                                     (if (string-suffix-p "/" base) base (concat base "/"))
                                     fid))
                           faculties))
           (raw-katedry (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (apply #'call-process "curl" nil t nil
                                 "-Z" "--parallel-max" "20" "-s" "-k" "--http1.1"
                                 "-H" "Connection: close"
                                 "--connect-timeout" "5" "--max-time" "10"
                                 "--ciphers" "DEFAULT@SECLEVEL=1"
                                 "-A" "Emacs plan-polsl.el (GNU Emacs)"
                                 f-urls)
                          (decode-coding-string (buffer-string) 'utf-8)))
           (k-branches nil)
           (teachers-dict (make-hash-table :test 'equal))
           (pos 0))
      (while (string-match "get_left_tree_branch([ \t\n]*'\\([0-9]+\\)'" raw-katedry pos)
        (let ((kid (match-string 1 raw-katedry))
              (end (match-end 0)))
          (push kid k-branches)
          (setq pos end)))

      ;; direct teachers in faculty feeds
      (setq pos 0)
      (while (string-match "href=\"plan\\.php\\?type=10&amp;id=\\([0-9]+\\)\"[^>]*>\\([^<]+\\)</a>" raw-katedry pos)
        (let ((tid (match-string 1 raw-katedry))
              (raw-name (match-string 2 raw-katedry))
              (end (match-end 0)))
          (let ((tname (string-trim raw-name)))
            (when (> (length tname) 1)
              (puthash tname tid teachers-dict)))
          (setq pos end)))
      (setq k-branches (delete-dups k-branches))

      ;; fetch all katedry feeds in parallel
      (let* ((k-urls (mapcar (lambda (kid)
                               (format "%sleft_menu_feed.php?type=2&branch=%s&link=0&bOne=1"
                                       (if (string-suffix-p "/" base) base (concat base "/"))
                                       kid))
                             k-branches))
             (raw-teachers (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (apply #'call-process "curl" nil t nil
                                    "-Z" "--parallel-max" "30" "-s" "-k" "--http1.1"
                                    "-H" "Connection: close"
                                    "--connect-timeout" "5" "--max-time" "15"
                                    "--ciphers" "DEFAULT@SECLEVEL=1"
                                    "-A" "Emacs plan-polsl.el (GNU Emacs)"
                                    k-urls)
                             (decode-coding-string (buffer-string) 'utf-8)))
             (pos 0))
        (while (string-match "href=\"plan\\.php\\?type=10&amp;id=\\([0-9]+\\)\"[^>]*>\\([^<]+\\)</a>" raw-teachers pos)
          (let ((tid (match-string 1 raw-teachers))
                (raw-name (match-string 2 raw-teachers))
                (end (match-end 0)))
            (let ((tname (string-trim raw-name)))
              (when (> (length tname) 1)
                (puthash tname tid teachers-dict)))
            (setq pos end)))
        (let ((alist nil))
          (maphash (lambda (name id) (push (cons name id) alist)) teachers-dict)
          (setq plan-polsl-search--teachers-cache
                (sort alist (lambda (a b) (string< (car a) (car b))))))

        ;; persist to disk cache
        (ignore-errors
          (let ((dir (file-name-directory plan-polsl-search-cache-file)))
            (unless (file-directory-p dir)
              (make-directory dir t)))
          (with-temp-file plan-polsl-search-cache-file
            (insert ";; Plan-PolSL teachers cache\n")
            (prin1 plan-polsl-search--teachers-cache (current-buffer))))
        plan-polsl-search--teachers-cache))))

(defun plan-polsl-search--get-teachers (&optional force-refresh)
  "Get teachers alist from memory, disk cache, or live fetch if FORCE-REFRESH."
  (cond
   ((and (not force-refresh) plan-polsl-search--teachers-cache)
    plan-polsl-search--teachers-cache)
   ((and (not force-refresh) (file-exists-p plan-polsl-search-cache-file))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents plan-polsl-search-cache-file)
          (goto-char (point-min))
          (forward-line 1)
          (setq plan-polsl-search--teachers-cache (read (current-buffer))))
      (error
       (plan-polsl-search--fetch-all-teachers))))
   (t
    (message "Pobieranie listy nauczycieli z plan.polsl.pl...")
    (redisplay)
    (plan-polsl-search--fetch-all-teachers))))

;;;###autoload
(defun plan-polsl-refresh-teachers ()
  "Force re-fetching the live academic teachers directory from plan.polsl.pl."
  (interactive)
  (message "Aktualizowanie listy nauczycieli z plan.polsl.pl...")
  (redisplay)
  (let ((teachers (plan-polsl-search--fetch-all-teachers)))
    (message "Zaktualizowano bazę nauczycieli: %d osób" (length teachers))))

;;;###autoload
(defun plan-polsl-search (&optional force-refresh)
  "Search for a PolSL schedule:
- For teachers: fuzzy search by surname across all university faculties
- For student groups / rooms: step-by-step interactive tree navigation.
With prefix arg FORCE-REFRESH, forces updating teacher directory from web."
  (interactive "P")

  ;; choose category
  (let* ((category-names (mapcar #'car plan-polsl-search--type-alist))
         (chosen-cat (completing-read "Szukaj planu: " category-names nil t))
         (handler (cdr (assoc chosen-cat plan-polsl-search--type-alist))))
    (unless handler
      (user-error "Nie wybrano kategorii"))
    (if (eq handler :teacher)

        ;; instant fuzzy search by surname
        (let* ((teachers (plan-polsl-search--get-teachers force-refresh))
               (teacher-names (mapcar #'car teachers))
               (choice (completing-read "Wybierz nauczyciela (wpisz nazwisko): "
                                        teacher-names nil t))
               (tid (cdr (assoc choice teachers))))
          (unless tid
            (user-error "Nie wybrano nauczyciela"))
          (message "Otwieranie planu: %s..." choice)
          (plan-polsl tid 10 t))

      ;; tree navigation for student groups and rooms
      (let ((menu-type handler)
            (breadcrumb (list (car (split-string chosen-cat " " t)))))
        (message "Pobieranie listy wydziałów...")
        (redisplay)
        (let ((nodes (condition-case err
                         (plan-polsl-search--fetch-root-nodes menu-type)
                       (error (user-error "Błąd połączenia z plan.polsl.pl: %s" (error-message-string err))))))
          (unless nodes
            (user-error "Nie znaleziono wydziałów na plan.polsl.pl"))
          (catch 'plan-polsl-search--done
            (while t
              (let* ((choices (mapcar (lambda (n)
                                        (cons (plan-polsl-search--format-choice n) n))
                                      nodes))
                     (prompt (format "%s > " (mapconcat #'identity (reverse breadcrumb) " > ")))
                     (selected-name (completing-read prompt
                                                     (mapcar #'car choices)
                                                     nil t))
                     (selected (cdr (assoc selected-name choices))))
                (unless selected
                  (user-error "Anulowano wyszukiwanie"))
                (pcase (car selected)
                  (:leaf
                   ;; display the plan
                   (let* ((leaf-type-str (nth 1 selected))
                          (leaf-id (nth 2 selected))
                          (leaf-name (nth 3 selected))
                          (plan-type (or (cdr (assoc leaf-type-str
                                                     plan-polsl-search--leaf-type-alist))
                                         0)))
                     (message "Otwieranie planu: %s..." leaf-name)
                     (redisplay)
                     (plan-polsl leaf-id plan-type t)
                     (throw 'plan-polsl-search--done t)))
                  (:branch
                   ;; drill deeper
                   (let ((branch-id (nth 1 selected))
                         (branch-name (nth 2 selected)))
                     (push branch-name breadcrumb)
                     (message "Pobieranie: %s..." branch-name)
                     (redisplay)
                     (let ((children (condition-case err
                                         (plan-polsl-search--fetch-children menu-type branch-id)
                                       (error (user-error "Błąd pobierania: %s" (error-message-string err))))))
                       (if children
                           (setq nodes children)
                         (user-error "Brak elementów w: %s" branch-name))))))))))))))

(provide 'plan-polsl-search)
;;; plan-polsl-search.el ends here
