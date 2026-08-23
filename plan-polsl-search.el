;;; plan-polsl-search.el --- Interactive timetable and faculty search -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, search

;;; Commentary:
;; Interactive search module for Politechnika Śląska timetables:
;; - Teachers: dynamic live fetch & instant fuzzy search across university faculty members
;; - Student groups / Rooms: step-by-step interactive tree drill-down

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

(defconst plan-polsl-search-categories
  '(("Grupa studencka" . 1)
    ("Nauczyciel" . :teacher)
    ("Sala / zasób" . 3))
  "Mapping from human-readable category name to search mode identifier.")

(defconst plan-polsl-search-type-map
  '(("0"  . 0)
    ("2"  . 0)
    ("10" . 10)
    ("12" . 10)
    ("20" . 20))
  "Mapping from plan.php?type= URL parameter to internal numeric type.")

(defun plan-polsl-search--parse-root-faculties (html)
  "Extract root faculty branches from left_menu.php HTML.
Returns list of (FACULTY-NAME . FACULTY-ID) pairs."
  (let ((faculties nil)
        (pos 0))
    (while (string-match "branch([0-9]+,\\([0-9]+\\),[0-9]+,'\\([^']+\\)')" html pos)
      (let ((fid (match-string 1 html))
            (fname (match-string 2 html))
            (end (match-end 0)))
        (push (cons fname fid) faculties)
        (setq pos end)))
    (nreverse faculties)))

(defun plan-polsl-search--parse-feed (html)
  "Parse left_menu_feed.php HTML response into branches and leaf plans.
Returns list of (:branch ID NAME) and (:leaf TYPE ID NAME) items."
  (let ((branches nil)
        (leaves nil)
        (branch-ids (make-hash-table :test 'equal))
        (pos 0))

    ;; extract expandable sub-branches
    (while (string-match "get_left_tree_branch([ \t\n]*'\\([0-9]+\\)'" html pos)
      (let* ((bid (match-string 1 html))
             (end (match-end 0))
             (chunk (substring html end (min (length html) (+ end 500))))
             (name (cond
                    ((string-match ">\\([^<]+\\)</a>" chunk)
                     (string-trim (match-string 1 chunk)))
                    ((string-match ">[ \t]*\\([^<]+[^ <\t]\\)[ \t]*<div" chunk)
                     (string-trim (match-string 1 chunk)))
                    (t (format "ID:%s" bid)))))
        (push (list :branch bid name) branches)
        (puthash bid t branch-ids)
        (setq pos end)))

    ;; extract final plan links
    (setq pos 0)
    (while (string-match "href=\"plan\\.php\\?type=\\([0-9]+\\)&amp;id=\\([0-9]+\\)\"[^>]*>\\([^<]+\\)</a>" html pos)
      (let ((ltype (match-string 1 html))
            (lid (match-string 2 html))
            (lname (string-trim (match-string 3 html)))
            (end (match-end 0)))
        (push (list :leaf ltype lid lname) leaves)
        (setq pos end)))

    ;; combine branches and unique standalone leaves
    (append (nreverse branches)
            (cl-remove-if (lambda (l)
                            (or (not (member (nth 1 l) '("0" "10" "20")))
                                (gethash (nth 2 l) branch-ids)))
                          (nreverse leaves)))))

(defun plan-polsl-search--extract-teachers-from-html (html dict)
  "Extract teacher links from HTML and insert into hash table DICT."
  (let ((pos 0))
    (while (string-match "href=\"plan\\.php\\?type=10&amp;id=\\([0-9]+\\)\"[^>]*>\\([^<]+\\)</a>" html pos)
      (let ((tid (match-string 1 html))
            (raw-name (match-string 2 html))
            (end (match-end 0)))
        (let ((name (string-trim raw-name)))
          (when (> (length name) 1)
            (puthash name tid dict)))
        (setq pos end)))))

(defun plan-polsl-search--fetch-all-teachers ()
  "Dynamically crawl and index all faculty teachers from plan.polsl.pl in parallel."
  (let* ((base (or (bound-and-true-p plan-polsl-base-url) "https://plan.polsl.pl/"))
         (url-faculties (format "%sleft_menu.php?type=2"
                                (if (string-suffix-p "/" base) base (concat base "/"))))
         (raw-faculties (plan-polsl-http-fetch url-faculties))
         (faculties (plan-polsl-search--parse-root-faculties raw-faculties))
         (teachers-dict (make-hash-table :test 'equal)))

    ;; fetch all faculty departments in parallel
    (let* ((f-urls (mapcar (lambda (f)
                             (format "%sleft_menu_feed.php?type=2&branch=%s&link=0&bOne=1"
                                     (if (string-suffix-p "/" base) base (concat base "/"))
                                     (cdr f)))
                           faculties))
           (raw-katedry (plan-polsl-http-fetch-parallel f-urls 20))
           (k-branches nil)
           (pos 0))
      (while (string-match "get_left_tree_branch([ \t\n]*'\\([0-9]+\\)'" raw-katedry pos)
        (let ((kid (match-string 1 raw-katedry))
              (end (match-end 0)))
          (push kid k-branches)
          (setq pos end)))
      (plan-polsl-search--extract-teachers-from-html raw-katedry teachers-dict)
      (setq k-branches (delete-dups k-branches))

      ;; fetch all department teacher feeds in parallel
      (let* ((k-urls (mapcar (lambda (kid)
                               (format "%sleft_menu_feed.php?type=2&branch=%s&link=0&bOne=1"
                                       (if (string-suffix-p "/" base) base (concat base "/"))
                                       kid))
                             k-branches))
             (raw-teachers (plan-polsl-http-fetch-parallel k-urls 30)))
        (plan-polsl-search--extract-teachers-from-html raw-teachers teachers-dict)

        ;; build sorted alist
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
  "Return teacher alist from memory, disk cache, or live network fetch."
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
      (error (plan-polsl-search--fetch-all-teachers))))
   (t
    (message "Pobieranie listy nauczycieli z plan.polsl.pl...")
    (redisplay)
    (plan-polsl-search--fetch-all-teachers))))

(defun plan-polsl-search--navigate-tree (handler category-name)
  "Navigate category tree (groups or rooms) starting from root faculties."
  (let* ((base (or (bound-and-true-p plan-polsl-base-url) "https://plan.polsl.pl/"))
         (root-url (format "%sleft_menu.php?type=%d"
                           (if (string-suffix-p "/" base) base (concat base "/"))
                           handler))
         (breadcrumb (list (car (split-string category-name " " t))))
         (raw-root (plan-polsl-http-fetch root-url))
         (nodes (mapcar (lambda (p) (list :branch (cdr p) (car p)))
                        (plan-polsl-search--parse-root-faculties raw-root))))
    (unless nodes
      (user-error "Nie znaleziono wydziałów na plan.polsl.pl"))
    (catch 'plan-polsl-search--done
      (while t
        (let* ((choices (mapcar (lambda (n)
                                  (cons (pcase (car n)
                                          (:branch (nth 2 n))
                                          (:leaf   (nth 3 n)))
                                        n))
                                nodes))
               (prompt (format "%s > " (mapconcat #'identity (reverse breadcrumb) " > ")))
               (selected-name (completing-read prompt (mapcar #'car choices) nil t))
               (selected (cdr (assoc selected-name choices))))
          (unless selected
            (user-error "Anulowano wyszukiwanie"))
          (pcase (car selected)
            (:leaf
             (let* ((leaf-type-str (nth 1 selected))
                    (leaf-id (nth 2 selected))
                    (leaf-name (nth 3 selected))
                    (plan-type (or (cdr (assoc leaf-type-str plan-polsl-search-type-map)) 0)))
               (message "Otwieranie planu: %s..." leaf-name)
               (plan-polsl leaf-id plan-type t)
               (throw 'plan-polsl-search--done t)))
            (:branch
             (let* ((branch-id (nth 1 selected))
                    (branch-name (nth 2 selected))
                    (feed-url (format "%sleft_menu_feed.php?type=%d&branch=%s&link=0&bOne=1"
                                      (if (string-suffix-p "/" base) base (concat base "/"))
                                      handler branch-id))
                    (raw-feed (plan-polsl-http-fetch feed-url))
                    (children (plan-polsl-search--parse-feed raw-feed)))
               (push branch-name breadcrumb)
               (if children
                   (setq nodes children)
                 (user-error "Brak elementów w: %s" branch-name))))))))))

;;;###autoload
(defun plan-polsl-refresh-teachers ()
  "Force re-fetching the academic teachers directory from plan.polsl.pl."
  (interactive)
  (message "Aktualizowanie listy nauczycieli z plan.polsl.pl...")
  (redisplay)
  (let ((teachers (plan-polsl-search--fetch-all-teachers)))
    (message "Zaktualizowano bazę nauczycieli: %d osób" (length teachers))))

;;;###autoload
(defun plan-polsl-search (&optional force-refresh)
  "Interactively search for and open any PolSL timetable:
- For teachers: instant fuzzy search by surname across all faculty members
- For student groups / rooms: step-by-step interactive tree navigation.
With prefix arg FORCE-REFRESH, forces updating teacher directory from network."
  (interactive "P")
  (let* ((chosen-cat (completing-read "Szukaj planu: "
                                      (mapcar #'car plan-polsl-search-categories)
                                      nil t))
         (handler (cdr (assoc chosen-cat plan-polsl-search-categories))))
    (unless handler
      (user-error "Nie wybrano kategorii"))
    (if (eq handler :teacher)
        (let* ((teachers (plan-polsl-search--get-teachers force-refresh))
               (choice (completing-read "Wybierz nauczyciela (wpisz nazwisko): "
                                        (mapcar #'car teachers) nil t))
               (tid (cdr (assoc choice teachers))))
          (unless tid
            (user-error "Nie wybrano nauczyciela"))
          (message "Otwieranie planu: %s..." choice)
          (plan-polsl tid 10 t))
      (plan-polsl-search--navigate-tree handler chosen-cat))))

(provide 'plan-polsl-search)
;;; plan-polsl-search.el ends here
