;;; plan-polsl-search.el --- Tree-based schedule search -*- lexical-binding: t; -*-

;; Author: Szymon Wilczek
;; Keywords: calendar, polsl, search

;;; Commentary:
;; Interactive tree navigation through plan.polsl.pl's department/group hierarchy.
;; Walks the user down the tree (Faculty -> Programme -> Semester -> Group) or
;; (Faculty -> Department -> Teacher) using `completing-read' at each level,
;; then displays the selected schedule in the *Plan PolSL* buffer.

;;; Code:

(require 'cl-lib)
(require 'plan-polsl-http)

;; map type names to the API type parameter
(defconst plan-polsl-search--type-alist
  '(("Grupa studencka" . 1)
    ("Nauczyciel" . 2)
    ("Sala / zasób" . 3))
  "Mapping from human-readable category names to plan.polsl.pl left_menu type values.")

;; map API leaf link type= values to plan-polsl-type values for rendering
(defconst plan-polsl-search--leaf-type-alist
  '(("0"  . 0)
    ("2"  . 0)
    ("10" . 10)
    ("12" . 10)
    ("20" . 20))
  "Mapping from plan.php?type= parameter to `plan-polsl-type' values.")

(defun plan-polsl-search--fetch-feed (url)
  "Fetch URL content using curl with ISO-8859-2 decoding."
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
            (name (match-string 4 html)))
        (push (cons name id) entries))
      (setq pos (match-end 0)))
    (nreverse entries)))

(defun plan-polsl-search--parse-feed (html)
  "Parse left_menu_feed.php HTML response.
Returns a list of entries, each one of:
  (:branch ID NAME) - a subtree that can be expanded further
  (:leaf TYPE ID NAME) - a final plan link (type=0/10/20 etc.)

Branches are extracted from get_left_tree_branch() onclick calls.
Leaves are extracted from plan.php href links.
Only returns branches that have no corresponding leaf, or returns
leaves that point to actual individual plans (type=0/10/20)."
  (let ((branches nil)
        (leaves nil)
        (branch-ids (make-hash-table :test 'equal))
        (pos 0))

    ;; extract branches from get_left_tree_branch('ID', ...)
    ;; name comes from the text after the closing > of the img tag
    (while (string-match
            "get_left_tree_branch([ \t\n]*'\\([0-9]+\\)'"
            html pos)
      (let* ((bid (match-string 1 html))
             (after-pos (match-end 0))

             ;; find the name
             (chunk (substring html after-pos (min (length html) (+ after-pos 500))))
             (name (cond
                    ;; name in <a href="plan.php...">NAME</a>
                    ((string-match ">\\([^<]+\\)</a>" chunk)
                     (string-trim (match-string 1 chunk)))

                    ;; plain text
                    ((string-match ">[ \t]*\\([^<]+[^ <\t]\\)[ \t]*<div" chunk)
                     (string-trim (match-string 1 chunk)))
                    (t (format "ID:%s" bid)))))
        (push (list :branch bid name) branches)
        (puthash bid t branch-ids))
      (setq pos (match-end 0)))

    ;; extract leaf plan.php links
    (setq pos 0)
    (while (string-match
            "href=\"plan\\.php\\?type=\\([0-9]+\\)&amp;id=\\([0-9]+\\)\"[^>]*>\\([^<]+\\)</a>"
            html pos)
      (let ((ltype (match-string 1 html))
            (lid (match-string 2 html))
            (lname (string-trim (match-string 3 html))))
        (push (list :leaf ltype lid lname) leaves))
      (setq pos (match-end 0)))

    ;; Determine what to return:
    ;; - if branch ID also appears as leaf, it means the node is both
    ;;   expandable AND directly viewable -> keep it as branch
    ;; - leaves with type=0/10/20 that are NOT also branches are final selections
    ;; - leaves with type=2/12 are "aggregate" links (whole semester/katedra)
    ;;   - skip them if same ID is already a branch
    (let ((result nil))

      ;; first add branches (always expandable)
      (dolist (b (nreverse branches))
        (push b result))

      ;; then add leaves that are NOT branches
      (dolist (l (nreverse leaves))
        (let ((lid (nth 2 l))
              (ltype (nth 1 l)))

          ;; only add leaf if its individual plan link and not already a branch
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
  "Fetch root-level nodes for MENU-TYPE (1=groups, 2=teachers, 3=rooms).
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

;;;###autoload
(defun plan-polsl-search ()
  "Interactively search for a PolSL schedule by navigating the tree hierarchy.
First choose category (group/teacher/room), then drill down through
faculties, departments, semesters, and groups until a final plan is selected.
The selected schedule is displayed in the *Plan PolSL* buffer."
  (interactive)
  ;; choose category
  (let* ((category-names (mapcar #'car plan-polsl-search--type-alist))
         (chosen-cat (completing-read "Szukaj planu: " category-names nil t))
         (menu-type (cdr (assoc chosen-cat plan-polsl-search--type-alist)))
         (breadcrumb (list chosen-cat)))
    (unless menu-type
      (user-error "Nie wybrano kategorii"))

    ;; fetch root nodes and walk down the tree
    (message "Pobieranie listy wydziałów...")
    (let ((nodes (plan-polsl-search--fetch-root-nodes menu-type)))
      (unless nodes
        (user-error "Nie znaleziono wydziałów na plan.polsl.pl"))
      (catch 'plan-polsl-search--done
        (while t
          (let* ((choices (mapcar (lambda (n)
                                    (cons (plan-polsl-search--format-choice n) n))
                                  nodes))
                 (prompt (format "%s > " (mapconcat #'identity breadcrumb " > ")))
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
                 (plan-polsl leaf-id plan-type t)
                 (throw 'plan-polsl-search--done t)))
              (:branch
               ;; drill deeper
               (let ((branch-id (nth 1 selected))
                     (branch-name (nth 2 selected)))
                 (push branch-name breadcrumb)
                 (message "Pobieranie: %s..." branch-name)
                 (let ((children (plan-polsl-search--fetch-children menu-type branch-id)))
                   (if children
                       (setq nodes children)
                     (user-error "Brak elementów w: %s" branch-name))))))))))))

(provide 'plan-polsl-search)
;;; plan-polsl-search.el ends here
