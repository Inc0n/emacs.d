
;;; Commentary:
;;; Code:

(defvar org-book-directory "~/books/"
  "The directory path to recursively find books.")

(defvar org-book-org-file "~/books/books.org"
  "The org file path to store the org-book states.")

(defvar org-book-verbose t
  "Useful for debug and see the changes org-book has made.")


(defvar org-book-extensions '("epub" "txt")
  "Extensions of files to scan for books.")

(defun org-book--gen-fd-extensions ()
  (mapconcat (lambda (x) (format "--extension %s" x))
             org-book-extensions
             " "))

(defun org-book--fd-dirs-command ()
  (format "fd -t f %s . -x dirname {} | sort | uniq"
          ;; generate the extensions
          (org-book--gen-fd-extensions)))

(defun org-book--fd-books-command (dir)
  (format "fd -t f --maxdepth 1 %s . '%s'"
          (org-book--gen-fd-extensions) dir))

(defun org-book--heading-exist-p (level title)
  "Regex search headline with LEVEL and TITLE."
  ;; (org-find-exact-headline-in-buffer )
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (rx (literal (s-repeat level "*")) (1+ any) (literal title))
                       nil t)))

(defun org-book--headline-match (level title)
  "Match headline with LEVEL and TITLE.
Returns the first point of matching headline if any."
  (car
   (delq nil
         (org-map-entries
          (lambda ()
            (let ((elm (org-element-context)))
              (and (string= (org-element-property :title elm) title)
                   (org-element-property :begin elm))))
          (format "LEVEL=%d" level)))))

(defun org-book--headline-at-path (path)
  (ignore-errors
    (org-find-olp path)))

(defun org-book-gen-headline (level title)
  (when (save-excursion
          (beginning-of-line)
          (not (looking-at-p "[[:space:]]*$")))
    (newline))
  (insert (s-repeat level "*"))
  (insert " " title))

(defun org-book-gen-subnode (epub level)
  (let ((title (file-name-base epub)))
    (when (not (org-book--heading-exist-p level title))
      (when org-book-verbose
        (message "Creating book entry %s %s" level title))
      ;; DONE: insert Initial state
      (org-book-gen-headline level "TODO ")
      (goto-char (1+ (point)))
      ;; DONE: make this into a link instead of just name
      (org-insert-link
       t
       (abbreviate-file-name
        (expand-file-name epub default-directory))
       title)
      (insert "\n"))))

(defun org-book-gen-heading-and-content (dir title)
  "Generate the org heading and the subheadings (books).
DIR.
TITLE."
  (let* ((dir-parts (split-string dir "/"))
         (level (length dir-parts))
         (heading (org-book--headline-match level title)))
    ;; the heading itself
    (if heading
        (progn
          ;; DONE: narrow to target directory lets do some rewrite
          (goto-char heading)
          (when org-book-verbose
            (message "Fount dir node %s %s" level title))
          (org-narrow-to-subtree)
          (goto-char (point-max)))
      (when org-book-verbose
        (message "Creating dir node %s %s" level title))
      (org-book-gen-headline level (concat title "\n")))
    ;; now for books in this dir
    (let ((level (1+ level))
          (epubs
           (util/shell-command-to-lines
            (org-book--fd-books-command dir))))
      (dolist (epub epubs)
        (org-book-gen-subnode epub level)))
    (org-N-empty-lines-before-current 0)))

;; DONE: archive these headings
(defun org-book-check-books ()
  (interactive)
  (org-book--buffer)
  (--> (lambda (link)
         (let ((parent (org-element-property :parent link)))
           (when (and (eq (org-element-type parent) 'headline)
                      (not (file-exists-p (org-element-property :path link))))
             (org-element-property :raw-value parent))))
       (org-element-map (org-element-parse-buffer) 'link it)
       (dolist (headline it)
         (message "deleted %s" headline)
         (--> (org-find-exact-headline-in-buffer headline nil t)
              (goto-char it))
         (kill-whole-line))))


;;;###autoload
(defun org-book-gen ()
  (interactive)
  (org-book--buffer)
  ;; file content gen
  (let* ((default-directory org-book-directory)
         (dirs
          (util/shell-command-to-lines
           (org-book--fd-dirs-command)))
         (root (car dirs))
         (dirs (cdr dirs)))
    (save-restriction
      (org-book-gen-heading-and-content root "Ungrouped"))
    (dolist (dir dirs)
      (save-restriction
        (org-book-gen-heading-and-content dir (file-name-nondirectory dir)))))
  (goto-char (point-min))
  (org-book-save-library))

;; DONE: Store the buffer into `org-book-org-file'
;; DONE: save org-book library to keep track of states
(defun org-book-save-library ()
  (write-file
   (or org-book-org-file
       (read-file-name
        (format "save %s to: " "*org book library*")))))

;;;

(defun org-book--buffer ()
  "Switch to org-book buffer."
  (if org-book-org-file
      (find-file org-book-org-file)
    (switch-to-buffer "*org book library*")
    ;; (erase-buffer)
    (org-mode)))

;; (defun org-book-delete ()
;;   ;; (org-get-outline-path)
;;   (let ((link (org-element-context)))
;;     (delete-file (org-element-property :path link))))

(provide 'org-book)
;;; org-book ends here