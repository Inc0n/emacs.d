;; check also
;; https://github.com/lujun9972/html2org.el/blob/master/html2org.el
;; (libxml-avalable-p)
;;; Code:

(require 'dom)

(defun html2org--parse-html (&optional start end)
  "Configured html parsing.
Can Optionally specify START and END."
  (let ((discard-comments nil)
        (start (or start (point-min)))
        (end (or end (point-max)))
        (base-url nil))
    (libxml-parse-html-region start end base-url discard-comments)))

(defun html2org ()
  (interactive)
  (html2org-buffer (current-buffer)))

(defun html2org--buffer (buffer)
  "Origfy html BUFFER."
  (with-current-buffer buffer
    (html2org--parse-html (point-min) (point-max))))

;; dom accessor

(defun html2org-dom-text (node)
  (string-trim (dom-text node)))

;; parsers

(defun html2org-parse-content (start-string node)
  (concat start-string " " (html2org-dom-text node)))

(defun html2org-parse-skip (node)
  "Skip parsing this NODE, return empty string."
  (ignore node)
  nil)

(defun html2org-parse-meta (node)
  (let ((attr (dom-attributes node)))
    (and (and (assq 'name attr) (assq 'content attr))
         (format "#+%s: %s"
                 (cdr (assq 'name attr))
                 (cdr (assq 'content attr))))))

(defun html2org-parse-link (node)
  (let ((attr (dom-attributes node))
        (text (html2org-dom-text node)))
    (if (string= text "")
        nil
      (if (assq 'href attr)
          (format "[[%s][%s]]"
                  (cdr (assq 'href attr))
                  (html2org-dom-text node))
        (html2org-dom-text node)))))

(defun html2org-parse-surround (string node)
  (format "%s%s%s" string (html2org-dom-text node) string))

;; html2org-parse-rules
;; TODO: add default parse rules
(defvar html2org-parse-rules
  `((html html2org-parse-children)
    (comment html2org-parse-content "#")
    (top html2org-parse-children)
    (meta html2org-parse-meta)
    (title html2org-parse-content "#+title:")
    (div html2org-parse-children)
    (a html2org-parse-link)
    (b html2org-parse-surround "*")
    ;;
    (style html2org-parse-skip)
    (script html2org-parse-skip)
    (form html2org-parse-skip)
    (span html2org-parse-skip)
    (link html2org-parse-skip)))

(defun html2org-parse-children (node)
  "Parse NODE children."
  (mapcar 'html2org-parse (dom-children node)))

(defun html2org--parse (node)
  "NODE."
  (cond ((listp node)
         (let ((children (dom-children node))
               (tag (dom-tag node)))
           (let ((parser (assoc tag html2org-parse-rules)))
             (if parser
                 (if (cddr parser)
                     (apply (cadr parser) (append (cddr parser) (list node)))
                     (funcall (cadr parser) node))
               (mapcar 'html2org-parse children)))))
        ((stringp node)
         (let ((text (string-trim node)))
           (if (string= text "")
               nil
             text)))
        (t (error "Unable to parse node %s" node))))

(defun html2org-parse (node)
  "NODE."
  (delete nil (html2org--parse node)))

(provide 'html2org)
;;; html2org.el ends here