;;; emacs-surround.el --- surround symbol or change surrouning char -*- lexical-binding: t -*-

;; Copyright (C) 2015 ganmacs

;; Author: ganmacs <ganmacs_at_gmail.com>
;; Maintainer: ganmacs <ganmacs_at_gmail.com>
;; URL: https://github.com/ganmacs/emacs-surround
;; Version: 0.0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defgroup emacs-surround nil
  "surround.vim for Emacs"
  :group 'surround)

(defcustom emacs-surround-alist
  '((""   . (""  . ""))
    ("'"  . ("'" . "'"))
    ("("  . ("(" . ")"))
    ("{"  . ("{" . "}"))
    ("["  . ("[" . "]"))
    ("/"  . ("/" . "/"))
    ("\"" . ("\"" . "\"")))
  "Surround key list."
  :group 'surround)

(defun emacs-surround-mark-region-line ()
  "Return list which is begening point of line and end point of line."
  (let ((start
		 (progn (back-to-indentation) (point))))
    (cons start (line-end-position))))

(defun emacs-surround-mark-region-sep ()
  "Return the bound of current sexp."
  (or (bounds-of-thing-at-point 'sexp)
	  ;; handle if text under cursor is empty, e.g. just space
	  (cons (point) (point))))

(defun emacs-surround-same-count-p (str a b)
  "Check that A and B appearing number in STR are same or not."
  (cl-flet ((count-match-str (regex)
                             (with-temp-buffer
                               (insert str)
                               (goto-char (point-min))
                               (count-matches regex))))
    (not (= (count-match-str a)
            (count-match-str b)))))

(defun emacs-surround-quote-p (type)
  "Return quote string or not.
TYPE is `forward` or `backward`."
  (let ((min (point-at-bol))
        (max (point-at-eol))
        (ppoint (if (eq type 'backward) (point) (- (point) 1))))
    (defun iter (i p)
      (if (and (<= min p) (<= p max))
          (if (= (char-before p) 92) ; backquote
              (iter (+ i 1) (- p 1))
            (= (mod i 2) 1))))
    (iter 0 ppoint)))

(defun emacs-surround-mark-between (prefix suffix)
  "Return list whch in PREFIX point and SUFFIX point."
  (cl-flet ((search-prefix () (search-backward prefix (point-min) nil 1))
            (search-suffix () (search-forward suffix (point-max) nil 1))
            (same-p (s e) (emacs-surround-same-count-p
                           (buffer-substring s e) prefix suffix)))
    (let* ((origin (point))
           (same-surrounds-p (string= prefix suffix)))
      (defun search-prefix-to-suffix ()
        (goto-char origin)
        (let* ((start (progn
                        (search-prefix)
                        (while (unless suffix (emacs-surround-quote-p 'backward))
						  (search-prefix))
                        (point)))
               (end (progn
                      (forward-char)
                      (search-suffix)
                      (while (if same-surrounds-p
                                 (emacs-surround-quote-p 'forward)
                               (same-p start (point)))
                        (search-suffix))
                      (point))))
          (cons start end)))
      (defun search-suffix-to-prefix ()
        (goto-char origin)
        (let* ((end (progn
                      (search-suffix)
                      (while (unless suffix (emacs-surround-quote-p 'forward))
						(search-suffix))
                      (point)))
               (start (progn
                        (backward-char)
                        (search-prefix)
                        (while (if same-surrounds-p
                                   (emacs-surround-quote-p 'forward)
                                 (same-p (point) end))
                          (search-prefix))
                        (point))))
          (cons start end)))
      (let* ((l1 (search-suffix-to-prefix))
             (l2 (search-prefix-to-suffix))
             (start (min (car l1) (car l2)))
             (end (max (cadr l1) (cadr l2))))
        (cons start end)))))

(defun emacs-surround-get-alist (key)
  "Get list by emacs-surround-alit with KEY."
  (or (cdr (assoc key emacs-surround-alist))
	  (cons key key)))

(defun emacs-surround-wrap (str prefix &optional suffix)
  "Wrap STR with PREFIX and SUFFIX(if suffix exists)."
  (concat prefix str (or suffix prefix)))

(defun emacs-surround-replace (str from to)
  "Replace FROM to TO in STR.
\\(FROM\\)STR\\(FROM\\) -> \\(TO\\)STR\\(TO\\)."
  (let* ((f-prefix (car from)) (f-suffix (cdr from))
         (t-prefix (car to)) (t-suffix (cdr to))
         (regx (format "^%s\\(\\(.\\|\n\\)*\\)%s$" f-prefix f-suffix)))
    (if (string-match regx str)
        (let ((match (match-string 1 str)))
          (emacs-surround-wrap match t-prefix t-suffix)))))

(defun emacs-surround-cut-region (region)
  "Cut region REGION car to REGION cdar."
  (buffer-substring (car region) (cdr region)))

(defun emacs-surround-region-list (fn)
  "If 'mark-active then region list else call FN and return list."
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (funcall fn)))

(defun emacs-surround-helper (mark-fn prefix suffix)
 "Helper function `emacs-surround' (inset|delte|line|change).
MARK-FN is regioning function.
PREFIX and SUFFIX are replace string."
 (save-excursion
   (let* ((region (emacs-surround-region-list mark-fn))
          (target-str (emacs-surround-cut-region region))
          (replaced-str (emacs-surround-replace
                         target-str
                         (emacs-surround-get-alist prefix)
                         (emacs-surround-get-alist suffix))))
     (if replaced-str
         (progn
           (delete-region (car region) (cdr region))
           (insert replaced-str))
       (message "not found prefix and suffix")))))

(defun emacs-surround-insert (str)
  "Insert surround string, STR."
  (interactive (list (char-to-string (read-char))))
  (emacs-surround-helper 'emacs-surround-mark-region-sep "" str))

(defun emacs-surround-delete (str)
  "Delete surround string, STR."
  (interactive (list (char-to-string (read-char))))
  (let ((s (emacs-surround-get-alist str)))
    (emacs-surround-helper (lambda () (emacs-surround-mark-between (car s) (cdr s)))
                           str "")))

(defun emacs-surround-line (str)
  "Wrap line with STR."
  (interactive (list (char-to-string (read-char))))
  (emacs-surround-helper 'emacs-surround-mark-region-line "" str))

(defun emacs-surround-change (to end)
  "Change surround string TO into END."
  (interactive (list (char-to-string (read-char))
					 (char-to-string (read-char))))
  (let ((s (emacs-surround-get-alist to)))
    (emacs-surround-helper (lambda () (emacs-surround-mark-between (car s) (cdr s)))
                           to end)))

;;;###autoload
(defun emacs-surround (arg)
  "Surround or Delete symbol etc.
if cmd1 is i(insert), surround at-point-symbol.
if cmd1 is d(delete), delete surround cmd2 char.
if cmd1 is l(line), surround line which point is here.
else change surround cmd1 to cmd2"
  (interactive "P")
  (cond ((equal arg '(4)) (call-interactively 'emacs-surround-change))
		((equal arg '(16)) (call-interactively 'emacs-surround-delete))
		(:else (call-interactively 'emacs-surround-insert)))
  ;; (let* ((cmd1 (read-char))
  ;;        (_cmd2 (read-char))
  ;;        (cmd2 (char-to-string _cmd2)))
  ;; 	;; (assoc cmd1 emacs-surround-map)
  ;; 	(cl-case cmd1
  ;; 	  (?i )
  ;; 	  (?d (emacs-surround-delete cmd2))
  ;; 	  (?l (emacs-surround-line cmd2))
  ;; 	  (otherwise
  ;; 	   (if (= ?\r _cmd2)				;return
  ;;          (emacs-surround-insert (char-to-string cmd1))
  ;;        (emacs-surround-change (char-to-string cmd1) cmd2)))))
  )


(provide 'emacs-surround)
  
;;; emacs-surround.el ends here
