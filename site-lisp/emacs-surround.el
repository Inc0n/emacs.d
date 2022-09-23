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

(defgroup emacs-surround ()
  "`surround.vim' for Emacs."
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
  :group 'emacs-surround)

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

(defun emacs-surround-get-alist (key)
  "Get list by emacs-surround-alit with KEY."
  (or (cdr (assoc key emacs-surround-alist))
	  (cons key key)))

(defun emacs-surround-replace (str from to)
  "Replace FROM to TO in STR.
\\(FROM\\)STR\\(FROM\\) -> \\(TO\\)STR\\(TO\\)."
  ;; using direct string manipulation instead of regex based
  ;; as changing from any delim to () is impossible.
  ;; i.e. emacs-surround-change-at-point [] -> ()
  (pcase-let ((`(,f-prefix . ,f-suffix) from)
			  (`(,t-prefix . ,t-suffix) to))
    (concat t-prefix
			(string-remove-suffix
			 f-suffix
			 (string-remove-prefix f-prefix str))
			t-suffix)))

(defun emacs-surround-region-list (fn)
  "If 'mark-active then region list else call FN and return list."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (funcall fn)))

(defun emacs-surround-helper (mark-fn prefix suffix)
  "Helper function `emacs-surround' (inset|delte|line|change).
MARK-FN is regioning function.
PREFIX and SUFFIX are replace string."
  (save-excursion
	(let* ((region (emacs-surround-region-list mark-fn))
           (target-str (buffer-substring (car region) (cdr region)))
           (replaced-str (emacs-surround-replace
                          target-str
                          (emacs-surround-get-alist prefix)
                          (emacs-surround-get-alist suffix))))
      (if replaced-str
          (progn
			(delete-region (car region) (cdr region))
			(insert replaced-str))
		(message "not found prefix and suffix")))))

(defun emacs-surround-delete-at-point ()
  "Delete surround delim."
  (interactive)
  (emacs-surround-change-at-point ""))

(defun emacs-surround-line (str)
  "Wrap line with STR."
  (interactive (list (char-to-string (read-char))))
  (emacs-surround-helper 'emacs-surround-mark-region-line "" str))

(defun emacs-surround-error-if-not-start-of-sexp ()
  "Start of sexp with delim checking (throws user-error)."
  (let* ((bounds
		  ;; BUG: cannot find bound in org-mode file e.g. ^example^
		  (or (bounds-of-thing-at-point 'sexp)
			  (bounds-of-thing-at-point 'word)))
		 (delim (char-after (car bounds))))
	(when (not
		   ;; (eq (char-syntax (char-after (car bounds))) ?\( )
		   bounds)
	  (user-error "Sexp delim mismatch, expected () delim, got %c in %S"
				  delim
				  (buffer-substring (car bounds) (cdr bounds))))))

(defun emacs-surround-change-at-point (to)
  "Change surround delim at point into TO."
  (interactive
   (progn (emacs-surround-error-if-not-start-of-sexp)
		  (list (char-to-string (read-char)))))
  (let* ((bounds (bounds-of-thing-at-point 'sexp))
		 (delim (char-after (car bounds))))
	(emacs-surround-helper
	 (lambda () bounds)
	 (char-to-string delim)
	 to)))

(defun emacs-surround-insert (str &optional mark-fn)
  "Insert surround string (region by MARK-FN) with STR."
  (interactive (list (char-to-string (read-char))))
  (emacs-surround-helper
   (or mark-fn
	   'emacs-surround-mark-region-sep)
   "" str))

;;;###autoload
(defun emacs-surround (arg)
  "Surround or Delete symbol etc.
if cmd1 is i(insert), surround at-point-symbol.
if cmd1 is d(delete), delete surround cmd2 char.
if cmd1 is l(line), surround line which point is here.
else change surround cmd1 to cmd2"
  (interactive "P")
  (cond ((equal arg '(4)) (call-interactively 'emacs-surround-change-at-point))
		((equal arg '(16)) (call-interactively 'emacs-surround-delete-at-point))
		(:else (call-interactively 'emacs-surround-insert))))

(provide 'emacs-surround)
;;; emacs-surround.el ends here
