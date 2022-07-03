;;; highlight-symbol.el --- automatic and manual symbol highlighting
;;
;; Copyright (C) 2007-2009, 2013-2015 Nikolaj Schumacher
;; Copyright (C) 2021 Danny He
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 1.4
;; Keywords: faces, matching
;; URL: http://nschum.de/src/emacs/highlight-symbol/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x, GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Add the following to your .emacs file:
;; (require 'highlight-symbol)
;; (global-set-key [(control f3)] 'highlight-symbol)
;; (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
;;
;; Use `highlight-symbol' to toggle highlighting of the symbol at
;; point throughout the current buffer.  Use `highlight-symbol-mode' to keep the
;; symbol at point highlighted.
;;
;; The functions `highlight-symbol-next', `highlight-symbol-prev',
;; `highlight-symbol-next-in-defun' and `highlight-symbol-prev-in-defun' allow
;; for cycling through the locations of any symbol at point.  Use
;; `highlight-symbol-nav-mode' to enable key bindings (M-p and M-p) for
;; navigation.
;;
;; `highlight-symbol-query-replace' can be used to replace the symbol.
;;
;;; Change Log:
;;
;;; Code:

(require 'thingatpt)
(require 'cl-lib)

(defgroup highlight-symbol ()
  "Automatic and manual symbols highlighting."
  :group 'faces
  :group 'matching)

(defcustom highlight-symbol-color
  (lambda ()
    `(;; (background-color . ,(face-attribute  :background))
      ;; (foreground-color . ,highlight-symbol-foreground-color)
      :underline t :inherit lazy-highlight))
  "It should be a cons of (type . value)
Color type can be one of `function', `list', `face', `color'.
`function' will be called with no arguments that returns a face.
`list' should contain a list of background colors to cycle through.
`face' should have a face name to apply to symbols.
`color' should be a background color to apply to symbols."
  :type '(choice function (list color) face color)
  :group 'highlight-symbol)

;; '("yellow" "DeepPink" "cyan" "MediumPurple1" "SpringGreen1"
;;   "DarkOrange" "HotPink1" "RoyalBlue1" "OliveDrab")

(defcustom highlight-symbol-use-last t
  "Use the last active symbol if no symbol found during highlight-symbol."
  :type 'boolean
  :group 'highlight-symbol)

(defconst highlight-symbol-border-pattern
  (if (>= emacs-major-version 22)
      '("\\_<" . "\\_>")
    '("\\<" . "\\>")))

(defcustom highlight-symbol-foreground-color "black"
  "*Foreground color of highlighted symbols."
  :type '(choice color
                 (const :tag "Keep original text color" nil))
  :group 'highlight-symbol)

(defcustom highlight-symbol-print-occurrence-count t
  "If non-nil, message the number of occurrences of the current symbol.
Otherwise, don't message the number of occurrences."
  :type '(choice
          (const :tag "Don't message occurrences count" nil)
          (const :tag "Always message occurrences count" t))
  :group 'highlight-symbol)

;;; internal variables

(defvar highlight-symbol--active nil
  "The current active symbol.")
(make-variable-buffer-local 'highlight-symbol--active)

(defvar highlight-symbol--last nil
  "The last active symbol.")
(make-variable-buffer-local 'highlight-symbol--last)

(defun highlight-symbol--at-point (&optional symbol)
  "Toggle highlighting of the SYMBOL at point.
This highlights or unhighlights the symbol at point using the first
element in of `highlight-symbol-faces'.
It returns t when highlighting is successful."
  (let ((symbol (or symbol
                    (highlight-symbol--get-symbol)
                    (and highlight-symbol-use-last highlight-symbol--last)
                    (user-error "No symbol at point"))))
    (if (highlight-symbol--symbol-highlighted-p symbol)
        ;; Unhighlight-symbol
        (progn (highlight-symbol--remove-symbol symbol)
               nil)
      ;; Change highlight-symbol
      (highlight-symbol-remove)
      (highlight-symbol--highlight-symbol symbol)
      t)))

;;;###autoload
(defun highlight-symbol (&optional symbol)
  "Interactive interface for `highlight-symbol--at-point'.
SYMBOL can be optionally passed in."
  (interactive)
  (when (highlight-symbol--at-point symbol)
    ;; navigation map
    (highlight-symbol--transient-navigation
     #'highlight-symbol-next
     #'highlight-symbol-prev)))

;;;###autoload
(defun highlight-symbol-last ()
  "Highlight the last active symbol."
  (interactive)
  (highlight-symbol highlight-symbol--last)
  (highlight-symbol-next))

;;;###autoload
(defun highlight-symbol-defun (&optional symbol)
  "Toggle highlighting of the SYMBOL at point.
The range is limited to the defun region."
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (highlight-symbol-at-point symbol))
  (highlight-symbol--transient-navigation
   #'highlight-symbol-next-in-defun
   #'highlight-symbol-prev-in-defun))

(defun highlight-symbol-query-replace-at-point ()
  "The at point version of `highlight-symbol-query-replace'."
  (interactive)
  (let ((symbol (or (highlight-symbol--get-symbol)
                    (error "No symbol at point"))))
    (if (highlight-symbol--symbol-highlighted-p symbol)
        (call-interactively 'highlight-symbol-query-replace)
      (highlight-symbol--highlight-symbol symbol)
      (call-interactively 'highlight-symbol-query-replace)
      (highlight-symbol-remove))))

(defun highlight-symbol--transient-navigation (next-fn prev-fn)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
	 (define-key map "n" next-fn)
	 (define-key map "N" prev-fn)
	 (define-key map "p" prev-fn)
	 ;; (define-key map "q" #'highlight-symbol-query-replace)
	 map)
   t
   #'highlight-symbol-remove))

(defun highlight-symbol--symbol-highlighted-p (symbol)
  "Test if the a SYMBOL regexp is currently highlighted."
  (eq symbol highlight-symbol--active))

(defun highlight-symbol--get-highlight-face ()
  (cond ((facep highlight-symbol-color) highlight-symbol-color)
        ((stringp highlight-symbol-color) highlight-symbol-color)
        ((functionp highlight-symbol-color) (funcall highlight-symbol-color))
        ((listp highlight-symbol-color)
         (nth (random (length highlight-symbol-color))
              highlight-symbol-color))
        ('else (error "unexpected type highlight-symbol-color %s"
                      highlight-symbol-color))))

(defun highlight-symbol--highlight-symbol (symbol)
  "The internal highlight function for SYMBOL.
That does the actual highlighting."
  (unless (highlight-symbol--symbol-highlighted-p symbol)
    (let ((color (highlight-symbol--get-highlight-face)))
      (when (and (not (facep color))
                 (not (listp color)))
        ;; fallback
        (setq color `(:background-color
                      ,color
                      :foreground-color ,highlight-symbol-foreground-color
                      :underline t
                      :inherit hl-line)))
      (message "%s" color)
      ;; highlight
      (hightlight-symbol--hightlight-symbol-with-face symbol color)
      (setq highlight-symbol--active symbol
            highlight-symbol--last symbol)
      ;; report count
      (when highlight-symbol-print-occurrence-count
        (highlight-symbol-count symbol)))))

(defun hightlight-symbol--hightlight-symbol-with-face (symbol face)
  (font-lock-add-keywords nil `((,symbol 0 ',face prepend)) 'append)
  (font-lock-fontify-buffer))

(defun highlight-symbol--remove-symbol (symbol)
  (setq highlight-symbol--active nil)
  (let ((keywords (assoc symbol (highlight-symbol--uncompiled-keywords))))
    (font-lock-remove-keywords nil (list keywords))
    (font-lock-fontify-buffer)))

(defun highlight-symbol--uncompiled-keywords ()
  (if (eq t (car font-lock-keywords))
      (cadr font-lock-keywords)
    font-lock-keywords))

;;;###autoload
(defun highlight-symbol-remove ()
  "Remove symbol highlighting in buffer."
  (interactive)
  (highlight-symbol--remove-symbol highlight-symbol--active))

;;;###autoload
(defun highlight-symbol-count (&optional symbol)
  "Print the number of occurrences of SYMBOL at point."
  (interactive)
  (message "%d occurrences in buffer"
           (let ((case-fold-search nil))
             (how-many (or symbol
                           (highlight-symbol--get-symbol)
                           (error "No symbol at point"))
                       (point-min) (point-max)))))

;;;###autoload
(defun highlight-symbol-next ()
  "Jump to the next location of the symbol at point within the buffer."
  (interactive)
  (highlight-symbol--jump 1))

;;;###autoload
(defun highlight-symbol-prev ()
  "Jump to the previous location of the symbol at point within the buffer."
  (interactive)
  (highlight-symbol--jump -1))

;;;###autoload
(defun highlight-symbol-next-in-defun ()
  "Jump to the next location of the symbol at point within the defun."
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (highlight-symbol--jump 1)))

;;;###autoload
(defun highlight-symbol-prev-in-defun ()
  "Jump to the previous location of the symbol at point within the defun."
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (highlight-symbol--jump -1)))

;;;###autoload
(defun highlight-symbol-query-replace (replacement)
  "Replace the symbol at point with REPLACEMENT.
TODO: can consider using `highlight-symbol--active-symbol'."
  (interactive (let ((symbol (or (thing-at-point 'symbol)
                                 (error "No symbol at point"))))
                 (set query-replace-to-history-variable
                      (cons (substring-no-properties symbol)
                            (eval query-replace-to-history-variable)))
                 (list
                  (read-from-minibuffer "Replacement: " nil nil nil
                                        query-replace-to-history-variable))))
  (goto-char (beginning-of-thing 'symbol))
  ;; (if (boundp 'vr/query-replace)
  ;;     (call-interactively 'vr/query-replace))
  (query-replace-regexp (highlight-symbol--get-symbol) replacement))

;;;###autoload
(defun highlight-symbol-occur (&optional nlines)
  "Call `occur' with the symbol at point.
Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative."
  (interactive "P")
  (or (thing-at-point 'symbol)
      (error "No symbol at point"))
  (occur (highlight-symbol--get-symbol) nlines))

(defun highlight-symbol--get-symbol ()
  "Return a regular expression identifying the symbol at point."
  (or highlight-symbol--active
      (let ((symbol (thing-at-point 'symbol)))
        (when symbol
          (concat (car highlight-symbol-border-pattern)
                  (regexp-quote symbol)
                  (cdr highlight-symbol-border-pattern))))))

(defun highlight-symbol--jump (dir)
  "Jump to the next or previous occurence of the symbol at point.
DIR has to be 1 or -1."
  (let ((symbol (or (highlight-symbol--get-symbol)
                    (error "No symbol at point")))
        (case-fold-search nil)
        (bounds (bounds-of-thing-at-point 'symbol)))
    (let* ((offset
            (if bounds
                (- (point) (if (< 0 dir) (cdr bounds) (car bounds)))
              0))
           (point
            (save-excursion             ; in case symbol not found
              (unless (eq last-command 'highlight-symbol--jump)
                (push-mark))
              ;; move a little, so we don't find the same instance again
              (goto-char (- (point) offset))
              (let ((target
                     (or (re-search-forward symbol nil 'noerror dir)
                         (progn ;; expand search region
                           (goto-char (if (< 0 dir) (point-min) (point-max)))
                           ;; (message "Continued from beginning of buffer")
                           (re-search-forward symbol nil 'noerror dir)))))
                (and target (+ target offset))))))
      (if point
          (goto-char point)
        (message "Keyword %s not found" symbol)))
    (setq this-command 'highlight-symbol--jump)))

(provide 'highlight-symbol)

;;; highlight-symbol.el ends here
