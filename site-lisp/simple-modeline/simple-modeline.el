;;; simple-modeline.el --- A simple mode-line configuration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021  Eder Elorriaga

;; Author: Eder Elorriaga <gexplorer8@gmail.com>
;; URL: https://github.com/gexplorer/simple-modeline
;; Keywords: mode-line faces
;; Version: 1.4
;; Package-Requires: ((emacs "26.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple mode-line configuration for Emacs.
;; To enable, put this code in your init file:
;; (require 'simple-modeline)
;; (simple-modeline-mode 1)
;; or
;; (use-package simple-modeline
;;   :ensure t
;;   :hook (after-init . simple-modeline-mode))
;;

;;; Code:

(require 'simple-modeline-segments)

(defgroup simple-modeline nil
  "A simple mode line."
  :prefix "simple-modeline-"
  :group 'mode-line)

(defvar simple-modeline--mode-line
  '((:eval
     (simple-modeline--format
      (car simple-modeline-segments)
      (cadr simple-modeline-segments))))
  "The default `simple-modeline' setup.")

;;;###autoload
(define-minor-mode simple-modeline-mode
  "Minor mode to get a simple mode line.

When called interactively, toggle
`simple-modeline-mode'.  With prefix ARG, enable
`simple-modeline--mode' if ARG is positive, otherwise
disable it.

When called from Lisp, enable `simple-modeline-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `simple-modeline-mode'.
Otherwise behave as if called interactively."
  :init-value nil
  :keymap nil
  :lighter ""
  :group 'simple-modeline
  :global t
  (if simple-modeline-mode
      ;; Set the new mode-line-format
      (progn
        (setq-default mode-line-format simple-modeline--mode-line)
        ;; (if simple-modeline-bottom
        ;;   (setq-default header-line-format '(:eval simple-modeline--mode-line))
        ;;   (setq-default mode-line-format nil))
        (simple-modeline--update-modeline))
    ;; Restore the original mode-line format
    (let ((symbl 'mode-line-format))
      (set symbl (eval (car (get symbl 'standard-value)))))))


;;
;; Options
;;

(defcustom simple-modeline-segments
  '((simple-modeline-segment-winum
     simple-modeline-segment-modified
     simple-modeline-segment-buffer-name
     simple-modeline-segment-position)
    (simple-modeline-segment-minor-modes
     simple-modeline-segment-input-method
     simple-modeline-segment-eol
     simple-modeline-segment-encoding
     simple-modeline-segment-vc
     simple-modeline-segment-misc-info
     simple-modeline-segment-process
     simple-modeline-segment-major-mode))
  "Simple modeline segments."
  :type '(list (repeat :tag "Left aligned" function)
               (repeat :tag "Right aligned" function))
  :package-version '(simple-modeline . "1.2"))

(make-variable-buffer-local 'simple-modeline-segments)
(put 'simple-modeline-segments 'safe-local-variable 'consp)

(defun simple-modeline-segment (x)
  "Convert symbol X to a `simple-modeline-segment' on."
  (intern (concat "simple-modeline-segment-"
                  (symbol-name x))))

(defun simple-modeline-get-segments (segments)
  "Setter for `simple-modeline-segments' with SEGMENTS."
  (cl-labels
      ((get-symbol (x)
                   (if (symbolp x)
                       (let ((mode-line-sym (simple-modeline-segment x)))
                         (cond
                          ((fboundp mode-line-sym)
                           `(:eval (,mode-line-sym)))
                          ((fboundp x)
                           `(:eval (,x)))
                          (t  ;; (error "Segment function doesn't exist %s" x)
                           x)))
                     (if (or (listp x) (stringp x))
                         x
                       (error "Nope %s" x)))))
    (mapcar (lambda (segment) (mapcar #'get-symbol segment))
            segments)))

;; TODO: add this into modeline-format
(defcustom simple-modeline-box-height 5
  "Simple modeline box height."
  :type 'number
  :package-version '(simple-modeline . "1.3"))

(defcustom simple-modeline-face-attributes '(:height 120)
  "Simple modeline face attributes."
  :type 'list
  :package-version '(simple-modeline . "1.3"))

(defcustom simple-modeline-bottom t
  "Simple modeline position, non-nil means bottom otherwise top."
  :type 'symbol
  :package-version '(simple-modeline . "1.3"))

(defun simple-modeline--update-modeline ()
  "Update face attributes of the whole modeline."
  (dolist (face '(mode-line mode-line-inactive))
    (apply 'set-face-attribute face nil simple-modeline-face-attributes)
    (set-face-attribute face nil
                        :box `(:line-width ,simple-modeline-box-height
                                           ;; ,@attrs
                                           :color ,(face-background face)))))

;;
;; Faces
;;

(defface simple-modeline-space
  '((t))
  "Face for space used to alight the right segments in the mode-line.")

(defface simple-modeline-important
  '((t (:inherit (mode-line-emphasis bold))))
  "Face for less important mode-line elements.")

(defface simple-modeline-unimportant
  '((t (:inherit (font-lock-type-face))))
  "Face for less important mode-line elements.")

(defface simple-modeline-status-modified
  '((t (:inherit (warning bold))))
  "Face for the 'modified' indicator symbol in the mode-line.")

(defface simple-modeline-status-info
  '((t (:inherit (font-lock-doc-face))))
  "Face for generic status indicators in the mode-line.")

(defface simple-modeline-status-success
  '((t (:inherit (success))))
  "Face used for success status indicators in the mode-line.")

(defface simple-modeline-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the mode-line.")

(defface simple-modeline-status-error
  '((t (:inherit (error))))
  "Face for error status indicators in the mode-line.")

;;
;; Helpers
;;

(defun simple-modeline--format (left-segments right-segments)
  "Return a string of `window-width' length containing LEFT-SEGMENTS and RIGHT-SEGMENTS, aligned respectively."
  (let* ((right (format-mode-line right-segments))
         (reserve (length right)))
    (concat
     (format-mode-line left-segments)
     (propertize " "
                 'display `((space :align-to (- right ,reserve 1)))
                 'face '(:inherit simple-modeline-space))
     right)))


(provide 'simple-modeline)
;;; simple-modeline.el ends here
