;;; simple-modeline-segments.el --- The segments for simple-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021  Eder Elorriaga

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

;; The segments for simple-modeline

;;; Code:

(require 'subr-x)

(defun simple-modeline-segment-winum ()
  "Winum modeline segment setup."
  (propertize (winum-get-number-string)
              'face 'simple-modeline-important))

(defvar simple-modeline-modified-status-plist
  '(read-only "禁" modified "写" default "梁"))

(defun simple-modeline--buffer-status ()
  "Find buffer modified status."
  (if (and buffer-read-only (buffer-file-name))
      'read-only
    (if (and buffer-file-name (buffer-modified-p))
        'modified
      'default)))

(defun simple-modeline-segment-modified ()
  "Displays a color-coded buffer modification/read-only indicator in the mode-line."
  ;; (if (not (string-match-p "\\*.*\\*" (buffer-name))))
  (let ((status (simple-modeline--buffer-status)))
    (propertize
     (plist-get simple-modeline-modified-status-plist status)
     'face `(;; :height 1.1
             :inherit
             ,(cl-case status
                (read-only 'simple-modeline-status-error)
                (modified 'simple-modeline-status-modified)
                (t        'simple-modeline-unimportant)))
     'display '(raise 0.1)
     'help-echo (format
                 "Buffer is `%s'\nmouse-1: Toggle read-only status."
                 status)
     'local-map '(keymap
                  (mode-line
                   keymap
                   (mouse-1 . mode-line-toggle-read-only)))
     'mouse-face 'mode-line-highlight)))

(defun simple-modeline-to-text (state)
  (cl-case state
    (insert "恶")
    ((normal vi) "常")
    (visual "选")
    (operator "动")
    (motion "水")
    (emacs  "本")
    (replace "换")
    (t state)))

(defun simple-modeline-segment-evil-modal ()
  "Displays a color-coded evil state modal indicator in the mode-line."
  (let ((state (if (bound-and-true-p viper-mode)
				   (intern
					(string-trim-right (symbol-name viper-current-state)
									   "-state"))
				 (if (bound-and-true-p evil-mode)
					 evil-state
				   (if (bound-and-true-p meow-global-mode)
					   meow--current-state
					 "E")))))
    (propertize
     (or (simple-modeline-to-text state) ; (evil-state-property state :tag)
         (symbol-name state))
     'face (cl-case state
             (insert 'simple-modeline-status-modified)
             (normal 'simple-modeline-unimportant)
             (emacs  'simple-modeline-status-error)
             (t 'simple-modeline-important))
     ;; (list :inherit :height 1.2)
     'display '(raise -0.1))))

(defun simple-modeline-segment-buffer-name ()
  "Displays the name of the current buffer in the mode-line."
  ;; mode-line-buffer-identification
  (propertize " %b " 'face 'mode-line-buffer-id))

(defun simple-modeline-segment-position ()
  "Displays the current cursor position in the mode-line."
  `((size-indication-mode " %p%% ")
    (line-number-mode
     (column-number-mode
      (column-number-indicator-zero-based "%l:%c" "%l:%C")
      "L%l")
     (column-number-mode
      (column-number-indicator-zero-based (4 "%cC") (4 "%CC"))))
    (mark-active                        ; (region-active-p)
     (:eval
      (propertize
       (let ((beg (region-beginning))
             (end (region-end)))
         (format " +%dlines"
                 (1+ (- (line-number-at-pos end) (line-number-at-pos beg)))))
       'font-lock-face 'font-lock-variable-name-face)))))

(defun simple-modeline-segment-vc ()
  "Displays color-coded version control information in the mode-line."
  ;; (format-mode-line '(vc-mode vc-mode))
  (when vc-mode
    (let* ((backend (vc-backend buffer-file-name))
           (state (vc-state buffer-file-name backend))
           (str                         ; (if vc-display-status "")
            ;; (cadr (split-string vc-mode ":"))
            (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))))
      (propertize str
                  ;; (if (> (length str) 15)
                  ;;     (concat
                  ;;      (substring str 0 (- 15 3))
                  ;;      "...")
                  ;;   str)
                  'face (cond ((eq state 'needs-update)
                               'simple-modeline-status-warning)
                              ((memq state '(removed conflict unregistered))
                               'simple-modeline-important)
                              (t 'simple-modeline-status-info))))))

(defun simple-modeline-segment-encoding ()
  "Displays the encoding style of the buffer in the mode-line."
  (unless (or (eq buffer-file-coding-system 'utf-8-unix)
              (eq buffer-file-coding-system 'utf-8))
    (propertize
     (let ((sys (coding-system-plist buffer-file-coding-system)))
       ;; (if (memq (plist-get sys :category)
       ;;           '(coding-category-undecided coding-category-utf-8))
       ;;     "UTF-8")
       (concat " "
               (upcase (symbol-name (plist-get sys :name)))))
     'help-echo 'mode-line-mule-info-help-echo
     'mouse-face 'mode-line-highlight
     'local-map mode-line-coding-system-map)))

(defun simple-modeline-segment-eol ()
  "Displays the EOL style of the current buffer in the mode-line."
  (let* ((eol (coding-system-eol-type buffer-file-coding-system))
         (mnemonic (pcase eol
                     ('0 " LF")
                     ('1 " CRLF")
                     ('2 " CR")
                     (_ "")))
         (desc (pcase eol
                 ('0 "Unix-style")
                 ('1 "DOS-style")
                 ('2 "Mac-style")
                 (_ "Undecided"))))
    (propertize
     mnemonic
     'help-echo (format "End-of-line style: %s\nmouse-1: Cycle" desc)
     'local-map (let ((map (make-sparse-keymap)))
	              (define-key map [mode-line mouse-1] 'mode-line-change-eol)
	              map)
     'mouse-face 'mode-line-highlight)))

(defun simple-modeline-segment-misc-info ()
  "Displays the current value of `mode-line-misc-info' in the mode-line."
  (let ((misc-info 
		 (format-mode-line mode-line-misc-info 'simple-modeline-unimportant)))
    (unless (string= misc-info "")
      (concat " " (string-trim misc-info)))))

(defun simple-modeline-segment-input-method ()
  "Displays the input-method of the buffer in the mode-line."
  `(current-input-method
    (:propertize
     current-input-method-title
     help-echo (format
                "Current input method: %s\nmouse-1: Describe current input method"
                current-input-method)
     local-map (keymap
                (mode-line
                 keymap
                 (mouse-1 .
                          ,(lambda (e)
                             (interactive "e")
                             (with-selected-window (posn-window
                                                    (event-start e))
                               (describe-current-input-method))))))
     mouse-face 'mode-line-highlight)))

(defun simple-modeline-segment-minor-modes ()
  "Displays the current minor modes in the mode-line."
  minor-mode-alist
  ;; (replace-regexp-in-string
  ;;  "%" "%%%%"
  ;;  (format-mode-line minor-mode-alist)
  ;;  t t)
  )

(defun simple-modeline-segment-process ()
  "Displays the current value of `mode-line-process' in the mode-line."
  (when mode-line-process
    (string-trim (format-mode-line mode-line-process))))

(defun simple-modeline-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (propertize (format-mode-line mode-name) 'face 'bold))

(defcustom simple-modeline-word-count-modes '(markdown-mode gfm-mode org-mode)
  "Major modes in which to display word count continuously."
  :type '(repeat (symbol :tag "Major-Mode"))
  :group 'simple-modeline)

(defun simple-modeline-segment-word-count ()
  "Display the buffer word count in the mode-line when in a major mode in `simple-modeline-word-count-modes'."
  (if (member major-mode simple-modeline-word-count-modes)
      (format " %dW" (count-words (point-min) (point-max)))))

(provide 'simple-modeline-segments)
;;; simple-modeline-segments.el ends here
