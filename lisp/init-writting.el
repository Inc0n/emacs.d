;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; themes also some selectrum specific theme hacks

;;; Code:

;;; mixed-pitch

;; (local-require 'mixed-pitch)
(autoload #'mixed-pitch-mode "mixed-pitch")
(autoload #'mixed-pitch-serif-mode "mixed-pitch"
  "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch." t)

(with-eval-after-load 'mixed-pitch
  (defface variable-pitch-serif
    '((t (:family "Merriweather")))
    "A variable-pitch face with serifs."
    :group 'basic-faces)
  ;; (set-face-attribute
  ;;  'variable-pitch-serif nil :family "STFangSong") ; "Merriweather"
  (setq mixed-pitch-set-height t))

(setq text-scale-mode-step 1.1)
;; (text-scale-set 0)

(setq-default fill-column 75			; my fonts are bigger
              visual-fill-column-center-text t
              ;; Additional margin needed for Unicode text width
              visual-fill-column-extra-text-width '(0 . -6))

;;; writeroom

(use-package writeroom-mode :ensure t
  :config
  (add-to-list/s 'writeroom--local-variables
                 '(mixed-pitch-mode
                   ;; org-indent-mode
                   ;; org-adapt-indentation
                   display-line-numbers-mode))
  (setq writeroom-width 0.7
        ;; writetoom-
        writeroom-window-maximized nil
        writeroom-fullscreen-effect 'maximized
        writeroom-extra-line-spacing nil
        writeroom-mode-line t))

(defun readroom-mode (&optional arg)
  (interactive)
  (let ((writeroom-width 0.5)
	(writeroom-extra-line-spacing 5))
    ;; (mixed-pitch-serif-mode 1)
    (call-interactively 'writeroom-mode)))

;; Small read room mode

(define-minor-mode center-buffer-mode
  "Minor mode to center the buffer within window.

When called interactively, toggle
`center-buffer-mode'.  With prefix ARG, enable
`center-buffer-mode' if ARG is positive, otherwise
disable it.

When called from Lisp, enable `center-buffer-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `center-buffer-mode'.
Otherwise behave as if called interactively."
  :init-value nil
  ;; :keymap nil
  ;; :lighter ""
  :group 'center-buffer-room
  :global nil
  (if center-buffer-mode
      (cl-flet ((resize-margin ()
                  (let ((margin-size
                         (/ (- (frame-width) fill-column) 2)))
                    (message "margin-size: %d" margin-size)
                    (if (> margin-size 0)
			(set-window-margins (selected-window) margin-size margin-size)))))
        (add-hook 'window-configuration-change-hook #'resize-margin 0 'local)
        (resize-margin))
    (remove-hook 'window-configuration-change-hook #'resize-margin 'local)
    (set-window-margins (selected-window) nil)))

(defun reading-experience-on ()
  (interactive)
  (cl-flet ((resize-margin ()
                           (let ((margin-size
                                  (/ (- (frame-width) fill-column) 2)))
                             (set-window-margins (selected-window) margin-size margin-size))))
    (add-hook 'window-configuration-change-hook #'resize-margin 0 'local)
    (resize-margin)))

;;;

(defun chinese-txt-novel-imenu ()
  (interactive)
  (let ((imenu-generic-expression
         `(("Chapters"
            ,(rx
              line-start (* space)
              (group "第"
                     (or (+ numeric)
                         (+ (any "一二三四五六七八九十")))
                     "章")
              (* any) line-end)
            1))))
    ;; (string-match-p chinese-chapter-expression "第10章")
    (consult-imenu)))

;;;

(defvar line-spacing-step 1)
(defvar line-spacing--orig nil)

(defun line-spacing-adjust (inc)
  "Adjust line spacing with INC."
  (interactive (list line-spacing-step))
  (let ((ev last-command-event)
	    (echo-keystrokes nil))
    (let* ((base (event-basic-type ev))
           (step
            (pcase base
              ((or ?+ ?=) inc)
              (?- (- inc))
              (?0 0)
              (_ inc))))
      (unless line-spacing--orig
        (setq-local line-spacing--orig line-spacing))
      (setq-local line-spacing (if (= step 0)
                                   (prog1 line-spacing--orig
                                     (setq-local line-spacing--orig nil))
                                 (+ line-spacing step)))
      (message "Use +,-,0 for further adjustment (spacing=%d)" line-spacing)
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (dolist (mods '(() (control)))
           (dolist (key '(?- ?+ ?= ?0)) ;; = is often unshifted +.
             (define-key map (vector (append mods (list key)))
               (lambda () (interactive) (line-spacing-adjust (abs inc))))))
         map)))))

;;; Filling

(defun operate-on-each-line (min max op)
  "OP is a function that operates on each line in region between MIN and MAX.
If OP returns 'forward-line, to forward 1 line."
  (let ((buffer-read-only nil)
        (maxp
         (if (= max (point-max))
             (lambda (_p) (eobp))
           (lambda (p) (> p max)))))
    (goto-char min)
    ;; (fill-region min max 'left 'nosqueeze)
    (while (not (funcall maxp (point)))
      (if (looking-at "^[[:space:]]*$")
          (forward-line 1)
        (when (eq (funcall op) 'forward-line)
          (forward-line 1))))))

(defun ensure-chinese-fill-paragraph (arg min max)
  "Fill buffer line by line from MIN to MAX.
Argument ARG add two space prefix to paragraph."
  (interactive (cons current-prefix-arg
                     (region-or-min-max-points)))
  (let ((fill-column 75))
    (operate-on-each-line
     min max
     (lambda ()
       (if (eq (org-element-type (org-element-at-point))
               'paragraph)
           (progn
             ;; (when arg)
             (unless (looking-at "^[[:space:]]*　　")
               (beginning-of-line-text)
               (insert "　　"))
             (fill-region-as-paragraph
              (line-beginning-position)
              (line-end-position)
              'left)
             'dont-forward)
         'forward-line)))))

(defun unfill-chinese (min max)
  "Fill buffer line by line from MIN to MAX.
Argument ARG add two space prefix to paragraph."
  (interactive (region-or-min-max-points))
  (let ((fill-column 75)
        (pos nil))
    (operate-on-each-line
     min max
     (lambda ()
       (if (eq (org-element-type (org-element-at-point))
               'paragraph)
           (if (looking-at "^　　")
               (progn
                 (if pos
                     (unfill-region pos (line-beginning-position)))
                 (setq pos (line-beginning-position))
                 'forward-line)
             'forward-line)
         'forward-line)))))

;;; writing

;; @see http://endlessparentheses.com/super-smart-capitalization.html
(defun endless/convert-punctuation (rg rp)
  "Look for regexp RG around point, and replace with RP.
Only applies to `text-mode'."
  (let ((f "\\(%s\\)\\(%s\\)")
        (space "?:[[:blank:]\n\r]*"))
    ;; We obviously don't want to do this in prog-mode.
    (when (and (derived-mode-p 'text-mode)
               (or (looking-at (format f space rg))
                   (looking-back (format f rg space))))
      (replace-match rp nil nil nil 1))))

(defun endless/capitalize ()
  "Capitalise region or word.
Also converts commas to full stops, and kills
extraneous space at beginning of line."
  (interactive)
  (endless/convert-punctuation "," ".")
  (if (use-region-p)
      (call-interactively 'capitalize-region)
    ;; A single space at the start of a line:
    (when (looking-at "^\\s-\\b")
      ;; get rid of it!
      (delete-char 1))
    (call-interactively 'subword-capitalize)))

(defun endless/downcase ()
  "Down-case region or word.
Also converts full stops to commas."
  (interactive)
  (endless/convert-punctuation "\\." ",")
  (if (use-region-p)
      (call-interactively 'downcase-region)
    (call-interactively 'subword-downcase)))

(defun endless/upcase ()
  "Upcase region or word."
  (interactive)
  (if (use-region-p)
      (call-interactively 'upcase-region)
    (call-interactively 'subword-upcase)))

;; these bindings are fine
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-l") 'endless/downcase)
(global-set-key (kbd "M-u") 'endless/upcase)

;;; comments

(defun comment-or-uncomment-dwim (beg end)
  "Comment or uncomment out line [region] from BEG to END."
  (interactive (region-or-line-beg-end-points current-prefix-arg))
  (if (comment-only-p (max (line-beginning-position) beg)
                      (min (line-end-position) end))
      (while (and (not (looking-at "^[[:space:]]*$"))
                  (comment-only-p
                   (line-beginning-position)
                   (line-end-position)))
        (uncomment-region (max (line-beginning-position) beg)
                          (line-end-position))
        (forward-line 1))
    (comment-or-uncomment-region beg end)
    (goto-char end)
    (forward-line 1)
    (indent-according-to-mode)))

(defun copy-and-paste (beg end)
  "Copy region BEG to END and paste it right below."
  (interactive (region-or-line-beg-end-points))
  (goto-char end)
  (let ((str (buffer-substring beg end)))
    (unless (s-suffix? "\n" str)
      (insert ?\n))
    (insert str))
  (indent-according-to-mode))

(defun backward-delete-word (start end)
  "Delete word or region from START to END.
This won't push deletion to `kill-ring'."
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list (point)
                       (save-excursion
                         (backward-word current-prefix-arg)
                         (point)))))
  (delete-region start end))

(provide 'init-writting)
