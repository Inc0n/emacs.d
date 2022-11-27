;;; visual-regexp.el --- A regexp/replace command for Emacs with interactive visual feedback

;; Copyright (C) 2013-2019 Marko Bencun

;; Author: Marko Bencun <mbencun@gmail.com>
;; URL: https://github.com/benma/visual-regexp.el/
;; Version: 1.1
;; Package-Requires: ((cl-lib "0.2"))
;; Keywords: regexp, replace, visual, feedback

;; This file is part of visual-regexp.

;; visual-regexp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; visual-regexp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with visual-regexp.  If not, see <http://www.gnu.org/licenses/>.

;;; WHAT'S NEW
;; 1.1: Add new customization: vr/plain
;; 1.0: Add support for one prompt for search/replace, using query-replace-from-to-separator
;;      (query-replace history like in Emacs 25).
;;      Breaking changes:
;;       - vr/minibuffer-(regexp|replace)-keymap have been collapsed to vr/minibuffer-keymap
;;       - vr/minibuffer-help-(regexp|replace) have been replaced by vr--minibuffer-help-text
;; 0.9: Fix warnings regarding free variables.
;; 0.8: Error handling for vr--get-regexp-string. Bug-fixes regarding error display.
;; 0.7: Customizable separator (arrow) string and face.
;; 0.6: distinguish prompts in vr/replace, vr/query-replace, vr/mc-mark.
;; 0.5: emulate case-conversion of replace-regexp.
;; 0.4: vr/mc-mark: interface to multiple-cursors.
;; 0.3: use the same history as the regular Emacs replace commands;
;; 0.2: support for lisp expressions in the replace string, same as in (query-)replace-regexp
;; 0.1: initial release

;;; Tip Jar
;; If you found this useful, please consider donating.
;; BTC: 1BxauiLGMQPb2pavkkQkuFe5CgrGMrUat2

;;; What's This?

;; visual-regexp for Emacs is like `replace-regexp`, but with live  visual feedback directly in the buffer.
;; While constructing the regexp in the minibuffer, you get live visual feedback for the matches, including group matches.
;; While constructing the replacement in the minibuffer, you get live visual feedback for the replacements.
;; It can be used to replace all matches in one go (like `replace-regexp`), or a decision can be made on each match (like `query-replace-regexp`).
;; Thanks to Detlev Zundel for his re-builder.

;;; Where does visual-regexp come from?
;;
;; I was not happy with the way I used emacs' replace-regexp before. Constructing the regular expression is error prone and emacs' regular expressions are limited
;; (for example, no lookaheads, named groups, etc.).
;; Using re-builder to interactively build regular expressions was a step into the right direction, but manually copying over the regexp
;; to the minibuffer is cumbersome.
;; Using the idea of interactive feedback of re-builder, this package makes it possible to use just the minibuffer to construct (with live visual feedback) the regexp and replacement,
;; using Emacs style regular expressions, or optionally, regular expressions powered by other (mode modern) engines, for the replacement. For the latter part, see the package visual-regexp-steroids.

;;; Installation

;; If you are using Emacs 24, you can get visual-regexp from [melpa](https://melpa.org/) with the package manager.
;; Add the following code to your init file. Of course you can select your own key bindings.
;; ----------------------------------------------------------
;; (add-to-list 'load-path "folder-in-which-visual-regexp-files-are-in/") ;; if the files are not already in the load path
;; (require 'visual-regexp)
;; (define-key global-map (kbd "C-c r") 'vr/replace)
;; (define-key global-map (kbd "C-c q") 'vr/query-replace)
;; ;; if you use multiple-cursors, this is for you:
;; (define-key global-map (kbd "C-c m") 'vr/mc-mark)
;; ----------------------------------------------------------
;; To customize, use `M-x customize-group [RET] visual-regexp`.

;;; Code:
(unless (fboundp 'make-overlay)
  (require 'overlay))

;; cl is used for the (loop ...) macro
(require 'cl-lib)

;;; faces

(defface vr/match-separator-face
  '((((class color))
     :foreground "red"
     :bold t)
    (t
     :inverse-video t))
  "Face for the arrow between match and replacement.
To use this, you must activate `vr/match-separator-use-custom-face'."
  :group 'visual-regexp)

(defcustom vr/match-separator-string
  (progn
    (custom-reevaluate-setting 'query-replace-from-to-separator)
    (substring-no-properties query-replace-from-to-separator))
  "String used to separate a match from the replacement during feedback."
  :type 'sexp
  :initialize 'custom-initialize-delay
  :group 'visual-regexp)

(defface vr/match-0
  '((t
	 :inherit error
	 :strike-through t
	 ;; :bold nil
	 ))
  "First face for displaying a whole match."
  :group 'visual-regexp)

(defface vr/match-1
  '((((class color) (background light))
     :background "pale turquoise")
    (((class color) (background dark))
     :background "dodgerblue4")
    (t
     :inverse-video t))
  "Second face for displaying a whole match."
  :group 'visual-regexp)

(defface vr/group-0
  '((((class color) (background light))
     :background "aquamarine")
    (((class color) (background dark))
     :background "blue3")
    (t
     :inverse-video t))
  "First face for displaying a matching group."
  :group 'visual-regexp)

(defface vr/group-1
  '((((class color) (background light))
     :background "springgreen")
    (((class color) (background dark))
     :background "chartreuse4")
    (t
     :inverse-video t))
  "Second face for displaying a matching group."
  :group 'visual-regexp)

(defface vr/group-2
  '((((min-colors 88) (class color) (background light))
     :background "yellow1")
    (((class color) (background light))
     :background "yellow")
    (((class color) (background dark))
     :background "sienna4")
    (t
     :inverse-video t))
  "Third face for displaying a matching group."
  :group 'visual-regexp)

;;; variables

(defcustom vr/auto-show-help t
  "Show help message automatically when the minibuffer is entered."
  :type 'boolean
  :group 'visual-regexp)

(defcustom vr/default-feedback-limit 50
  "Limit number of matches shown in visual feedback.
If nil, don't limit the number of matches shown in visual feedback."
  :type 'integer
  :group 'visual-regexp)

(defcustom vr/query-replace-from-history-variable query-replace-from-history-variable
  "History list to use for the FROM argument.
The default is to use the same history as Emacs' `query-replace' commands."
  :type 'symbol
  :group 'visual-regexp)

(defcustom vr/query-replace-to-history-variable query-replace-to-history-variable
  "History list to use for the TO argument.
The default is to use the same history as Emacs' `query-replace' commands."
  :type 'symbol
  :group 'visual-regexp)

(defcustom vr/query-replace-defaults-variable
  'query-replace-defaults
  "History of search/replace pairs."
  :type 'symbol
  :group 'visual-regexp)


(defcustom vr/plain nil
  "If non-nil, use plain search/replace instead of regexp search/replace."
  :type 'boolean
  :group 'visual-regexp)

(defvar vr/initialize-hook nil
  "Hook called before `vr/replace' and `vr/query-replace'.")

;;; private variables

(defconst vr--group-faces '(vr/group-0 vr/group-1 vr/group-2)
  "Faces in list for convenience.")

(defconst vr--overlay-priority 1001
  "Starting priority of visual-regexp overlays.")

(defvar vr--in-minibuffer nil
  "Is visual-regexp currently being used?")

(defvar vr--last-minibuffer-contents nil
  "Keeping track of minibuffer changes.")

(defvar vr--target-buffer-start nil
  "Starting position in target buffer.")

(defvar vr--target-buffer-end nil
  "Ending position in target buffer.")

(defvar vr--limit-reached)

(defvar vr--regexp-string nil
  "Entered regexp.")

(defvar vr--replace-string nil
  "Entered replacement.")

(defvar vr--feedback-limit nil
  "Feedback limit currently in use.")

(defvar vr--target-buffer nil
  "Buffer to which visual-regexp is applied to.")

(defvar vr--overlays (make-hash-table :test 'eq)
  "Overlays used in target buffer.")

(defvar vr--visible-overlays (list)
  "Overlays currently visible.")

;;; keymap

(defvar vr/minibuffer-keymap
  (let ((map (make-sparse-keymap)))
	(set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-c ?") 'vr--minibuffer-help)
    (define-key map (kbd "C-c a") 'vr--shortcut-toggle-limit)
    map)
  "Keymap used while using visual-regexp.")

;;; helper functions

(defun vr--shortcut-toggle-limit ()
  "Toggle the limit of overlays shown (default limit / no limit)."
  (interactive)
  (if vr--feedback-limit
      (setq vr--feedback-limit nil)
    (setq vr--feedback-limit vr/default-feedback-limit))
  (vr--do-replace-feedback))

(defun vr--query-replace--split-string (string)
  "Copy/paste of query-replace--split-string, removing the assertion."
  (let* ((length (length string))
         (split-pos (text-property-any 0 length 'separator t string)))
    (if (not split-pos)
        (substring-no-properties string)
      (cons (substring-no-properties string 0 split-pos)
            (substring-no-properties string (1+ split-pos) length)))))

(defun vr--get-regexp-string (&optional for-display)
  (let ((split (vr--query-replace--split-string
				vr--regexp-string)))
    (if (consp split) (car split) split)))

(defun vr--get-replace-string ()
  (if (eq vr--in-minibuffer 'vr--minibuffer-replace)
      (minibuffer-contents-no-properties)
    (let ((split (vr--query-replace--split-string
				  vr--regexp-string)))
      (if (consp split) (cdr split) vr--replace-string))))

;;; minibuffer functions

(defun vr--set-minibuffer-prompt ()
  (concat "Query replace "
		  (propertize (vr--get-regexp-string t)
					  'face 'font-lock-keyword-face)
		  " with: "))

(defun vr--update-minibuffer-prompt ()
  (when (and (minibufferp) vr--in-minibuffer)
    (let ((inhibit-read-only t)
          (prompt (vr--set-minibuffer-prompt)))
      (put-text-property
	   (point-min)
	   (minibuffer-prompt-end)
	   'display prompt))))

(defun vr--minibuffer-help ()
  (interactive)
  (minibuffer-message
   (substitute-command-keys "\\<vr/minibuffer-keymap>\\[vr--minibuffer-help]: help, \\[vr--shortcut-toggle-limit]: toggle show all, \\[previous-history-element]: previous")))

;;; overlay functions

(defun vr--get-overlay (i)
  "I: match index."
  (let ((overlay (gethash i vr--overlays)))
    (unless overlay ;; create new one if overlay does not exist yet
      (setq overlay (make-overlay 0 0))
      (overlay-put overlay 'face 'vr/match-0)
      (overlay-put overlay 'priority vr--overlay-priority)
      (overlay-put overlay 'vr-ij i)
      (puthash i overlay vr--overlays))
    overlay))

(defun vr--delete-overlays ()
  "Delete all visible overlays."
  (message "test %s %s"
		   (length vr--visible-overlays)
		   (hash-table-count vr--overlays))
  (mapc #'delete-overlay vr--visible-overlays)
  (setq vr--visible-overlays '()))

(defun vr--delete-overlay-display (overlay)
  (overlay-put overlay 'display nil)
  (overlay-put overlay 'after-string nil)
  (overlay-put overlay 'priority vr--overlay-priority))

(defun vr--delete-overlay-displays ()
  "Delete the display of all visible overlays.
Call before `vr--delete-overlays'."
  (mapc #'vr--delete-overlay-display vr--visible-overlays))

;;; hooks

(defun vr--after-change (beg end len)
  (when (and vr--in-minibuffer (minibufferp))
    ;; minibuffer-up temporarily deletes minibuffer contents before inserting new one.
    ;; don't do anything then as the messages shown by visual-regexp are irritating while browsing the history.
    (unless
		(and (string-empty-p (minibuffer-contents-no-properties))
             (eq last-command 'previous-history-element))
      ;; do something when minibuffer contents changes
      (unless (eq vr--last-minibuffer-contents
				  (buffer-modified-tick))
        (setq vr--last-minibuffer-contents (buffer-modified-tick))
        ;; minibuffer contents has changed, update visual feedback.
        ;; not using after-change-hook because this hook applies to
        ;; the whole minibuffer, including minibuffer-messages that
        ;; disappear after a while.
        (vr--update-minibuffer-prompt)))
    (vr--do-replace-feedback)))

(defun vr--minibuffer-setup ()
  "Setup prompt and help when entering minibuffer."
  (when vr--in-minibuffer
    (vr--update-minibuffer-prompt)
    (when vr/auto-show-help
	  (vr--minibuffer-help))))

;;; helper functions

(defun vr--compose-messages (&rest msgs)
  (string-join
   (cl-remove-if (lambda (msg)
				   (or (null msg) (string= "" msg)))
				 msgs)
   " - "))

;;; show feedback functions

(defun vr--feedback-match-callback (i begin end)
  (with-current-buffer vr--target-buffer
    (save-excursion
      (let ((overlay (vr--get-overlay i)))
        (move-overlay overlay begin end vr--target-buffer)
		(overlay-put overlay 'face 'vr/match-0)
        (overlay-put overlay 'after-string
					 ;; empty match; indicate by a pipe
					 (and (= begin end)
            			  (propertize "|"
									  'face (if (= 0 i)
												'vr/match-0
											  'vr/match-1)
									  'help-echo "empty match")))
        (push overlay vr--visible-overlays))
      ;; mark if we have reached the specified feedback limit
      (when (and vr--feedback-limit (= vr--feedback-limit (1+ i)) )
        (setq vr--limit-reached t)))))

(defun vr--get-replacement (replacement match-data i)
  (with-current-buffer vr--target-buffer
    (let*
        ;; emulate case-conversion of (perform-replace)
        ((case-fold-search (if (and case-fold-search search-upper-case)
                               (ignore-errors
								 (isearch-no-upper-case-p
								  (vr--get-regexp-string) t))
                             case-fold-search))
         (nocasify (not (and case-replace case-fold-search))))
      ;; we need to set the match data again, s.t. match-substitute-replacement works correctly.
      ;; (match-data) could have been modified in the meantime, e.g. by vr--get-regexp-string->pcre-to-elisp.
      (set-match-data match-data)
      (if (stringp replacement)
          (match-substitute-replacement replacement nocasify vr/plain)
        (match-substitute-replacement (funcall (car replacement) (cdr replacement) i) nocasify vr/plain)))))

(defun vr--do-replace-feedback-match-callback (replacement match-data i)
  (let ((begin (cl-first match-data))
        (end (cl-second match-data))
        (replacement (vr--get-replacement replacement match-data i)))
    (let ((overlay (vr--get-overlay i)))
      (move-overlay overlay begin end vr--target-buffer)
	  ;; repaint with match-0
	  (overlay-put overlay 'face 'vr/match-0)
      (vr--delete-overlay-display overlay)
      (let ((replace
			 (propertize replacement 'face 'success)))
        (overlay-put overlay 'after-string replace)
        (overlay-put overlay 'priority vr--overlay-priority)))))

(defun vr--mapcar-nonnil (rep list)
  (mapcar (lambda (it) (when it (funcall rep it))) list))

(defun vr--get-replacements (feedback feedback-limit)
  "Get replacements using emacs-style regexp."
  (setq vr--limit-reached nil)
  (save-excursion
	(let ((regexp-string (vr--get-regexp-string))
          (replace-string (vr--get-replace-string))
          (message-line "")
          (replacements '())
          (buffer-contents
		   (with-current-buffer vr--target-buffer
			 (buffer-substring-no-properties (point-min) (point-max)))))
	  (with-current-buffer vr--target-buffer
        (goto-char vr--target-buffer-start)
        (cl-loop named loop
				 for i from 0
				 while (funcall (if vr/plain 'search-forward 're-search-forward) regexp-string vr--target-buffer-end t)
				 if (or (not feedback)
						(not feedback-limit)
						(< i feedback-limit))
				 collect (let ((match-data
								(vr--mapcar-nonnil
								 'marker-position
								 (match-data))))
						   (list (query-replace-compile-replacement replace-string t) match-data i))
				 into acc
				 else do (setq vr--limit-reached t)
				 do (when (= (match-beginning 0)
							 (match-end 0))
					  ;; don't get stuck on zero-width matches
					  (if (> vr--target-buffer-end (point))
						  (forward-char)
						(cl-return-from loop)))
				 finally (progn
						   (setq replacements acc)
						   (if feedback
							   (when (string= "" message-line)
								 (setq message-line
									   (vr--compose-messages
										(format "%s matches" i)
										(when vr--limit-reached
										  (format "%s matches shown, hit C-c a to show all" feedback-limit)))))
							 (setq message-line (format "replaced %d matches" i))))))
      replacements)))

(defun vr--do-replace-feedback ()
  "Show visual feedback for replacements."
   ;; only really needed when regexp has not been changed from default (=>
   ;; no overlays have been created)
  (vr--delete-overlays)
  (setq vr--limit-reached nil)

  (let ((replace-string				  ; Get this before focus buffer
		 (vr--get-replace-string)))
	(with-current-buffer vr--target-buffer
	  (save-excursion
		(cl-loop for i from 0
				 while (re-search-forward vr--regexp-string nil t)
				 do (push (vr--get-overlay i) vr--visible-overlays)
				 do (vr--do-replace-feedback-match-callback
					 replace-string
					 (vr--mapcar-nonnil
					  'marker-position
					  (match-data))
					 i)))))

  ;; don't print it out, it has a timeout delay!
  ;; (unless (string= "" message-line)
  ;;   (vr--minibuffer-message message-line))
  )

;;; vr/replace

(defun vr--do-replace ()
  "Replace matches."
  (vr--delete-overlay-displays)
  (vr--delete-overlays)
  (save-match-data
	(cl-loop for count from 0
			 while (re-search-forward vr--regexp-string nil t)
			 do (replace-match vr--replace-string nil nil)
			 finally return count)))

(defun vr--set-regexp-string ()
  (if (or (null isearch-string)
		  (string-empty-p isearch-string))
	  (message "Empty text to replace")
	(setq vr--regexp-string isearch-string)))

(defun vr--set-replace-string ()
  (save-excursion
    ;; deactivate mark so that we can see our faces instead of region-face.
    (deactivate-mark)

    (setq vr--in-minibuffer 'vr--minibuffer-replace)
    (setq vr--last-minibuffer-contents nil)
    (let ((history-add-new-input nil))
      (setq vr--replace-string
            (read-from-minibuffer
             " " ;; prompt will be set in vr--minibuffer-setup
             nil vr/minibuffer-keymap
             nil vr/query-replace-to-history-variable
			 ;; added default value
			 (cdar
			  (symbol-value vr/query-replace-defaults-variable))))
      (add-to-history vr/query-replace-to-history-variable vr--replace-string nil t)
      (add-to-history vr/query-replace-defaults-variable (cons vr--regexp-string vr--replace-string)))))

(defun vr--interactive-get-args ()
  "Get interactive args for the `vr/replace' and `vr/query-replace' functions."
  (unwind-protect
      (save-excursion
        (let ((buffer-read-only t)) ;; make target buffer
          (when vr--in-minibuffer
			(error "Visual-regexp already in use"))
          (add-hook 'after-change-functions 'vr--after-change)
          (add-hook 'minibuffer-setup-hook 'vr--minibuffer-setup)

          (setq vr--target-buffer (current-buffer))
		  (setq vr--target-buffer-start (if (region-active-p)
											(region-beginning)
										  (point)))
		  (setq vr--target-buffer-end (if (region-active-p)
										  (region-end)
										(point-max)))

          (run-hooks 'vr/initialize-hook)
          (setq vr--feedback-limit vr/default-feedback-limit)

          (vr--set-regexp-string)
		  (add-to-history vr/query-replace-from-history-variable
						  vr--regexp-string nil t)

          (vr--set-replace-string)

          ;; Successfully got the args, deactivate mark now. If the
          ;; command was aborted (C-g), the mark (region) would remain
          ;; active.
          (deactivate-mark)
          (list vr--regexp-string
                vr--replace-string
                vr--target-buffer-start
                vr--target-buffer-end)))
    (progn ;; execute on finish
      (setq vr--in-minibuffer nil)
      (remove-hook 'after-change-functions 'vr--after-change)
      (remove-hook 'minibuffer-setup-hook 'vr--minibuffer-setup)
      (vr--delete-overlay-displays)
      (vr--delete-overlays))))

;; query-replace-regexp starts here

(defvar vr--query-replacements nil)
;; we redefine the help text from replace.el to remove the commands we don't support.

(defconst vr--query-replace-help
  "Type Space or `y' to replace one match, Delete or `n' to skip to next,
RET or `q' to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
C-r [not supported in visual-regexp],
C-w [not supported in visual-regexp],
C-l to clear the screen, redisplay, and offer same replacement again,
! to replace all remaining matches with no more questions,
^ [not supported in visual-regexp],
E [not supported in visual-regexp]"
  "Help message while in `vr/query-replace'.")

(defvar vr--query-replace-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map query-replace-map)
    ;; the following replace.el commands are not supported by visual-regexp.
    (define-key map "e" nil)
    (define-key map "E" nil)
    (define-key map "\C-r" nil)
    (define-key map "\C-w" nil)
    (define-key map "^" nil)
    map
    ))

;;;###autoload
(defun vr/query-replace (regexp replace start end)
  "Use `vr/query-replace' like you would use `query-replace-regexp'."
  (interactive (vr--interactive-get-args))
  (unwind-protect
      (progn
        (when vr--in-minibuffer
		  (error "`visual-regexp' already in use"))
        (setq vr--target-buffer (current-buffer)
              vr--target-buffer-start start
              vr--target-buffer-end end
              vr--regexp-string regexp
              vr--replace-string replace)
        (vr--perform-query-replace))
    ;; execute on finish
    (setq vr--in-minibuffer nil)))

(defun vr--perform-query-replace ()
  ;; This function is a heavily modified version of (perform-replace) from replace.el.
  ;; The original plan was to use the original perform-replace, but various issues stood in the way.
  (and minibuffer-auto-raise
       (raise-frame (window-frame (minibuffer-window))))
  (let* ((from-string (vr--get-regexp-string))
         (map vr--query-replace-map)
		 ;; Optimization: This is the only other place that uses
		 ;; vr--get-replacements, Perhaps it will be better to integrate
		 ;; this with the match search below?
         (vr--query-replacements (vr--get-replacements nil nil))
         (next-replacement nil) ;; replacement string for current match
         (keep-going t)
         (replace-count 0)
         ;; a match can be replaced by a longer/shorter replacement. cumulate the difference
         (cumulative-offset 0)
         (recenter-last-op nil) ; Start cycling order with initial position.
         (message
          (concat
           (propertize "Replacing " 'read-only t)
           (propertize "%s" 'read-only t 'face 'font-lock-keyword-face)
           (propertize " with " 'read-only t)
           (propertize "%s" 'read-only t 'face 'font-lock-keyword-face)
           (propertize (substitute-command-keys
                        " (\\<vr--query-replace-map>\\[help] for help) ")
                       'read-only t))))
    ;; show visual feedback for all matches
    (mapc (lambda (replacement-info)
            (cl-multiple-value-bind (replacement match-data i) replacement-info
              (vr--feedback-match-callback i (cl-first match-data) (cl-second match-data))))
          vr--query-replacements)

    (goto-char vr--target-buffer-start)
    (push-mark)
    (undo-boundary)
    (unwind-protect
        ;; Loop finding occurrences that perhaps should be replaced.
        (while (and keep-going vr--query-replacements)
          ;; Advance replacement list
          (cl-multiple-value-bind (replacement match-data i)
			  (pop vr--query-replacements)
            (setq match-data (vr--mapcar-nonnil (lambda (el) (+ cumulative-offset el)) match-data))
            (let ((begin (cl-first match-data))
                  (end (cl-second match-data))
                  (next-replacement-orig replacement))
              (setq next-replacement (vr--get-replacement replacement match-data replace-count))
              (goto-char begin)

              (undo-boundary)
              (let (done replaced key def)
                ;; Loop reading commands until one of them sets done,
                ;; which means it has finished handling this
                ;; occurrence.
                (while (not done)
                  ;; show replacement feedback for current occurrence
                  (unless replaced
                    (vr--do-replace-feedback-match-callback next-replacement-orig match-data i))
                  ;; Bind message-log-max so we don't fill up the message log
                  ;; with a bunch of identical messages.
                  (let ((message-log-max nil))
                    (message message from-string next-replacement))
                  (setq key (read-event))
                  (setq key (vector key))
                  (setq def (lookup-key map key))

                  ;; can use replace-match afterwards
                  (set-match-data match-data)

                  ;; Restore the match data while we process the command.
                  (cl-case def
					(help
                     (with-output-to-temp-buffer "*Help*"
                       (princ
                        (concat "Query replacing visual-regexp "
                                from-string " with "
                                next-replacement ".\n\n"
                                (substitute-command-keys
                                 vr--query-replace-help)))
                       (with-current-buffer standard-output
                         (help-mode))))
                    (exit
                     (setq keep-going nil
                           done t))
                    (act
                     (unless replaced
                       (replace-match next-replacement t t)
                       (cl-incf replace-count))
                     (setq done t
                           replaced t))
                    (act-and-exit
                     (unless replaced
                       (replace-match next-replacement t t)
                       (cl-incf replace-count))
                     (setq keep-going nil
                           done t
                           replaced t))
                    (act-and-show
                     (unless replaced
                       (replace-match next-replacement t t)
                       (cl-incf replace-count)
                       (setq replaced t)))
                    (automatic
                     (setq vr--target-buffer-start (match-beginning 0)
                           vr--target-buffer-end (+ cumulative-offset vr--target-buffer-end))
                     (cl-incf replace-count (vr--do-replace))
                     (setq done t
                           replaced t
                           keep-going nil))
                    (skip
                     (setq done t))
                    (recenter
                     ;; `this-command' has the value `query-replace',
                     ;; so we need to bind it to `recenter-top-bottom'
                     ;; to allow it to detect a sequence of `C-l'.
                     (let ((this-command 'recenter-top-bottom)
                           (last-command 'recenter-top-bottom))
                       (recenter-top-bottom)))
                    (t
                     (setq this-command 'mode-exited)
                     (setq keep-going nil)
                     (setq unread-command-events
                           (append (listify-key-sequence key)
                                   unread-command-events))
                     (setq done t)))
                  (when replaced
                    (setq cumulative-offset (+ cumulative-offset (- (length next-replacement) (- end begin)))))
                  (unless (eq def 'recenter)
                    ;; Reset recenter cycling order to initial position.
                    (setq recenter-last-op nil))
                  ;; in case of 'act-and-show: delete overlay display or it will still be
                  ;; visible even though the replacement has been made
                  (when replaced (vr--delete-overlay-display (vr--get-overlay i)))))

              ;; occurrence has been handled, delete feedback overlay
              (delete-overlay (vr--get-overlay i)))))

      ;; unwind
      (progn
        (vr--delete-overlay-displays)
        (vr--delete-overlays)
        ;; (replace-dehighlight)
        ))
    (unless unread-command-events
      ;; point is set to the end of the last occurrence.
      (goto-char (match-end 0))
      (message "Replaced %d occurrence%s"
               replace-count
               (if (= replace-count 1) "" "s")))))

(provide 'visual-regexp)

;;; visual-regexp.el ends here
