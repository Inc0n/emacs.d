;;; init-bindings --- bindings config -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Created: Sat Aug 21 01:15 BST 2021
;; There are some evil related bindings here as well
;;

;;; Code:

;; How to move forward and backward in Emacs `mark-ring'
;; https://stackoverflow.com/a/14539202

(use-package history
  :disabled
  :load-path "~/.emacs.d/site-lisp/"
  :ensure nil
  :init (add-hook 'after-init-hook 'history-mode))

(defun my/remind (keybind func-name &optional no-error)
  "Remind to use KEYBIND to invoke this FUNC-NAME."
  (when (and (null (key-binding keybind))
			 (not no-error))
	(warn "my/remind keybind %s -> %s does not exist in global map"
		  keybind func-name))
  (lambda ()
    (interactive)
    ;; TODO: maybe call func-name as well
    (message "use %s instead to invoke %s"
			 (propertize (key-description keybind) 'face 'help-key-binding)
			 func-name)))

(with-eval-after-load 'mac-win
  (when mac-mouse-wheel-mode
	(mac-mouse-wheel--remove-bindings))
  ;; dont't want accidental scrolls with controll+wheel up
  (setq mouse-wheel-scroll-amount
		(remove '((control) . text-scale)
				mouse-wheel-scroll-amount))
  (when mac-mouse-wheel-mode
	(mac-mouse-wheel--setup-bindings)))

(use-package emacs-surround :ensure nil
  :load-path "~/.emacs.d/site-lisp/"
  :commands (emacs-surround emacs-surround-insert)
  :config (add-to-list 'emacs-surround-alist '("<" . ("<" . ">")))
  :init
  (defun emacs-surround-sexp-at-point ()
	(or (bounds-of-thing-at-point 'sexp)
		;; handle if text under cursor is empty, e.g. just space
		(cons (point)
			  ;; forward-thing would error if forward-sexp failed
			  ;; (or (save-excursion
			  ;; 	   (forward-thing 'sexp 1)
			  ;; 	   (point)))
			  (point))))
  (defun emacs-surround-wrap-sexp-with (thing)
	(interactive
	 (list
	  (let* ((type (event-basic-type last-command-event))
			 (char (if (characterp type)
					   ;; Number on the main row.
					   type
					 ;; Keypad number, if bound directly.
					 (car (last (string-to-list (symbol-name type)))))))
		;; convert to string
		(string char))))
	(emacs-surround-insert thing #'emacs-surround-sexp-at-point)
	(forward-char)
	;; (my/indent-defun)
	(ignore-errors (indent-pp-sexp)))

  (setq delete-pair-blink-delay 0.1)

  (keyboard-translate ?9 ?\()
  (keyboard-translate ?\( ?9)

  (util:define-keys global-map
	[?\M-s ?c] #'emacs-surround-change-at-point
	[?\M-s ?s] #'emacs-surround-insert
	[?\M-s ?d] #'delete-pair
	[?\M-\(] #'emacs-surround-wrap-sexp-with
	[?\M-\[] #'emacs-surround-wrap-sexp-with
	[?\M-\"] (lambda () (interactive) (emacs-surround-insert "\""))))

(defun move-line-up (start end n)
  (interactive "r\np")
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down (start end n)
  (interactive "r\np")
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;; {{ Leader key
(defvar leader-keymap ;; (make-sparse-keymap)
  mode-specific-map)  ; It's C-c map

;; (global-set-key [C-m] leader-keymap)

(util:define-keys leader-keymap
  "ar" #'align-regexp
  "aw" #'avy-goto-word-or-subword-1
  "am" #'apply-macro-to-region-lines

  "f" #'next-buffer
  "b" #'mode-line-other-buffer

  "cc" #'clipboard-kill-ring-save
  "cam" #'org-tags-view	       ; `C-c a m': search items in org-file-apps by tag
  "ci" #'org-clock-in	       ; `C-c C-x C-i'
  "co" #'org-clock-out	       ; `C-c C-x C-o'
  "cr" #'org-clock-report	   ; `C-c C-x C-r'
  "cb" (lambda () (interactive)	   ; copy buffer name
		 (kill-new buffer-file-name) (message buffer-file-name))

  "di" #'dictionary-lookup
  "dd" #'sdcv-search-input				; details
  "dt" #'sdcv-search-input+				; summary
  "dm" #'man

  "ct" #'copy-this-buffer-and-file
  "dt" #'delete-this-buffer-and-file
  "rt" #'rename-this-buffer-and-file

  "eb" #'eval-buffer

  "hf" #'consult-find
  "hs" #'consult-recent-file
  "hr" #'copy-and-paste

  "ih" #'my/goto-git-gutter
  "ii" #'consult-imenu
  "ic" #'completing-imenu-comments
  "im" #'consult-mark

  ;; NOTE: `avy-goto-word' can be replaced with an isearch
  ;; implementation and `avy-goto-line' can be achieved with switching
  ;; to relative line spacing, and read char twice
  "j" #'avy-goto-line-below
  "k" #'avy-goto-line-above

  "m" (lambda ()
		(interactive)
		(push-mark-command t)
		(deactivate-mark))
  "n" #'narrow-or-widen-dim

  "op" #'compile
  "on" (lambda (arg) (interactive "P") (org-agenda arg "n"))
  "od" (lambda (arg) (interactive "P") (org-agenda arg "d"))
  "ot" (lambda (arg) (interactive "P") (org-agenda arg "t"))
  "om" #'emms
  "oa" #'org-agenda
  "oc" #'org-capture
  "os" (lambda () (interactive) (occur-symbol-at-mouse nil))

  "pw" #'pwd
  "pp" #'clipboard-yank

  "q" #'quit-window

  "." #'my/insert-date

  "sr" #'scratch
  "ss" #'completing-ripgrep
  "sg" #'consult-git-grep

  "tr" #'run-temp-lambda
  "ts" #'save-temp-lambda
  "tt" #'my/toggle-day/night

  "xb" #'switch-to-buffer
  "xf" #'find-file-at-point
  "xw" #'kill-buffer-and-window
  ;;
  "uu" #'winner-undo
  "wr" #'rotate-two-split-window
  "ws" (lambda (k) (interactive
			   (list (read-key "Press key for direction: ")))
		 (let ((k (cl-case k
					((left right up down) k)
					((?h) 'left)
					((?j) 'down)
					((?k) 'up)
					((?l) 'right)
					(t k))))
		   (autoload 'windmove-swap-states-in-direction "windmove")
		   (if (memq k '(left right up down))
			   (windmove-swap-states-in-direction k)
			 (user-error "Use arrow key instead! Got %s" k)))))
;; }}

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map
	"" #'util:unbound-symbol))

(util:define-keys global-map
  [?\M-n] #'move-line-down
  [?\M-p] #'move-line-up

  ;; [?\C-x ?\C-i] #'eval-print-last-sexp
  [?\C-x ?\C-b] #'ibuffer

  [?\C-s] #'my/isearch-at-point-maybe)

(global-unset-key [?\C-z])				; suspend-frame

(with-eval-after-load 'mac-win
  ;; emacs-mac port introduce some unnecessary binds., disable them!
  (util:define-keys global-map
	[C-wheel-up] 'ignore				; annoying scrolls
	[C-wheel-down] 'ignore
    [mouse-2] 'ignore					; what does this do? Prob bad
    [swipe-right] 'ignore				; annoying frame switch
    [swipe-left] 'ignore))

;; Emacs editing binds
(util:define-keys global-map
  [?\C-_] #'pop-global-mark
  [?\C-\M-_] #'unpop-global-mark-command

  [?\C-\M-\-] #'goto-last-change ;; highlight-changes-next-change
  [?\C-\M-=] #'goto-last-change-reverse

  [?\C-\M-s] #'isearch-forward-regexp
  [?\C-\M-\'] #'vundo

  ;; [?\C-x ?b] #'consult-project-buffer	; switch-to-buffer

  [?\C-h ?d] #'describe-function		; apropos-documentation
  [?\C-h ?f] #'find-function			; describe-function
  [?\C-h ?K] #'find-function-on-key		; Info-goto-emacs-key-command-node
  [?\C-h ?M] #'describe-keymap

  [?\C-\S-s] #'isearch-backward
  [?\C-\S-u] #'join-line
  ;; join line from below
  [?\C-\S-o] [?\C-e ?\C-n ?\C-\S-u]

  [s-backspace] (lambda (arg)
				  "Kill ARG lines backward."
				  (interactive "p")
				  (kill-line (- 1 arg)))
  [C-backspace] #'backward-delete-word
  [C-M-backspace] #'backward-kill-sexp

  ;; @see https://emacs-china.org/t/emacs-builtin-mode/11937/63
  ;; Navigate window layouts with "C-c <left>" and "C-c <right>"
  ;; prompt for buffer to switch after window split
  [?\C-x ?2] (lambda () (interactive)
			   (split-window-vertically)
			   (other-window 1)
			   (consult-buffer))
  [?\C-x ?3] (lambda () (interactive)
			   (split-window-horizontally)
			   (other-window 1)
			   (consult-buffer))

  ;; tab bar
  ;; don't set [C-tab], as it will be set by tab-bar if not set
  [?\s-t] #'tab-bar-new-tab

  [?\s-w] (lambda () (interactive)
			(if (bound-and-true-p tab-bar-mode)
				(call-interactively 'tab-bar-close-tab)
			  (call-interactively 'delete-frame)))
  [C-tab] #'tab-next
  [C-S-tab] #'tab-previous)

(provide 'init-bindings)
;;; init-bindings.el ends here
