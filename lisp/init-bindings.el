;;; init-bindings --- bindings config -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Created: Sat Aug 21 01:15 BST 2021
;; There are some evil related bindings here as well
;;

;;; Code:

;; How to move forward and backward in Emacs `mark-ring'
;; https://stackoverflow.com/a/14539202

(defun unpop-to-mark-command ()
  "Unpop off mark ring.  Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (let ((last (copy-marker (car (last mark-ring)))))
	  (push last mark-ring)
      ;; (unless (mark t) (ding))
      (setq mark-ring (nbutlast mark-ring))
      (goto-char (marker-position last)))))

(defun my/remind (keybind func-name &optional no-error)
  "Remind to use KEYBIND to invoke this FUNC-NAME."
  (when (and (null (key-binding keybind))
			 (not no-error))
	(warn "my/remind keybind %s -> %s does not exist in global map" keybind func-name))
  (lambda ()
    (interactive)
    ;; TODO: maybe call func-name as well
    (message "use %s to invoke %s"
			 (propertize (key-description keybind) 'face 'help-key-binding)
			 func-name)))

(defun visual-replace-regexp-text-at-point (text &optional regexp)
  (interactive (list (util/thing-at-point/deselect)))
  (require 'visual-replace)
  (when isearch-mode
    ;; @see
    ;; https://github.com/szermatt/visual-replace/issues/2#issuecomment-1160217255
    (user-error "Cannot use this whilst `isearch-mode' is active"))
  (pcase-let* ((vr-args
				(visual-replace-make-args
				 :from (or text "")
				 :query t
				 :regexp regexp))
			   (`(,args ,ranges) (visual-replace-read vr-args)))
    (visual-replace args ranges)))

;; (popup-tip (documentation 'kill-line))

(use-package emacs-surround :ensure nil
  :defer t
  :init
  (defun emacs-surround-wrap-sexp ()
	(interactive)
	(emacs-surround-insert "(")
	(forward-char)
	(my/indent-defun))
  (global-set-key (kbd "M-[") 'emacs-surround)
  (global-set-key (kbd "M-(") 'emacs-surround-wrap-sexp))

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

(defvar leader-keymap (make-sparse-keymap))

(global-set-key [C-m] leader-keymap)

(define-keys leader-keymap
  "aa" #'avy-goto-char-timer
  "ac" #'aya-create
  "ar" #'align-regexp
  "aw" #'avy-goto-word-or-subword-1
  "am" #'apply-macro-to-region-lines

  "cc" #'clipboard-kill-ring-save
  "cam" #'org-tags-view	       ; `C-c a m': search items in org-file-apps by tag
  "ci" #'org-clock-in	       ; `C-c C-x C-i'
  "co" #'org-clock-out	       ; `C-c C-x C-o'
  "cr" #'org-clock-report	   ; `C-c C-x C-r'
  "cf" (lambda () (interactive)	   ; copy buffer name
		 (kill-new buffer-file-name) (message buffer-file-name))

  "di" #'dictionary-lookup
  "dd" #'sdcv-search-input				; details
  "dt" #'sdcv-search-input+				; summary
  "dm" #'lookup-doc-in-man
  ;;
  "da" #'diff-region-tag-selected-as-a
  "db" #'diff-region-compare-with-b
  "cp" #'copy-this-buffer-and-file
  "dt" #'delete-this-buffer-and-file
  "rt" #'rename-this-buffer-and-file

  "eb" #'eval-buffer

  "fm" #'dired-jump ;; open the dired File Manager from current file
  "fb" #'flyspell-buffer
  "fc" #'flyspell-correct-word-before-point

  "gf" #'consult-find
  "gs" #'consult-recent-file
  "gr" #'copy-and-paste
  "g," #'goto-last-change

  "ih" #'my/goto-git-gutter
  "ii" #'consult-imenu
  "ic" #'completing-imenu-comments
  "ip" #'my/insert-path
  "im" #'consult-mark

  "j" #'avy-goto-line-below
  "k" #'avy-goto-line-above

  "lo" (lambda () (interactive) (occur-symbol-at-mouse nil))
  "lq" #'visual-replace-regexp-text-at-point

  "m" (lambda ()
		 (interactive)
		 (push-mark-command t)
		 (deactivate-mark))
  "nw" #'narrow-or-widen-dim

  "op" #'compile
  "oa" (lambda (arg) (interactive "P") (org-agenda arg "n"))
  "od" (lambda (arg) (interactive "P") (org-agenda arg "d"))
  "og" #'org-agenda

  "pw" #'pwd
  "pp" #'clipboard-yank

  "q" #'quit-window
  
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
  "wr" #'rotate-two-split-window)

(define-keys leader-keymap
  [?\C-s] #'completing-swiper
  [?\C-o] #'my/browse-file
  [?\C-b] #'mode-line-other-buffer
  [?\C-t] #'log-done)
;; }}

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-u") #'my/unbound-symbol))

(define-keys global-map
  [?\M-n] #'move-line-down
  [?\M-p] #'move-line-up
  
  (kbd "C-x C-i") #'eval-print-last-sexp
  [?\C-x ?\C-b] #'ibuffer
  
  [?\C-s] #'my/isearch-at-point-maybe
  ;; org mode
  [?\C-c ?o ?c] #'org-capture)

(global-unset-key [?\C-z])				; suspend-frame

(with-eval-after-load 'mac-win
  ;; emacs-mac port introduce some unnecessary binds disable them
  (define-keys global-map
    [mouse-2] 'ignore
    [swipe-right] 'ignore
    [swipe-left] 'ignore))

;; Emacs editing binds
(define-keys global-map
  
  [?\C-\M-_] #'unpop-to-mark-command
  [?\C-_] 'pop-to-mark-command

  [?\C-x ?b] #'consult-project-buffer

  [?\C-h ?d] #'describe-function		; apropos-documentation
  [?\C-h ?f] #'find-function			; describe-function
  [?\C-h ?V] #'find-variable
  [?\C-h ?K] #'find-function-on-key		; Info-goto-emacs-key-command-node
  [?\C-h ?M] #'describe-keymap

  [?\C-\M-s] #'isearch-forward-regexp
  [?\C-\S-s] #'isearch-backward
  [?\C-\S-u] #'join-line
  ;; join line from below
  [?\C-\S-j] (my/remind [?\C-\S-o] 'reverse-open-line :no-error)
  [?\C-\S-o] [?\C-e ?\C-n ?\C-\S-u]
  [?\C-\M-\'] #'vundo

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
  [?\C-c ?.] #'my/insert-date
  
  ;; tab bar
  ;; don't set [C-tab], as it will be set by tab-bar if not set
  [?\s-t] #'tab-bar-new-tab

  [?\s-w] (lambda () (interactive)
			(if (bound-and-true-p tab-bar-mode)
				(call-interactively 'tab-bar-close-tab)
			  (call-interactively 'delete-frame)))
  [C-tab] #'tab-next
  [C-S-tab] #'tab-previous)

(with-eval-after-load 'org
  (define-keys org-mode-map
    [?\C-\M-u] 'org-up-element
    [?\C-\M-e] 'org-next-block
    [?\C-\M-n] 'org-next-visible-heading
    [?\C-\M-p] 'org-previous-visible-heading
    [?\C-\M-d] 'org-forward-heading-same-level

    [?\M-N] 'org-move-subtree-down
    [?\M-P] 'org-move-subtree-up

    [?\C->] 'org-shiftmetaright		; org-do-demote
    [?\C-<] 'org-shiftmetaleft		; org-do-promote
    [?\M-+] 'org-latex-pdf-count-words

    [?\C-c ?o ?a] 'org-archive-subtree
    [?\C-c ?o ?u] 'org-update-statistics-cookies
    [C-m ?o ?e] 'org-babel-execute-subtree))

(provide 'init-bindings)
;;; init-bindings.el ends here
