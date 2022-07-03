;;; init-bindings --- bindings config -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Created: Sat Aug 21 01:15 BST 2021
;; There are some evil related bindings here as well
;;

;;; Code:

(defun visual-replace-regexp-text-at-point (text &optional regexp)
  (interactive (list (util/thing-at-point/deselect)))
  (require 'visual-replace)
  (when isearch-mode
    ;; @see
    ;; https://github.com/szermatt/visual-replace/issues/2#issuecomment-1160217255
    (user-error "Cannot use this whilst `isearch-mode' is active"))
  (pcase-let* ((vr-args
				(visual-replace-make-args
				 :from text
				 :query t
				 :regexp regexp
				 :word nil))
			   (`(,args ,ranges) (visual-replace-read vr-args)))
    (visual-replace args ranges)))

(defun paredit-spliced-raise ()
  (interactive)
  (kill-sexp)
  (paredit-backward-up)
  (paredit-forward)
  (newline-and-indent)
  (call-interactively 'yank)
  (pop kill-ring))

;; (popup-tip (documentation 'paredit-copy-as-kill))

(defun handle-sexp (fn arg)
  "FN ARG."
  (when (and (null arg)
			 (not (region-active-p))
			 ;; if point is before the end of sexp
			 (-some--> (bounds-of-thing-at-point 'sexp)
			   (< (point) (cdr it))))
    (mark-sexp))
  (call-interactively fn))

;; DONE <2021-08-20 FRI>: mark region if char syntax is ?\)
;; small change allow operation to operate on ?\) instead of marking region

(defun sexp-and-normal (normal-fn)
  "Check `handle-sexp' for details of SEXP-FN and NORMAL-FN."
  (if (and (functionp normal-fn)
		   (commandp normal-fn))
      (lambda (&optional arg)
		(interactive "P")
		(handle-sexp normal-fn arg))
    ;; use warning over error to prevent stopping loading config files
    (warn "Normal-fn is not an interactive function, %s" normal-fn)))

(local-require 'paredit 'force)
(with-eval-after-load 'paredit
  (define-keys paredit-mode-map
    [?\C-\M-w] (sexp-and-normal #'kill-ring-save)
    [C-m ?g ?r] (sexp-and-normal #'copy-and-paste)
    (kbd "C-M-;") (sexp-and-normal #'comment-or-uncomment-dwim)))

(local-require 'wrap-region)
(with-eval-after-load 'wrap-region
  (let ((js-mode '((?j . ("JSON.stringify(" . ")"))
				   (?> . ("(e) => " . "(e)")))))
    (add-to-list 'wrap-region-mode-pairs (cons 'js-mode js-mode))
    (add-to-list 'wrap-region-mode-pairs (cons 'js-mode2 js-mode)))
  (add-to-list 'wrap-region-mode-pairs '(sh-mode (?$ . ("$(" . ")")))))

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
  "ac" #'aya-create
  "ar" #'align-regexp
  "aw" #'avy-goto-word-or-subword-1
  "aa" #'avy-goto-char-timer

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

  "fp" #'find-file-in-project-at-point
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

  "mr" #'apply-macro-to-region-lines
  "mp" 'pop-to-mark-command ;; 'avy-pop-mark
  "mm" (lambda ()
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
  [?\C-x ?b] #'consult-project-buffer
  [?\C-h ?d] #'describe-function	; apropos-documentation
  [?\C-h ?f] #'find-function		; describe-function
  [?\C-h ?V] #'find-variable
  [?\C-h ?K] #'find-function-on-key	; Info-goto-emacs-key-command-node
  [?\C-h ?M] #'describe-keymap

  [?\M-n] #'move-line-down
  [?\M-p] #'move-line-up
  
  (kbd "C-x C-i") #'eval-print-last-sexp
  [?\C-x ?\C-b] #'ibuffer
  
  [?\M-\[] #'wrap-region-activate
  
  [?\C-s] #'my/isearch-at-point-maybe
  ;; org mode
  [?\C-c ?o ?c] #'org-capture)

(with-eval-after-load 'mac-win
  ;; emacs-mac port introduce some unnecessary binds disable them
  (define-keys global-map
    [mouse-2] 'ignore
    [swipe-right] 'ignore
    [swipe-left] 'ignore))

;; Emacs editing binds
(define-keys global-map
  [?\C-\M-s] #'isearch-forward-regexp
  [?\C-\S-u] #'join-line
  ;; join line from below
  [?\C-\S-j] [?\C-e ?\C-n ?\C-\S-u]
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
