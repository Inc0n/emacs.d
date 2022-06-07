;;; init-bindings --- bindings config -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Created: Sat Aug 21 01:15 BST 2021
;; There are some evil related bindings here as well
;;

;;; Code:

(require-package 'visual-regexp) ;; Press "M-x vr-*"

(defmacro my/evil-define-key (&rest body)
  (declare (indent defun))
  `(with-eval-after-load 'evil
     (evil-define-key ,@body)))

(with-eval-after-load 'visual-regexp
  (setq vr/match-separator-custom-face nil
	vr/match-separator-string nil))

(defun visual-replace-regexp-isearch-or-at-point (&optional arg)
  (interactive "P")
  (require 'visual-replace)
  (let ((arg t))
    (let* ((text (if arg
		     (thing-at-point 'symbol)
		   ;; (util/thing-at-point/deselect)
		   (or isearch-string "")))
	   (vr-args
	    (visual-replace-make-args
	     :from text
	     :query t
	     ;; :regexp t
	     :word t)))
      (message "here")
      (pcase-let ((`(,args ,ranges) (visual-replace-read vr-args)))
	;; (visual-replace args ranges)
	args)
      ;; (when (bounds-of-thing-at-point 'symbol)
      ;;   (goto-char (car (bounds-of-thing-at-point 'symbol))))
      ;; (goto-char (point))
      ;; (let ((inhibit-quit t)
      ;;	   (isearch-string regexp-string))
      ;;   (with-local-quit
      ;;	 (call-interactively 'isearch-query-replace))
      ;;   (lazy-highlight-cleanup 'force))
      )))

(defun paredit-spliced-raise ()
  (interactive)
  (kill-sexp)
  (paredit-backward-up)
  (paredit-forward)
  (newline-and-indent)
  (call-interactively 'yank)
  (pop kill-ring))

;; (popup-tip (documentation 'paredit-copy-as-kill))

(defun handle-sexp (paredit-fn normal-fn arg)
  "PAREDIT-FN NORMAL-FN ARG."
  (if (and (null arg)
	   (not (region-active-p))
	   ;; if point is before the end of sexp
	   (-some--> (bounds-of-thing-at-point 'sexp)
	     (< (point) (cdr it))))
      (funcall paredit-fn)
    (call-interactively normal-fn)))

;; DONE <2021-08-20 FRI>: mark region if char syntax is ?\)
;; small change allow operation to operate on ?\) instead of marking region

(defun sexp-or-normal (sexp-fn normal-fn)
  "Check `handle-sexp' for details of SEXP-FN and NORMAL-FN."
  (if (and (functionp normal-fn)
	   (commandp normal-fn)
	   (functionp sexp-fn))
      (lambda (&optional arg)
	(interactive "P")
	(handle-sexp sexp-fn normal-fn arg))
    ;; use warning over error to prevent stopping loading config files
    (warn "Normal-fn is not an interactive function, %s" normal-fn)))

(defun sexp-and-normal (sexp-fn normal-fn)
  "Check `handle-sexp' for details of SEXP-FN and NORMAL-FN.
The two functions are applied in sequence."
  (sexp-or-normal
   (lambda ()
     (funcall sexp-fn)
     (call-interactively normal-fn))
   normal-fn))

(defun my/remind (keybind func-name)
  "Remind to use KEYBIND to invoke this FUNC-NAME."
  (unless (key-binding keybind)
    (warn "keybind %s -> %s does not exist in global map" keybind func-name))
  (lambda ()
    (interactive)
    ;; TODO: maybe call func-name as well
    (message "use %s to invoke %s"
	     (propertize (key-description keybind) 'face 'help-key-binding)
	     func-name)))

(local-require 'paredit 'force)
(with-eval-after-load 'paredit
  (define-keys paredit-mode-map
    [?\C-\M-w] (sexp-and-normal #'mark-sexp  #'kill-ring-save)
    [C-m ?g ?r] (sexp-and-normal #'mark-sexp #'copy-and-paste)
    (kbd "M-;") (sexp-and-normal #'mark-sexp #'comment-or-uncomment-dwim))
  (my/evil-define-key '(normal visual) paredit-mode-map
    "y" (sexp-and-normal #'mark-sexp  #'evil-yank)
    "gr" (sexp-and-normal #'mark-sexp #'copy-and-paste)
    "gy" (sexp-and-normal #'mark-sexp #'comment-and-copy-line)
    "gc" (sexp-and-normal #'mark-sexp #'comment-or-uncomment-dwim)))

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
(with-eval-after-load 'evil
  (evil-define-key '(normal motion visual) 'global (kbd "SPC") leader-keymap))

(define-keys leader-keymap
  "ac" #'aya-create
  "ar" #'align-regexp
  "aw" #'avy-goto-word-or-subword-1
  "aa" #'avy-goto-char-timer

  "cc" #'clipboard-kill-ring-save
  "cam" #'org-tags-view	       ; `C-c a m': search items in org-file-apps by tag
  "ci" #'org-clock-in	       ; `C-c C-x C-i'
  "co" #'org-clock-out	       ; `C-c C-x C-o'
  "cr" #'org-clock-report       ; `C-c C-x C-r'
  "cf" (lambda () (interactive)
	 (kill-new buffer-file-name) (message buffer-file-name))

  "di" #'dictionary-lookup
  "dd" #'sdcv-search-input		; details
  "dt" #'sdcv-search-input+		; summary
  "dm" #'lookup-doc-in-man
  ;;
  "da" #'diff-region-tag-selected-as-a
  "db" #'diff-region-compare-with-b
  "cp" #'copy-this-buffer-and-file
  "dt" #'delete-this-buffer-and-file
  "rt" #'rename-this-buffer-and-file

  "eb" #'eval-buffer

  "fp" #'find-file-in-project-at-point
  "fc" #'find-file-with-similar-name	; ffip v5.3.1
  "fi" #'selectsel-ffip			; 'find-file-in-project
  "fd" #'find-directory-in-project-by-selected
  "fm" #'dired-jump ;; open the dired File Manager from current file
  "fa" #'flyspell-auto-correct-word
  "fb" #'flyspell-buffer
  "fc" #'flyspell-correct-word-before-point
  "fs" #'flyspell-goto-next-error
  "fe" #'flycheck-next-error

  "gs" #'consult-recent-file
  "gf" #'find-file-at-point
  "gj" #'avy-goto-line-below
  "gk" #'avy-goto-line-above
  "gb" #'consult-buffer
  "g," #'goto-last-change
  "gr" #'copy-and-paste

  "ih" #'my/goto-git-gutter
  "ii" #'consult-imenu
  "ic" #'completing-imenu-comments
  "ip" #'my/insert-path
  "im" #'consult-mark

  "kr" #'apply-macro-to-region-lines

  "lo" (lambda () (interactive) (occur-symbol-at-mouse nil))
  "ll" #'isearch-repeat-forward
  "lq" #'visual-replace-regexp-isearch-or-at-point
  "ld" #'isearch-within-defun

  "mp" 'pop-to-mark-command ;; 'avy-pop-mark
  "mm" (lambda ()
	 (interactive)
	 (push-mark-command t)
	 (deactivate-mark))

  "nw" #'narrow-or-widen-dim
  "nh" #'my/goto-next-hunk

  "op" #'compile
  "oa" (lambda (arg) (interactive "P") (org-agenda arg "n"))
  "od" (lambda (arg) (interactive "P") (org-agenda arg "d"))
  "og" #'org-agenda

  "pw" #'pwd
  "ph" #'my/goto-previous-hunk
  "pp" #'clipboard-yank

  "sr" #'scratch
  "ss" #'completing-ripgrep
  "sg" #'consult-git-grep

  "tr" #'run-temp-lambda
  "ts" #'save-temp-lambda
  "tp" #'toggle-pinyin-in-completing-read
  "tt" #'my/toggle-day/night

  "vv" #'vc-msg-show

  "xw" #'kill-buffer-and-window
  ;;
  "ur" #'winner-redo
  "uu" #'winner-undo
  "wr" #'rotate-two-split-window
  ;;
  (kbd "SPC") 'just-one-space)

(define-keys leader-keymap
  [?\C-s] #'completing-swiper
  [?\C-o] #'my/browse-file
  [?\C-b] #'mode-line-other-buffer
  [?\C-t] #'log-done)
;; }}

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-u") #'my/unbound-symbol))

(define-keys global-map
  [?\C-h ?d] #'describe-function	; apropos-documentation
  [?\C-h ?f] #'find-function		; describe-function
  [?\C-h ?V] #'find-variable
  [?\C-h ?K] #'find-function-on-key	; Info-goto-emacs-key-command-node
  [?\C-h ?M] #'describe-keymap

  [?\M-n] #'move-line-down
  [?\M-p] #'move-line-up
  
  (kbd "C-x C-i") #'eval-print-last-sexp
  
  [?\M-\[] #'wrap-region-activate

  [?\C-s] #'my/isearch-at-point-maybe
  ;; [?\C-s] #'completing-swiper
  [?\C-\\] #'evil-toggle-input-method
  ;; org mode
  [?\C-c ?o ?c] #'org-capture)

;; Emacs editing binds
(define-keys global-map
  ;; [?\C-q] 'aya-open-line
  [?\C-\S-u] #'join-line
  ;; join line from below
  [?\C-\S-j] [?\C-n ?\C-\S-u]
  ;; [?\C-\S-s] #'isearch-backward
  [?\C-\M-s] #'isearch-repeat-forward
  [?\C-\M--] #'undo-redo
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
    
    [?\C-\M-\S-n] 'org-move-subtree-down
    [?\C-\M-\S-p] 'org-move-subtree-up

    [?\C->] 'org-shiftmetaright		  ; org-do-demote
    [?\C-<] 'org-shiftmetaleft		  ; org-do-promote
    [?\M-+] 'org-latex-pdf-count-words

    [?\C-c ?o ?a] 'org-archive-subtree
    [?\C-c ?o ?u] 'org-update-statistics-cookies
    [C-m ?o ?e] 'org-babel-execute-subtree))

(provide 'init-bindings)
;;; init-bindings ends here
