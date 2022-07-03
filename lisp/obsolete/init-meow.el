;;; init-meow.el --- meow setup -*- coding: utf-8; -*-
;;; Commentary:
;;; Code:

(defmacro meow-define-key (states &rest key-binds)
  (declare (debug (&define states
                           [&rest key-binds]))
           (indent defun))
  (if (listp states)
      `(progn
	 ,@(cl-loop
	    for state in states
	    collect
	    `(let ((keymap (cdr (assoc ',state meow-keymap-alist))))
	       ,@(cl-loop for (key def) on key-binds by #'cddr
			  collect `(define-key keymap ,key ,def)))))
    `(meow-define-key
       (,states)
       ,@key-binds)))

(defvar my/meow-state-mode-map-alist '())

(defmacro my/meow-define-state-for-map (major-mode &rest key-binds)
  (declare (indent defun))
  (let ((keymap (intern (format "meow-custom-%s-keymap" major-mode))))
    `(progn
       (defvar ,keymap (make-keymap))
       (meow-define-state ,major-mode
	 ,(format "meow state for interacting with %s" major-mode)
	 ;; :lighter " [P]"
	 :keymap ,keymap)

       ;; meow-define-state creates the variable
       ;; (setq meow-cursor-type-paren 'hollow)

       (meow-define-key ,major-mode ,@key-binds)
       (add-to-list 'my/meow-state-mode-map-alist ',major-mode))))

(defun meow-activate-custom-state-map ()
  (when-let ((mode
	      (cl-find-if
	       (lambda (mode)
		 (eq major-mode mode))
	       my/meow-state-mode-map-alist)))
    (meow--switch-state mode)))

(defun meow-search-forward (arg)
  (interactive "p")
  (meow-search
   (and (if (and arg (not (eq arg -1)))
	    (meow--direction-backward-p)
	  (not (meow--direction-backward-p)))
	'-)))

(defun meow-search-backward ()
  (interactive)
  (meow-search-forward -1))

(with-eval-after-load 'meow
  (meow-define-key (motion)
    "j" 'meow-next
    "k" 'meow-prev)
  ;; (meow-define-key (leader)
  ;; "xf" 'find-file-at-point
  ;; "ls" 'isearch-at-point)

  (meow-define-key (insert)
    [escape] nil)

  (meow-define-key (normal insert)
    [?\C-r] 'undo-redo)

  (meow-define-key (normal)
    [?\C-z] 'meow-normal-mode)
  
  (meow-define-key (normal)
    "0" #'meow-expand-0
    "9" #'meow-expand-9
    "8" #'meow-expand-8
    "7" #'meow-expand-7
    "6" #'meow-expand-6
    "5" #'meow-expand-5
    "4" #'meow-expand-4
    "3" #'meow-expand-3
    "2" #'meow-expand-2
    "1" #'meow-expand-1
    "-" #'negative-argument
    ";" #'meow-reverse
    "," #'meow-inner-of-thing
    "." #'meow-bounds-of-thing
    "[" #'meow-beginning-of-thing
    "]" #'meow-end-of-thing
    "a" #'meow-append
    "A" #'meow-block
    "b" #'meow-back-word
    "B" #'meow-back-symbol
    "c" #'meow-change
    "C" "si"
    "d" #'ignore
    "D" #'ignore 
    "e" #'meow-next-word
    "E" #'meow-next-symbol
    "f" #'meow-find
    "g" #'meow-cancel-selection
    "G" #'meow-grab
    "h" #'meow-left
    "H" #'meow-left-expand
    "i" #'meow-insert
    "I" #'meow-open-above
    "j" #'meow-next
    "J" #'meow-next-expand
    "k" #'meow-prev
    "K" #'meow-prev-expand
    "l" #'meow-right
    "L" #'meow-right-expand
    "m" #'meow-join
    "n" #'meow-search-forward
    "N" #'meow-search-backward
    "o" #'meow-open-below
    "O" #'meow-to-block
    "p" #'meow-yank
    "q" (lambda () (interactive)
	  (if (or defining-kbd-macro executing-kbd-macro)
	      (call-interactively #'end-kbd-macro)
	    (call-interactively #'kmacro-start-macro)))
    "Q" #'meow-goto-line
    "r" #'meow-replace
    "R" #'meow-swap-grab
    "s" #'meow-kill
    "t" #'meow-till
    "u" #'meow-undo
    "U" #'join-line
    "v" #'meow-visit
    "V" #'meow-line
    "/" #'isearch-forward-regexp
    "w" #'meow-mark-word
    "W" #'meow-mark-symbol
    "x" #'meow-delete
    "X" #'meow-goto-line
    "y" #'meow-save
    "Y" #'meow-sync-grab
    "z" #'meow-pop-selection
    "'" #'repeat
    "<escape>" #'meow-cancel)

  (setf (cdr (assoc 'help-mode meow-mode-state-list))
	'motion)

  (my/meow-define-state-for-map prog
    "mr" (sexp-and-normal #'mark-sexp #'copy-and-paste)
    "my" (sexp-and-normal #'mark-sexp #'comment-and-copy-line)
    "mc" (sexp-and-normal #'mark-sexp #'comment-or-uncomment-dwim)))

;;; 
;; open below
;; join line
;; jump match
;; comment
(local-require 'meow)
(use-package meow
  :commands (meow-global-mode)
  :defer t
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
        meow-use-clipboard t)
  (setf (cdr (assq 'leader meow-keymap-alist)) leader-keymap)
  :init
  (meow-global-mode -1))

(provide 'init-meow)
