;;; init-viper --- viper setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;; Every feels alright, after all the key bindings are in place
;;; and the right config set.
;;; However, there are two missing:
;;; 1. viper-define-key, the equivalent of evil-define-key
;;; 2. C-[, it is treated as ESC

;;; Code:

(use-package viper
  :ensure t
  :defer t
  :config
  (setq viper-no-multiple-ESC 'twice)
  :init

  ;; (toggle-viper-mode)
  (setq viper-mode t
        viper-always t
        viper-vi-style-in-minibuffer nil
        viper-ex-style-editing nil	; del go back prev line <insert>
        viper-want-ctl-h-help t
        viper-want-emacs-keys-in-vi t
        viper-want-emacs-keys-in-insert t))

;; (define-keys viper-insert-global-user-map)
(with-eval-after-load 'viper
  (define-keys viper-insert-basic-map
    "\C-d" nil)

  (define-keys viper-vi-global-user-map
   ;; "G"  'end-of-buffer
    )

  (define-keys viper-vi-basic-map
    "m" 'viper-paren-match
    "q" 'start-kbd-macro
    "g" nil  ; delete `viper-nil' binding
    "u" 'undo
    "U" 'join-line
    [return] #'newline-and-indent
    "v" 'er/expand-region
    "/" 'isearch			; viper-exec-mapped-kbd-macro
    "\C-v" 'nil
    "\C-r" 'undo-redo)

  (define-keys viper-emacs-global-user-map
    "\C-z" 'viper-change-state-to-vi)

  (define-keys viper-vi-basic-map
    "g?" 'evil-rot13
    "gd" 'xref-find-definitions
    "gm" 'viper-mark-point
    "gg" 'beginning-of-buffer
    "gh" 'backward-char
    "ga" 'what-cursor-position
    "gc" 'comment-or-uncomment-dwim
    "gr" 'copy-and-paste
    "gf" #'find-file-at-point
    "gs" #'consult-recent-file
    "gb" #'switch-to-buffer
    "gU" 'endless/upcase
    "gu" 'downcase-region
    "g~" 'evil-invert-case
    "g;" 'goto-last-change))

;; disable return
;; (define-key viper-vi-basic-map [?\C-\[] 'viper-exit-insert-state)

(add-hook 'viper-replace-state-hook
          (defun my/viper-replace-hook ()
            "Immediately switch to insert state."
            (when nil
	      (viper-change-state-to-insert)
	      (viper-intercept-ESC-key)
	      (viper-change-state-to-insert)
	      (viper-forward-char 1)
	      (viper-insert))))

(defvar my/viper-local-bindings-alist '())
(defun define-viper-local-binds (mode &rest binds)
  (declare (indent defun))
  (let ((map
	 (make-sparse-keymap)))
    ;; (set-keymap-parent map viper-vi-basic-map)
    (cl-loop for (key bind) on binds by #'cddr
	     do (define-key map key bind))
    (if (assq mode my/viper-local-bindings-alist)
	(setf (cdr (assq mode my/viper-local-bindings-alist))
	      map)
      (push (cons mode map) my/viper-local-bindings-alist))))


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

(define-viper-local-binds 'paredit-mode
  ;; "y" (sexp-and-normal #'mark-sexp  #'evil-yank)
  "gr" (sexp-and-normal #'mark-sexp #'copy-and-paste)
  "gy" (sexp-and-normal #'mark-sexp #'comment-and-copy-line)
  "gc" (sexp-and-normal #'mark-sexp #'comment-or-uncomment-dwim))

(define-viper-local-binds 'org-mode
  ;; custom g leader key
  "gJ" 'org-move-subtree-down
  "gK" 'org-move-subtree-up

  "$" 'org-end-of-line			   ; smarter behaviour on headlines etc.
  "^" 'org-beginning-of-line		   ; ditto
  "<" 'org-do-promote
  ">" 'org-do-demote
  ;; [ret] #'newline-and-indent
  [?\M-+] 'org-latex-pdf-count-words

  ;; [tab] 'org-cycle
  [return] 'org-return)

;; (setq change-major-mode-hook
;;       (remove 'my/prog-mode-viper-key-binds-setup
;; 	      change-major-mode-hook))
(add-hook 'change-major-mode-hook
	  (defun my/prog-mode-viper-key-binds-setup ()
	    (cl-loop for (mode . map) in my/viper-local-bindings-alist
		     if (or (eq major-mode mode)
			    (memq mode minor-mode-list))
		     ;; https://www.gnu.org/software/emacs/manual/html_node/viper/Key-Bindings.html
		     do (viper-modify-major-mode major-mode 'vi-state map)
		     ;; (add-to-list 'emulation-mode-map-alists
		     ;; 		     (cons 'viper-vi-local-user-minor-mode
		     ;; 			   map))
		     )))
		     
(provide 'init-viper)
;;; init-viper.el ends here
