;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

(use-package janet-mode :ensure t :defer t
  :commands (janet-mode)
  :config
  (add-hook 'janet-mode-hook
            (defun janet-mode-setup ()
              (when (boundp 'ajsc-interaction-mode)
                (ajsc-interaction-mode 1)))))

(use-package inf-janet :disabled
  :defer t
  :straight (inf-janet :type git :host github
					   :repo "velkyel/inf-janet"
					   :files ("*.el")))

(use-package cider :ensure t :defer t
  :disabled
  :config
  ;; @See for information on configuring profiles
  ;; https://github.com/clojure-emacs/cider/discussions/3025
  (add-to-list 'cider-jack-in-dependencies
			   '("org.clojure/tools.deps.alpha" "0.14.1222"))
  (setq cider-jack-in-dependencies '())
  (util:define-keys cider-repl-mode-map
	(kbd "C-c M-o") 'cider-repl-clear-buffer))

;; (use-package ob-clojure-literate :ensure t)
(use-package flymake-kondor
  :ensure t
  :hook ((clojure-mode . flymake-kondor-setup)
		 (clojurescript-mode . flymake-kondor-setup)))

(use-package clojure-mode :ensure t :defer t
  :config
  (add-hook 'clojure-mode-hook
			(defun my/clojure-mode-setup ()
			  ;; cider is force setting sesman system to 'CIDER
			  ;; Shame on them. This causes an error on c-redisplay.
			  (setq-local sesman-system nil))))

(use-package inf-clojure :ensure t :defer t
  :config
  ;; Allows for ~/.clojure/deps.edn setup
  (setq clojure-project-root-function
		(lambda (arg) (expand-file-name "~/.clojure/"))
		clojure-cache-project-dir nil)

  (defun inf-clojure-eval-last-sexp-in-ns ()
	(interactive)
	(inf-clojure-eval-string
	 (format "(binding [*ns* (find-ns '%s)] (eval '%s))"
			 (cider-current-ns)
			 (buffer-substring
			  (save-excursion (backward-sexp) (point))
			  (point)))))
  (define-key inf-clojure-minor-mode-map ""
	'inf-clojure-eval-last-sexp-in-ns)

  (add-to-list 'inf-clojure-startup-forms
			   '(clojure . "clj -M:nREPL -m nrepl.cmdline -i"))
  :init
  (setq inf-clojure-auto-mode nil)
  (with-eval-after-load 'clojure-mode
	(define-key clojure-mode-map ""
	  'inf-clojure
	  ;; (lambda () (interactive)
	  ;; 	(let ((default-directory "~/.clojure/"))
	  ;; 	  (call-interactively 'cider-jack-in-clj)
	  ;; 	  ))
	  )))

(require-package 'slime)

(setq-default inferior-lisp-program ;; (executable-find "ecl")
			  (executable-find "sbcl"))

;; lisp mode setups

(with-eval-after-load 'sly
  ;; too intrusive, it does not co-exist with slime.
  ;; e.g. asking to remove slime on every lisp file opened.
  (require 'sly-autoloads)

  (use-package sly-stepper :disabled
	:straight
	(sly-stepeer :type git :host github
				 :repo "joaotavora/sly-stepper"
				 :files ("*.el" "*.lisp")))

  (add-hook 'sly-mode-hook
			(defun sly-mode-hook ()
			  (require 'sly-stepper-autoloads)
			  ;; No, Thank you
			  (sly-symbol-completion-mode -1))))

(with-eval-after-load 'lisp-mode
  ;; 09/08/22 racket has changed my view on [()] mixing
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table))

(with-eval-after-load 'scheme-mode
  ;; Make [] part of scheme syntax table as well
  (modify-syntax-entry ?\[ "(]" scheme-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" scheme-mode-syntax-table))

(defun prog-comment-self-insert (char)
  "Insert the comment CHAR.
Call `newline-and-indent' if there are imbalanced sexp after point"
  (interactive (list last-input-event))
  (if (and (= (char-syntax char) ?<)	; is a comment char?
		   ;; (not (looking-at-p (rx (* whitespace) line-end)))
		   (not (puni--in-comment-p))
		   ;; newline if there are imbalanced sexp after point
		   (not (puni-region-balance-p (point) (line-end-position))))
	  (progn (insert char)
			 ;; (insert ?\n), the below is more sophisticated
			 ;; reindent-then-newline-and-indent
			 (save-excursion
			   (newline-and-indent)))
	(insert char)))

(defun my/lisp-setup ()
  "Enable features useful in for Lisp mode."
  (local-set-key ";" 'prog-comment-self-insert)
  (puni-mode 1)
  ;; (rainbow-delimiters-mode 1)
  (turn-on-eldoc-mode)
  (checkdoc-minor-mode 1)
  (cl-font-lock-built-in-mode 1)
  (push `("Alias" ,(rx (group "defalias")
					   (1+ space)
					   (group (1+ alnum)))
		  2)
        imenu-generic-expression))

(dolist (hook '(emacs-lisp-mode-hook
				lisp-mode-hook
				lisp-data-mode-hook
                racket-mode-hook
				racket-repl-mode
                ;;
                inferior-lisp-mode-hook
                lisp-interaction-mode-hook
                slime-repl-mode-hook
				sly-mrepl-mode-hook
                ;;
				scheme-mode-hook
				janet-mode-hook
                gerbil-mode-hook))
  (add-hook hook #'my/lisp-setup))

;; racket
(require-package 'racket-mode)
(with-eval-after-load 'racket-mode
  ;; raco pkg install --auto data-lib errortrace-lib macro-debugger-text-lib rackunit-lib racket-index scribble-lib drracket-tool-text-lib
  ;; If you do not want to use racket-xp-mode, then you can omit drracket-tool-text-lib. 
  ;; this would breaks pyim (chinese input)
  ;; (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)
  ;; (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
  ;; (setq racket-images-system-viewer "qiv")
  (setq-default racket-debuggable-files #'list
				racket-error-context 'high
				racket-memory-limit 512
				racket-program (executable-find "racket"))
  (defun my/racket-mark-defun ()
	(interactive)
	(while (condition-case err
			   (call-interactively 'backward-up-list)
			 (user-error nil)
			 (:success t)))
	(beginning-of-line)
	(activate-mark)
	(forward-sexp))

  (defun my/racket-repl-restart ()
	(interactive)
	(when (racket--repl-live-p)
	  (comint-send-string
	   (get-buffer-process racket-repl-buffer-name)
	   ""))
	(racket-repl))

  (defun racket-mode-setup ()
	(setq-local eldoc-documentation-function
				;; 'racket-xp-eldoc-function
				nil)
	;; lsp experience, but require drracket package
	(racket-xp-mode -1)
	(add-hook 'xref-backend-functions
			  'racket-xp-xref-backend-function
			  nil :local))
  (add-hook 'racket-mode-hook 'racket-mode-setup)

  (util:define-keys racket-mode-map
	;; [?\M-\[] 'emacs-surround-wrap-sexp-with
	[?\C-\M-y] nil		; racket-insert-lambda
	(kbd "C-x tab") nil
	[remap eval-print-last-sexp] 'racket-eval-last-sexp)

  ;; default
  ;; (setq racket-show-functions '(racket-show-pseudo-tooltip))
  (setq racket-show-functions '(racket-dante-show-result))
  ;; add-to-list doesn't work ...
  ;; (add-to-list 'racket-show-functions )
  ;;; NOTE: racket show does not capture prints and display
  (defun racket-dante-show-result (str &optional pos)
	"Dante style eval sexp within file."
	(when (and str pos)					; save guard.
	  (save-excursion
		(let ((prefix ">>> "))
		  (unless current-prefix-arg
			;; (comment-dwim nil)
			(save-match-data
			  (forward-line)
			  (beginning-of-line)
			  (let ((line-end (line-end-position)))
				(if (search-forward prefix line-end
									:noerror)
					(goto-char (match-beginning 0))
				  (beginning-of-line)
				  ;; (when (not (looking-at-p
				  ;; 			(rx (* whitespace) line-end))))
				  (save-excursion (insert ?\n))
				  (save-excursion
					(insert prefix)	; insert dummy
					(comment-line 1)
					;; (insert ?\n)
					)))))
		  (comment-dwim nil)			; move cursor into comment
		  (delete-region (point) (line-end-position))
		  (save-excursion (insert prefix str))
		  (just-one-space))))))

;; slime swank

(with-eval-after-load 'slime
  (autoload 'slime-dispatch-media-event "slime-media" nil)
  ;; (require 'slime-media)
  ;; in-case not loaded properly, I will do it myself
  (add-hook 'slime-event-hooks 'slime-dispatch-media-event)

  (add-hook 'sldb-mode-hook 'toggle-truncate-lines)

  (defun slime-c-p-c-completion-at-point ()
	#'slime-complete-symbol*)

  (util:define-keys lisp-mode-map
	""		; was `switch-to-lisp'
	(lambda () (interactive)
	  (if slime-net-processes
		  (pop-to-buffer
		   (slime-repl-buffer))
		(slime))))
  (define-key slime-mode-map
	;; was slime-interactive-eval
	(kbd "C-c C-e") 'slime-macroexpand-1)
  (define-key slime-macroexpansion-minor-mode-map
	[return] 'slime-macroexpand-1-inplace)
  (setq-default slime-lisp-implementations nil
				;; `((sbcl (,(executable-find "sbcl"))))
				))

;;; gerbil

(use-package gambit
  :disabled
  :load-path "/opt/homebrew/opt/gambit-scheme/current/share/emacs/site-lisp/"
  :init
  (autoload 'gambit-inferior-mode "gambit" "gambit package for gerbil")
  ;; (add-hook 'inferior-scheme-mode-hook #'gambit-inferior-mode)
  )

(use-package gerbil-mode
  :disabled
  :load-path
  ;; (concat (string-trim-right
  ;;          (shell-command-to-string "brew --prefix gerbil-scheme"))
  ;;   	  "/share/emacs/site-lisp/")
  "/opt/homebrew/opt/gerbil-scheme/share/emacs/site-lisp/gerbil-scheme/"
  :config
  (setq-default gerbil-program-name (executable-find "gxi"))
  (defun run-gerbil ()
    (interactive)
    (setq xscheme-prompt "> ")
    (run-scheme "gxi"))
  :init
  ;; (defvar my/gerbil-home (getenv "GERBIL_HOME"))
  ;; (let ((gerbil-program-name (concat my/gerbil-home "/bin/gxi")))
  ;;   ;; gerbil mode
  ;;   (add-to-list 'load-path (concat my/gerbil-home "/etc/"))
  ;;   (autoload 'gerbil-mode "gerbil-mode" "Gerbil editing mode." t)
  ;;   ;; gerbil tags
  ;;   ;; (add-to-list 'tags-table-list (concat my/gerbil-home "/src/TAGS"))
  ;;   (setq scheme-program-name gerbil-program-name))

  ;; (add-auto-mode 'gerbil-mode "\\.ss$")
  )

(defun my/geiser-repl--set-this-buffer-repl (r)
  "Make sure repl will persist.
No need to start repl again for every scm file visited.  Argument
R ."
  (setq-default geiser-repl--repl r))

(advice-add 'geiser-repl--set-this-buffer-repl :after
            'my/geiser-repl--set-this-buffer-repl)

;; https://gist.github.com/drewc/5f260537b7914a2b999c8a539fb48098
(defun gerbil-scheme-start-swank (file encoding)
  (format "%S\n\n"
          `(begin (import (ecraven gerbil-swank))
                  (start-swank ,file))))

(setq slime-lisp-implementations
      '((gerbil-scheme ("gxi" "-:d-" "--lang" "r7rs")
                       :init gerbil-scheme-start-swank
                       :env
                       "GERBIL_LOADPATH=~/.gerbil/pkg/github.com/ecraven/r7rs-swank/")))

(with-eval-after-load 'scheme
  ;; (pop scheme-font-lock-keywords-2)
  (add-to-list 'scheme-font-lock-keywords-2
               '("\\<\\sw+:\\>" . font-lock-builtin-face))
  (add-to-list 'scheme-font-lock-keywords-2
               '("\\<#!\\sw+\\>" . font-lock-builtin-face))
  (add-to-list 'scheme-font-lock-keywords-2
               `(,(concat
                   "(" (regexp-opt '("cons" "car" "cdr"
                                     "null?" "pair?" "number?"
                                     "error"
                                     "#f" "#t")
                                   t)
                   "\\>")
                 (1 font-lock-builtin-face)))
  (setq scheme-program-name "guile"))

(with-eval-after-load 'geiser
  (geiser-implementation-extension 'guile "scm")
  ;; (geiser-implementation-extension 'chicken "scm")
  ;; (setq-default geiser-implementations-alist
  ;;   			'(((regexp "\\.scm\\'") chicken)))
  )

;; gerbil tag table

;; Gerbil package manager generates TAGS tables for all installed packages at this path
;; (add-to-list 'tags-table-list "~/.gerbil/pkg/TAGS")
;; to generate tags for your own code by using gxtags. The invocation is very simple:
;; gxtags [-a] [-o TAGS] source-file-or-directory ...

(provide 'init-lisp)
;;; init-lisp.el ends here
