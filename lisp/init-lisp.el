;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

(require-package 'geiser)
(require-package 'slime)

(setq-default inferior-lisp-program ;; (executable-find "ecl")
			  (executable-find "sbcl"))

;; lisp mode setups

(with-eval-after-load 'sly
  ;; too intrusive, it does not co-exist with slime.
  ;; e.g. asking to remove slime on every lisp file opened.
  (require 'sly-autoloads)

  (use-package sly-stepper
	:straight
	(sly-stepeer :type git :host github
				 :repo "joaotavora/sly-stepper"
				 :files ("*.el" "*.lisp")))

  (add-hook 'sly-mode-hook
			(defun sly-mode-hook ()
			  (require 'sly-stepper-autoloads)
			  ;; no fuck off
			  (sly-symbol-completion-mode -1))))

(with-eval-after-load 'lisp-mode
  ;; 09/08/22 racket has changed my view on [()] mixing
  (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table))

(defun my/lisp-setup ()
  "Enable features useful in for Lisp mode."
  (puni-mode 1)
  (rainbow-delimiters-mode 1)
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
                ;;
                inferior-lisp-mode-hook
                lisp-interaction-mode-hook
                slime-repl-mode-hook
				sly-mrepl-mode-hook
                ;;
				scheme-mode-hook
                gerbil-mode-hook))
  (add-hook hook #'my/lisp-setup))

;; racket
(require-package 'racket-mode)
(with-eval-after-load 'racket-mode
  ;; this would breaks pyim (chinese input)
  ;; (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)
  ;; (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
  ;; (setq racket-images-system-viewer "qiv")
  (setq racket-error-context 'high
		racket-memory-limit 1024)
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
				'racket-xp-eldoc-function)
	(racket-xp-mode 1))
  (add-hook 'racket-mode-hook 'racket-mode-setup)

  (define-key racket-mode-map
	[?\C-\M-y] nil ; racket-insert-lambda
	))

;; slime swank

(defun gerbil-scheme-start-swank (file encoding)
  (format "%S\n\n" `(begin (import :drewc/r7rs/gerbil-swank) (start-swank ,file))))

(autoload 'slime-dispatch-media-event "slime-media" nil)

(with-eval-after-load 'slime
  ;; (require 'slime-media)
  ;; in-case not loaded properly, I will do it myself
  (add-hook 'slime-event-hooks 'slime-dispatch-media-event)

  (setq slime-lisp-implementations nil
		;; `((sbcl (,(executable-find "sbcl")))
		;;   (gerbil-scheme ("gxi" "-:d-") :init gerbil-scheme-start-swank))
		))

;;; gerbil

;; gambit
;; (require-package 'gambit)
;; (autoload 'gambit-inferior-mode "gambit" "gambit package for gerbil")
;; (add-hook 'inferior-scheme-mode-hook #'gambit-inferior-mode)

;; gerbil setup
(use-package gerbil-mode
  :disabled
  :load-path
  (concat (shell-command-to-string "brew --prefix gerbil-scheme")
		  "/share/emacs/site-lisp/")
  :config
  (setq gerbil-program-name (executable-find "gxi"))
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

(with-eval-after-load 'geiser
  ;; (geiser-implementation-extension 'guile "scm")
  (setq-default geiser-implementations-alist
				'(((regexp "\\.scm\\'") guile))))

;; gerbil tag table

;; Gerbil package manager generates TAGS tables for all installed packages at this path
;; (add-to-list 'tags-table-list "~/.gerbil/pkg/TAGS")
;; to generate tags for your own code by using gxtags. The invocation is very simple:
;; gxtags [-a] [-o TAGS] source-file-or-directory ...

(provide 'init-lisp)
;;; init-lisp.el ends here
