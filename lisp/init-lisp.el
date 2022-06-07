;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

(require-package 'racket-mode)
(require-package 'geiser)
(require-package 'slime)
(require-package 'gambit)

(setq-default inferior-lisp-program ;; (executable-find "ecl")
	      "ros -Q run")

;; elisp
(defun set-up-hippie-expand-for-elisp ()
  "Set `hippie-expand' completion functions for use with Emacs Lisp."
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

;; lisp mode setups
(defun my/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (setq-local tab-width 8)
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (turn-on-eldoc-mode)
  (checkdoc-minor-mode 1)
  (cl-font-lock-built-in-mode 1)
  (push `("Alias" ,(rx (group "defalias")
		       (1+ space)
		       (group (1+ alnum)))
	  2)
        imenu-generic-expression))

(use-package smartparens :ensure t
  :disabled
  :config (require 'smartparens-config)
  :init (setq sp-base-key-bindings 'sp))
;; (customize-set-variable 'sp-base-key-bindins 'sp)

(dolist (hook '(emacs-lisp-mode-hook
		lisp-mode-hook
                racket-mode-hook
                ;;
                inferior-lisp-mode-hook
                lisp-interaction-mode-hook
                slime-repl-mode-hook
                ;; sly-mrepl-mode-hook
                ;;
                hy-mode-hook
                scheme-mode-hook
                gerbil-mode-hook))
  (add-hook hook #'my/lisp-setup))

;; racket
(with-eval-after-load 'racket-mode
  ;; this would breaks pyim (chinese input)
  ;; (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
  ;; (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
  (setq racket-images-system-viewer "qiv"))

;; slime swank

(defun gerbil-scheme-start-swank (file encoding)
  (format "%S\n\n" `(begin (import :drewc/r7rs/gerbil-swank) (start-swank ,file))))

(autoload 'slime-dispatch-media-event "slime-media" nil)

(with-eval-after-load 'slime
  ;; (require 'slime-media)
  ;; in-case not loaded properly, i will do it mysel
  (add-hook 'slime-event-hooks 'slime-dispatch-media-event)
  ;; (setq slime-enable-evaluate-in-emacs nil)
  ;;

  (setq slime-lisp-implementations
	`((sbcl (,(executable-find "sbcl")))
	  (gerbil-scheme ("gxi" "-:d-") :init gerbil-scheme-start-swank)))
  (setq slime-lisp-implementations nil))

;;; gerbil

;; gambit
(autoload 'gambit-inferior-mode "gambit" "gambit package for gerbil")
(add-hook 'inferior-scheme-mode-hook #'gambit-inferior-mode)

;; gerbil setup
(use-package gerbil
  :disabled
  :init
  (defvar my/gerbil-home (getenv "GERBIL_HOME"))
  (let ((gerbil-program-name (concat my/gerbil-home "/bin/gxi")))
    ;; gerbil mode
    (add-to-list 'load-path (concat my/gerbil-home "/etc/"))
    (autoload 'gerbil-mode "gerbil-mode" "Gerbil editing mode." t)
    ;; gerbil tags
    ;; (add-to-list 'tags-table-list (concat my/gerbil-home "/src/TAGS"))
    (setq scheme-program-name gerbil-program-name))

  (add-auto-mode 'gerbil-mode "\\.ss$"))

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
;;; init-lisp ends here