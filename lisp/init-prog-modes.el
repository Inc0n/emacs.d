;;; init-prog-modes --- -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;; All prog modes related setup are put into here.
;;;
;;; Code:

;; How to make flymake less angry
(eval-when-compile (require 'use-package))

;; avoid default "gnu" style, use more popular one
(with-eval-after-load 'cc-mode
  (setq c-default-style '((java-mode . "java")
						  (awk-mode . "awk")
						  (other . "gnu"))
		c-style-variables-are-local-p nil)

  (setq-default c-basic-offset 'set-from-style
				c-indent-level)

  (util:define-keys c-mode-map
	[tab] #'indent-for-tab-command
	;; @see
	;; http://stackoverflow.com/questions/3509919/emacs-c-opening-corresponding-header-file
	[?\C-x ?\C-o] #'ff-find-other-file)

  ;; don't use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
  (add-hook 'c-mode-common-hook 'my/common-cc-mode-setup)
  (add-hook 'c-mode-hook 'my/c-mode-setup)
  (add-hook 'c++-mode-hook 'my/c-mode-setup))

(defun my/common-cc-mode-setup ()
  "Setup shared by all languages (java/groovy/c++ ...)."
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)
  (setq-local tab-width 3)				; make sure its
  (setq-local c-basic-offset 3)

  (eldoc-mode 1)
  (when (boundp 'inferior-cling-minor-mode)
	(inferior-cling-minor-mode))
  ;; make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)

  (c-set-style "cc-mode")
  ;; (mapcar 'car c-style-alist)
  ;; indent
  ;; google "C/C++/Java code indentation in Emacs" for more advanced skills
  ;; C code:
  ;;   if(1) // press ENTER here, indent with N spaces
  (c-set-offset 'substatement 2)
  ;;   void fn() // press ENTER here, zero means no indentation
  (c-set-offset 'func-decl-cont 0))

(defun my/c-mode-setup ()
  "C/C++ only setup."

  (setq-local c-basic-offset 3)

  (setq cc-search-directories
		'("." "/usr/include" "/usr/local/include/*" "../*/include" "$WXWIN/include"))

  ;; {{ @see https://github.com/redguardtoo/cpputils-cmake
  ;; In theory, you can write your own Makefile for `flyamke-mode' without cmake.
  ;; Nobody actually does it in real world.

  ;; debugging Emacs c code
  (add-to-list 'imenu-generic-expression '(nil "^DEFUN *(\"\\([a-zA-Z0-9-]+\\)" 1))
  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior '(alignleft)))

;; This is making scrolling slower
(use-package highlight-indent-guides :ensure t
  :commands (highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'character))

(use-package zig-mode :ensure t
  :commands (zig-mode)
  :config
  (setq zig-return-to-buffer-after-format t
		zig-indent-offset 2
		zig-format-on-save nil))

(use-package nim-mode :ensure t
  :commands (nim-mode)
  :config
  (setq nim-smie-indent-dedenters nil)
  (util:define-keys nim-mode-map
	[?\C-\M-a] 'nim-nav-backward-block
	[?\C-\M-e] 'nim-nav-forward-block)
  (defun nim-mode-setup ()
	(nimsuggest-mode))
  (add-hook 'nim-mode-hook 'nim-mode-setup))

(use-package tree-sitter :ensure t
  :defer t
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (add-to-list
   'tree-sitter-major-mode-language-alist
   '(haskell-mode . haskell))
  :init
  ;; (add-hook 'prog-mode-hook 'tree-sitter-mode)
  (add-hook 'emacs-startup-hook 'global-tree-sitter-mode))

(use-package tree-sitter-ispell
  :disabled
  (defvar tree-sitter-default-text-grammar '(string comment))
  (defvar tree-sitter-text-grammar-alist
	'((js-mode string template_string comment)))

  (defun get-text-node-at-point ()
	"Get the text node at point, to each major mode grammar."
	(car					; get first valid match
	 (seq-some (lambda (type)
		         (tree-sitter-node-at-point type))
		       (alist-get major-mode tree-sitter-text-grammar-alist
			              tree-sitter-default-text-grammar))))

  (defun run-ispell-at-point ()
	"Run Ispell on text node at point if found."
	(interactive)
	(when-let ((node (get-text-node-at-point)))
	  (ispell-region
	   (tsc-node-start-position node)
	   (tsc-node-end-position node)))))

(defun tsc-forward-node ()
  (interactive)
  (let* ((this (treesit-node-at (point)))
         (sib (treesit-node-next-sibling this)))
    (let ((beg (treesit-node-start node))
          (end (treesit-node-end node)))
      (pulse-momentary-highlight-region
       beg end
       'secondary-selection)
      (goto-char (treesit-node-start sib)))))

(use-package tree-sitter-langs :ensure t
  :defer t
  :after tree-sitter)

(with-eval-after-load 'elec-pair
  (setq electric-pair-inhibit-predicate
		'electric-pair-default-inhibit
		;; there is some bugs in this predicate
		;; 'my/electric-pair-inhibit
		)
  (defun my/electric-pair-inhibit (char)
	(or (electric-pair-default-inhibit char)
		;; (and (memq major-mode '(minibuffer-inactive-mode))
		;; 	 (not (string-match "^Eval:" (buffer-string))))
		(let ((char-after (following-char)))
		  (or
		   ;; (eq (char-syntax char) (char-syntax char-after))
		   ;; input single/double quotes at the end of word
		   (and (memq char '(?\" ?\'))
				(eq (char-syntax char-after) ?w))
		   ;; I find it more often preferable not to pair when the
		   ;; same char is next.
		   (eq char char-after)
		   ;; I also find it often preferable not to pair next to a
		   ;; word.
		   (and (eq (char-charset char-after) 'ascii)
				(eq (char-syntax char-after) ?w)))))))

(with-eval-after-load 'compile
  (setq compilation-scroll-output t
        compilation-auto-jump-to-first-error 'first-known)

  ;; ANSI-escape coloring in compilation-mode
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

  ;; @see http://xugx2007.blogspot.com.au/2007/06/benjamin-rutts-emacs-c-development-tips.html
  (add-to-list 'compilation-finish-functions
			   'compilation-finish-hide-buffer-on-success))

(defun compilation-finish-hide-buffer-on-success (buffer str)
  "Bury BUFFER whose name marches STR.
This function can be re-used by other major modes after compilation."
  ;;TODO: only exit window if window was created
  (if (string-match "exited abnormally" str)
	  ;; there were errors
	  (message "compilation errors, press C-x ` to visit")
	(when (and (buffer-name buffer)
			   (string-match "*compilation*" (buffer-name buffer)))
	  ;; @see http://emacswiki.org/emacs/ModeCompile#toc2
	  (bury-buffer buffer)
	  ;; (with-selected-window (get-buffer-window buffer)
	  ;;   (delete-window))
	  ;; (winner-undo)
	  (message "NO COMPILATION ERRORS!"))))

(when t
  (setq sh-indent-statement-after-and nil))

;; structural editing package
(use-package puni :ensure t
  :commands (puni-mode)
  :config
  ;; (defun paredit-reindent-defun (&optional argument)
  ;; 	"Reindent the definition that the point is on.
  ;; If the point is in a string or a comment, fill the paragraph instead,
  ;;   and with a prefix argument, justify as well."
  ;; 	(interactive "P")
  ;; 	(if (or (paredit-in-string-p)
  ;; 			(paredit-in-comment-p))
  ;; 		(lisp-fill-paragraph argument)
  ;;       (paredit-preserving-column
  ;; 		(save-excursion
  ;;           (end-of-defun)
  ;;           (beginning-of-defun)
  ;;           (indent-sexp)))))
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

  (defun my/indent-defun ()
	(interactive)
	(save-mark-and-excursion
	  (mark-defun nil :interactive)
	  ;; indent-pp-sexp
	  (indent-region (region-beginning) (region-end))))

  ;; this is not realistic, as different languages has different key
  ;; for comment, there is not a realible and easy way to ensure all
  ;; comment keys to be bind to this function (defun
  ;; my/puni-insert-comment () (interactive) (beginning-of-line)
  ;; (unless (looking-at-p (rx line-start (+ space) line-end))
  ;; (newline)) (call-interactively #'self-insert-command))

  (util:define-keys puni-mode-map
	;; [C-m ?g ?r] (sexp-and-normal #'copy-and-paste)
	;; free up the following key binds
	[?\C-\M-a] nil						; puni-beginning-of-sexp
	[?\C-\M-e] nil						; puni-end-of-sexp
	[?\M-\(] nil						; puni-syntactic-backward-punct
	[?\M-\)] nil						; puni-syntactic-forward-punct
	[?\M-r] #'puni-raise
	[remap split-line] #'puni-split			 ; C-M-o
	[remap transpose-sexps] #'puni-transpose ; C-M-t

	;; newline just creates annoying newline
	[remap newline] #'reindent-then-newline-and-indent

	;; Moved to init-binding, text modes needs this as well.
	;; [?\C-\M-\-] #'goto-last-change
	;; [?\C-\M-=] #'goto-last-change-reverse

	[?\C-\(] #'puni-slurp-backward
	[?\C-\)] #'puni-slurp-forward
	[?\C-\{] #'puni-barf-backward
	[?\C-\}] #'puni-barf-forward
	[?\C-\M-q] #'my/indent-defun
	[?\C-\M-\;] (sexp-and-normal #'comment-or-uncomment-dwim)
	[?\C-\M-w] (sexp-and-normal #'kill-ring-save)))

(with-eval-after-load 'eldoc
  ;; multi-line message should not display too soon
  (setq eldoc-idle-delay 0.5
		eldoc-echo-area-use-multiline-p t))

(use-package rainbow-delimiters :ensure t :defer t
  :init
  (setq rainbow-delimiters-max-face-count 1
		rainbow-delimiters-pick-face-function
		;; since only 1 face needs to be fontlocked, we can optimize
		;; things with a little here:
		(lambda (depth match _loc)
		  (cond ((<= depth 0) 'rainbow-delimiters-unmatched-face)
				((not match) 'rainbow-delimiters-mismatched-face)
				(:otherwise  'rainbow-delimiters-depth-1-face)))))

(with-eval-after-load 'flymake
  (setq elisp-flymake-byte-compile-load-path
		(list "./"
			  (file-name-directory (locate-library "use-package"))
			  ;; (file-name-directory (locate-library "cl-macs"))
			  )))

(defun generic-prog-mode-hook-setup ()
  "Setup for `prog-mode'."
  (setq prettify-symbols-alist
		'(("lambda" . ?λ)
		  ;; ("Lambda" . ?Λ)
		  ("!=" . ?≠)
		  ("<=" . ?≤)
		  (">=" . ?≥)))
  (prettify-symbols-mode 1)

  (flymake-mode 1)						; check for syntax
  (subword-mode 1)
  (puni-mode 1)							; generic paredit
  (whitespace-mode 1)					; visualization of tab/space
  ;; (setq show-trailing-whitespace nil)
  (electric-pair-mode 1)			   ; auto insert pairing delimiter
  ;; (hs-minor-mode 1)					   ; code/comment fold
  ;; (auto-fill-mode 1)				   ; auto indent
  ;; show doc in minibuffer area
  (turn-on-eldoc-mode))

(add-hook 'prog-mode-hook #'generic-prog-mode-hook-setup)

(with-eval-after-load 'elec-pair
  (add-to-list 'electric-pair-pairs '(?{ . ?})))

(with-eval-after-load 'xref
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq xref-search-program 'ripgrep))

(use-package dumb-jump :ensure t
  :defer t
  :init
  ;; enable the xref backend
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 90)
  ;; xref use completing-read to select a target
  :config
  (setq dumb-jump-prefer-searcher 'rg
		dumb-jump-default-project "../")
  (setq dumb-jump-rg-search-args "--no-require-git"))


(add-auto-mode 'octave-maybe-mode "\\.m$")
(util:define-hook-setup 'octave-mode-hook
  "Set up of `octave-mode'."
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (font-lock-mode 1)
  (setq-local comment-start "%"
			  comment-add 0))

(use-package rust-mode :ensure t :defer t :mode "\\.rs\\'")

;; (autoload 'gradle-mode "gradle-mode")

(use-package julia-mode :defer t :ensure t
  :config (setq julia-indent-offset 3
				julia-arguments '("-i" "--color=no")
				julia-prompt-regexp "julia>")

  ;; (setq inf-julia-kept-output nil)
  ;; (ansi-color-apply (car inf-julia-kept-output))

  (when nil
	(defvar inf-julia-kept-output nil)
	(let* ((buf (get-buffer "*inferior-cling*"))
		   (process (get-buffer-process buf)))
	  ;; (set-process-filter process
	  ;; 					  (lambda (process output)
	  ;; 						(push output inf-julia-kept-output)))
	  ;; (comint-send-string process "123\n")
	  ;; (process-plist process)
	  (with-current-buffer buf)
	  ;; (comint-redirect-send-command-to-process
	  ;;  "123"
	  ;;  (get-buffer-create "*inf-output*")
	  ;;  process
	  ;;  nil
	  ;;  :nodisplay)
	  ))
  ;; (inf-julia-send-string-and-output "1")
  )

(use-package lua-mode :defer t :ensure t)

(use-package go-mode :ensure t :defer t)

(use-package eglot :ensure t
  :commands (eglot)
  :config
  (setq eglot-confirm-server-initiated-edits nil))

;;; Web

(require-package 'tagedit) ; paredit for html

(util:define-hook-setup 'web-mode-hook
  (setq-local my/flyspell-check-doublon nil))

(with-eval-after-load 'web-mode
  (setq web-mode-enable-auto-closing t	; auto close tag
		web-mode-enable-auto-pairing t
		web-mode-enable-css-colorization t)
  (setq web-mode-imenu-regexp-list
		'(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
		  ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
		  ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
		  ;; angular imenu
		  (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "="))))

;;; Css

(use-package rainbow-mode :ensure t :defer t)

(defun common-css-mode-setup ()
  "Css mode setup."
  (rainbow-mode 1)
  ;; (setq imenu-generic-expression
  ;; 		'((nil "^ *\\([a-zA-Z0-9&,.: _-]+\\) *{ *$" 1)
  ;; 		  ("Variable" "^ *\\$\\([a-zA-Z0-9_]+\\) *:" 1)
  ;; 		  ;; post-css mixin
  ;; 		  ("Function" "^ *@define-mixin +\\([^ ]+\\)" 1)))
  )

(use-package css-mode
  :mode ("\\.css\\'" . css-mode)
  :hook (css-mode-hook . common-css-mode-setup)
  :config (setq css-indent-offset 2))
;; node plugins can compile css into javascript
;; flymake-css is obsolete

(use-package scss-mode
  :ensure t
  :mode ("\\.scss\\'" . scss-mode)
  :hook (scss-mode-hook . common-css-mode-setup)
  :config
  ;; compile *.scss to *.css on the pot could break the project build
  (setq scss-compile-at-save nil))

;;; JavaScript

(require-package 'js-doc)
(require-package 'js2-mode)
(require-package 'rjsx-mode)


;; this has hard dependencies on lsp
(use-package tsx-mode
  :disabled
  :straight (tsx-mode.el
             :host github
             :repo "orzechowskid/tsx-mode.el"
             :type git
             :branch "emacs28"))

;; (use-package dap-mode :defer t)
;; (require 'dap-firefox)
;; (dap-firefox-setup 'force)

;; skewer doesn't support require or import
(use-package skewer-mode :ensure t :defer t
  ;; Extra:
  ;; https://sachachua.com/blog/2014/11/emacs-evaluating-javascript-css-chrome-using-skewer-mode/
  :config
  (add-hook 'skewer-js-hook
			(defun my/skewer-extra-handler ()
			  ;; Module handler
			  (insert
			   "skewer.fn.module = function(request) {
	var script = document.createElement('script');
	script.src = skewer.host + request.eval;
	script.type = 'module';
	document.body.appendChild(script);
	return {value: JSON.stringify(request.eval)};
};
")
			  ;; load project file
			  (if (file-exists-p my/skewer-project-file)
				  (progn
					(message "My skewer project setup file loaded %s"
							 my/skewer-project-file)

					(load-file my/skewer-project-file))
				(message "No skewer project file setup."))))

  ;; project file setup
  (defvar my/skewer-project-file-name "./skewer-project.el")
  (defvar-local my/skewer-project-file my/skewer-project-file-name)

  (defun my/skewer-setup-project-file (&optional arg)
	"My wrapper around `run-skewer', which loads `my/skewer-project-file' also.
This project file is just an elisp file to setup dependencies
using e.g. my/skewer-load-file."
	(interactive "p")
	(setq-local my/skewer-project-file
				(expand-file-name
				 (cl-case arg
				   (4  (read-file-name "Skewer project file: "))
				   (16 (current-buffer))
				   (otherwise my/skewer-project-file-name)))))

  (defun my/run-skewer (arg)
	(interactive "p")
	(my/skewer-setup-project-file)
	(call-interactively #'run-skewer))

  (cl-defun my/skewer-cache-file (file &key (type "script") (extra nil))
	(if (file-exists-p file)
		(message "loading %s" file)
	  (user-error "This file doesn't exist %s" file))
	(let* ((parent-dir
			(file-name-nondirectory
			 (directory-file-name default-directory)))
		   (id
			(md5 (format "%s/%s" parent-dir file))))
	  ;; host this script
	  (setf (cache-table-get id skewer-hosted-scripts)
			(with-temp-buffer
			  (insert-file-contents file)
			  (buffer-string)))
	  (skewer-eval (format "/skewer/script/%s/%s"
						   id
						   file)
				   (lambda (_) (message "%s loaded" file))
				   :type type
				   :extra extra)))

  (cl-defun my/skewer-load-file (file &key (type "script") (extra nil))
	(if (file-exists-p file)
		(message "loading %s" file)
	  (user-error "This file doesn't exist %s" file))
	(let ((parent-dir
		   (file-name-nondirectory
			(directory-file-name default-directory))))
	  (skewer-eval (format "/skewer/%s" file)
				   (lambda (_) (message "%s loaded" file))
				   :type type
				   :extra extra)))

  (defservlet skewer "text/javascript; charset=UTF-8" (path)
	(setq path (s-chop-prefix "/skewer/" path))
	(message "getting %s" path)
	(if (and path (file-exists-p path))
		(insert-file-contents path)
	  (insert "")))

  (defun httpd/skewer.js (proc &rest args)
	(with-httpd-buffer proc "text/javascript; charset=UTF-8"
	  (message "skewer.js loaded")
	  (insert-file-contents (expand-file-name "skewer.js" skewer-data-root))
	  (goto-char (point-max))
	  (run-hooks 'skewer-js-hook)))

  ;; (defun httpd/skewer/buffer (proc path &rest args)
  ;;   (with-httpd-buffer proc "text/javascript; charset=UTF-8"
  ;;     (setq path (s-chop-prefix "/skewer/buffer" path))
  ;;     (let ((buf (get-buffer path)))
  ;; 	(if buf
  ;; 		(insert-buffer buf)
  ;; 	  (insert ""))
  ;; 	(if buf
  ;; 		(message "got buffer %s" path)
  ;; 	  (message "buffer %s not found" path)))))

  (cl-defun skewer-load-file (&optional file &key (type "script") (extra nil))
	(interactive (list (buffer-file-name)))
	(if (not (file-exists-p file))
		(message "This file doesn't exist %s" file)
	  (message "loading %s" file)
	  (let ((parent-dir
			 (file-name-nondirectory
			  (directory-file-name default-directory))))
		(skewer-eval (format "/skewer/file/%s" file)
					 (lambda (_) (message "%s loaded" file))
					 :type type
					 :extra extra))))

  (defun httpd/skewer/file (proc path &rest args)
	(with-httpd-buffer proc "text/javascript; charset=UTF-8"
	  (setq path (s-chop-prefix "/skewer/file/" path))
	  (message "getting %s" path)
	  (if (and path (file-exists-p path))
		  (insert-file-contents path)
		(insert "")))))

(add-auto-mode 'js-mode "\\.ja?son$" "\\.pac$" "\\.jshintrc$")
(add-auto-mode 'rjsx-mode "\\.tsx\\'")

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;; (rx (0+ space) (or "let" "var" "const") (1+ space) (group ))
;; don't waste time on angular patterns, it's updated too frequently
(defvar js-common-imenu-regex-list
  `(("Variable" ,(format "^[ \t]*%s[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*="
						 (rx (or "let" "var" "const")))
	 2)
	("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
	("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
	;; {{ es6 beginning
	("Function" "^[ \t]*\\([A-Za-z_$][A-Za-z0-9_$]+\\)[ \t]*([a-zA-Z0-9, ]*) *\{ *$" 1) ;; es6 fn1 () { }
	("Function" "^[ \t]*\\([A-Za-z_$][A-Za-z0-9_$]+\\)[ \t]*=[ \t]*(?[a-zA-Z0-9, ]*)?[ \t]*=>" 1) ;; es6 fn1 = (e) =>
	;; }}
	))

;; js-mode imenu enhancement

(util:define-hook-setup 'js-mode-hook :mo
  (unless (derived-mode-p 'js2-mode)
	(setq imenu-generic-expression js-common-imenu-regex-list)))

(with-eval-after-load 'js-mode
  ;; '$' is part of variable name like '$item'
  (modify-syntax-entry ?$ "w" js-mode-syntax-table))

(with-eval-after-load 'js2-mode
  (util:define-keys js2-mode-map
	;; disable hot keys for elements hiding/showing
	(kbd "C-c C-e") nil
	(kbd "C-c C-s") nil
	(kbd "C-c C-f") nil
	(kbd "C-c C-t") nil
	(kbd "C-c C-o") nil
	(kbd "C-c C-w") nil)
  (setq-default ;; js2-use-font-lock-faces t
   ;; js2-mode-must-byte-compile nil
   ;; {{ comment indention in modern frontend development
   js-indent-level 2
   typescript-indent-level 2
   ;; }}
   js2-strict-trailing-comma-warning nil ; it's encouraged to use trailing comma in ES6
   js2-idle-timer-delay 0.5             ; NOT too big for real time syntax check
   js2-skip-preprocessor-directives t
   js2-strict-inconsistent-return-warning nil ; return <=> return null
   js2-bounce-indent-p t)
  (setq-default js2-additional-externs '())
  (util:define-hook-setup 'js2-mode-hook
	;; if use node.js we need nice output
	(js2-imenu-extras-mode)
	(skewer-mode)
	;; @see https://github.com/mooz/js2-mode/issues/350
	(setq forward-sexp-function nil))
  ;; }}

  ;; @see https://github.com/felipeochoa/rjsx-mode/issues/33
  (with-eval-after-load 'rjsx-mode
	;; (define-key rjsx-mode-map "<" nil)
	))

;; Latest rjsx-mode does not have indentation issue
;; @see https://emacs.stackexchange.com/questions/33536/how-to-edit-jsx-react-files-in-emacs

(use-package typescript-mode :ensure t :defer t
  :config
  (util:define-hook-setup 'typescript-mode-hook
	(setq imenu-generic-expression js-common-imenu-regex-list)))

;;; Python

;; @see https://github.com/jorgenschaefer/elpy/issues/1729#issuecomment-880045698
;; Fix exit abnormally with code 1
(use-package elpy :ensure t
  :disabled
  :defer t
  :config
  ;; use (elpy-config) to configure dependencies
  (elpy-enable)

  ;; (setq elpy-shell-command-prefix-key "C-c C-f")
  ;; If you don't like any hint or error report from elpy,
  ;; set `elpy-disable-backend-error-display' to t.
  (setq elpy-disable-backend-error-display nil)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-rpc-python-command "python3")

  (util:define-keys elpy-mode-map
	(kbd "C-c C-c") nil
	[C-return] nil))

;;; ocaml
(use-package tuareg :ensure t
  :disabled
  :defer t
  :config
  (defalias 'ocaml-mode 'tuareg-mode)
  :init

  (use-package merlin :ensure t
	:hook (tuareg-mode . merlin-mode))

  (use-package merlin-eldoc :ensure t
	:after tuareg
	:defer t
	:hook (tuareg-mode . merlin-eldoc-setup)))

(use-package python :ensure t
  :commands (python-mode)	  ; ("\\.py\\'" .)
  ;; :interpreter ("python" . python-mode)
  :config
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))

  (setq python-indent-offset 4
		python-indent-guess-indent-offset nil ; no don't please
		python-forward-sexp-function #'python-nav-forward-sexp)

  ;; http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem/3338#3338
  ;; emacs 24.4+
  ;; (setq electric-indent-chars (delq ?: electric-indent-chars))
  )

(defun elsvg-update-svg-buffer-on-save (arg)
  (interactive "P")
  (let ((svg (save-excursion
			   (beginning-of-buffer)
			   (forward-sexp)
			   (backward-sexp)
			   (eval (sexp-at-point)))))
	(pop-to-buffer (get-buffer-create "*sierpinksi*"))
	(fundamental-mode)
	(erase-buffer)
	(svg-insert-image svg))
  (image-mode))

(defun toggle-elsvg ()
  "Toggle elsvg."
  (interactive)
  (if (memq 'elsvg-update-svg-buffer-on-save after-save-hook)
	  (remove-hook 'after-save-hook 'elsvg-update-svg-buffer-on-save)
	(add-hook 'after-save-hook 'elsvg-update-svg-buffer-on-save 1 t)
	(elsvg-update-svg-buffer-on-save)))

(use-package verilog-mode
  :commands (verilog-mode)
  :config
  (setq verilog-indent-lists nil
		verilog-auto-lineup 'all
		verilog-auto-newline nil		; no newline after semicolons.
		verilog-tool 'verilog-simulator
		verilog-simulator "vvp"
		verilog-compiler "iverilog")
  (defun verilog-compile-or-simulate (arg)
	(interactive "P")
	(let ((verilog-tool
		   (if (null arg)
			   'verilog-compiler
			 (if (equal '(4) arg)
				 'verilog-simulator
			   (user-error "Unknown prefix-arg %s" arg)))))
	  (verilog-set-compile-command))))

;; (use-package vhdl-mode
;;   :config
;;   (setq flycheck-ghdl-language-standard "08"))

(provide 'init-prog-modes)
;;; init-prog-modes.el ends here
