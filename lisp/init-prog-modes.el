;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;; avoid default "gnu" style, use more popular one
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "gnu")))

(setq-default c-basic-offset 4)

(defun my/common-cc-mode-setup ()
  "Setup shared by all languages (java/groovy/c++ ...)."
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)
  (setq-local tab-width 4)		; make sure its 4

  (eldoc-mode 1)

  ;; make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)

  (c-set-style "awk")
  ;; (mapcar 'car c-style-alist)
  ;; indent
  ;; google "C/C++/Java code indentation in Emacs" for more advanced skills
  ;; C code:
  ;;   if(1) // press ENTER here, indent with 4 spaces
  (c-set-offset 'substatement 4)
  ;;   void fn() // press ENTER here, zero means no indentation
  (c-set-offset 'func-decl-cont 0))

(defun my/c-mode-setup ()
  "C/C++ only setup."
  ;; @see http://stackoverflow.com/questions/3509919/ \
  ;; emacs-c-opening-corresponding-header-file
  (local-set-key (kbd "C-x C-o") 'ff-find-other-file)

  (add-to-list 'company-backends 'company-cmake)
  (add-to-list 'company-backends 'company-c-headers)

  (setq cc-search-directories
        '("." "/usr/include" "/usr/local/include/*" "../*/include" "$WXWIN/include"))

  ;; {{ @see https://github.com/redguardtoo/cpputils-cmake
  ;; In theory, you can write your own Makefile for `flyamke-mode' without cmake.
  ;; Nobody actually does it in real world.

  ;; debugging Emacs c code
  (add-to-list 'imenu-generic-expression '(nil "^DEFUN *(\"\\([a-zA-Z0-9-]+\\)" 1))
  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior '(alignleft)))

;; don't use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
(add-hook 'c-mode-common-hook 'my/common-cc-mode-setup)
(add-hook 'c-mode-hook 'my/c-mode-setup)
(add-hook 'c++-mode-hook 'my/c-mode-setup)

(use-package zig-mode :defer t :ensure t)

(use-package nim-mode
  :defer t
  ;; :init
  :config
  (setq nim-smie-indent-dedenters nil)
  (define-keys nim-mode-map
    [?\C-\M-a] 'nim-nav-backward-block
     [?\C-\M-e] 'nim-nav-forward-block)
  (defun nim-mode-setup ()
    nil)
  (add-hook 'nim-mode-hook 'nim-mode-setup))

;; mac m1 doesn't support this yet
(use-package tree-sitter :ensure t
  :config
  (global-tree-sitter-mode 1)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs :ensure t)

(with-eval-after-load 'elec-pair
  (setq electric-pair-inhibit-predicate
        (defun my/electric-pair-inhibit (char)
          ;; (electric-pair-conservative-inhibit char)
          (or (electric-pair-default-inhibit char)
              (and (memq major-mode '(minibuffer-inactive-mode))
		   (not (string-match "^Eval:" (buffer-string))))
              (let ((char-after (following-char)))
	        (eq (char-syntax char) (char-syntax char-after))
	        ;; input single/double quotes at the end of word
	        (and (memq char '(?\" ?\'))
                     char-after
                     (eq (char-syntax char-after) ?w))
	        ;; I find it more often preferable not to pair when the
	        ;; same char is next.
	        (eq char char-after)
	        ;; Don't pair up when we insert the second of "" or of ((.
	        ;; (and (eq char ?\") (eq char char-after))
                ;; I also find it often preferable not to pair next to a word.
                (and (eq (char-charset char-after) 'ascii)
                     (eq (char-syntax char-after) ?w)))))))

(defun my-prog-nuke-trailing-whitespace ()
  "Only operate in the visible region of the window.
With exception to the current line."
  (when (derived-mode-p 'prog-mode)
    (let ((win-beg (window-start))
          (win-end (window-end))
          (line-beg (line-beginning-position))
          (line-end (line-end-position)))
      (if (and (not (or (< line-beg win-beg)
                        (> line-end win-end)))
               (evil-insert-state-p))
          (progn (delete-trailing-whitespace win-beg line-beg)
                 (delete-trailing-whitespace line-end win-end))
        (delete-trailing-whitespace win-beg win-end)))))

;; (add-hook 'before-save-hook 'my-prog-nuke-trailing-whitespace)

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

;; @see http://xugx2007.blogspot.com.au/2007/06/benjamin-rutts-emacs-c-development-tips.html
(add-to-list 'compilation-finish-functions 'compilation-finish-hide-buffer-on-success)

(use-package paredit-everywhere :ensure t :defer t)

(with-eval-after-load 'eldoc
  ;; multi-line message should not display too soon
  (setq eldoc-idle-delay 0.5
	eldoc-echo-area-use-multiline-p t))

(use-package rainbow-delimiters :ensure t :defer t
  :init (setq rainbow-delimiters-max-face-count 1))

(defun generic-prog-mode-hook-setup ()
  "My generic `prog-mode-hook' setup function."
  (company-ispell-setup)

  (setq prettify-symbols-alist
        `(,@prettify-symbols-alist
          ("lambda" . ?λ)))
  (prettify-symbols-mode 1)

  (unless (buffer-file-temp-p)
    ;; Selectively enable flycheck-mode
    (unless scratch-buffer
      (flycheck-mode 1)))

  ;; enable for all programming modes
  ;; http://emacsredux.com/blog/2013/04/21/camelcase-aware-editing/

  ;; (setq show-trailing-whitespace nil)
  (electric-pair-mode 1) ; auto insert pairing delimiter
  ;; (hs-minor-mode 1)      ; code/comment fold
  ;; (turn-on-auto-fill)    ; auto indent
  ;; eldoc, show API doc in minibuffer echo area
  (turn-on-eldoc-mode))

;; some major-modes do NOT inherited from prog-mode
(add-hook 'prog-mode-hook #'generic-prog-mode-hook-setup)

(add-auto-mode 'octave-maybe-mode "\\.m$")
(define-hook-setup 'octave-mode-hook
  "Set up of `octave-mode'."
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (font-lock-mode 1)
  (setq-local comment-start "%"
	      comment-add 0))

(use-package rust-mode :defer t :ensure t :mode "\\.rs\\'")

(use-package dart-mode :defer t :ensure t)
(use-package kotlin-mode :defer t :ensure t)
(use-package groovy-mode :defer t :ensure t) ; gradle syntax highlighting
(local-require 'gradle-mode)

(use-package julia-mode :defer t :ensure t
  :config (setq julia-indent-offset 3))

(use-package lua-mode :defer t :ensure t)
;; @see http://lua-users.org/wiki/LuaStyleGuide
;; (define-hook-setup 'lua-mode-hook
;;   "Set up lua script."
;;   (unless (buffer-file-temp-p)
;;     (setq-local imenu-generic-expression
;;                 '(("Variable" "^ *\\([a-zA-Z0-9_.]+\\) *= *{ *[^ ]*$" 1)
;;                   ("Function" "function +\\([^ (]+\\).*$" 1)
;;                   ("Module" "^ *module +\\([^ ]+\\) *$" 1)
;;                   ("Variable" "^ *local +\\([^ ]+\\).*$" 1)))))

(use-package lsp-mode :ensure t
  :defer t
  :init (setq lsp-keymap-prefix "C-c l"))

(use-package qml-mode :ensure t)

(use-package lsp-ui :ensure t
  :defer t
  :init
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-doc-show-with-mouse nil)
  ;; (custom-set-faces
  ;;  '(lsp-ui-doc-header ((t (:weight bold)))))
  ;; (add-hook 'lsp-ui-doc-frame-hook)
  (add-hook 'lsp-ui-doc-frame-mode-hook
            (defun lsp-ui-doc-frame-setup ()
              (setq-local display-line-numbers nil))))

;;; Web

(require-package 'websocket) ; for debug debugging of browsers
(require-package 'tagedit) ; paredit for html

(define-hook-setup 'web-mode-hook
  (unless (buffer-file-temp-p)
    (setq-local my/flyspell-check-doublon nil)
    (remove-hook 'yas-after-exit-snippet-hook 'web-mode-yasnippet-exit-hook t)))

(with-eval-after-load 'web-mode
  ;; make org-mode export fail, I use evil and evil-matchit
  ;; to select text, so expand-region.el is not used
  (remove-hook 'web-mode-hook 'er/add-web-mode-expansions)
  (setq web-mode-enable-auto-closing t ; enable auto close tag in text-mode
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t)
  (setq web-mode-imenu-regexp-list
        '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
          ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
          ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
          ;; angular imenu
          (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "="))))

;;; Css

(require-package 'rainbow-mode)

(defun common-css-mode-setup ()
  "Css mode setup."
  (unless (buffer-file-temp-p)
    (rainbow-mode 1)
    (setq imenu-generic-expression
          '((nil "^ *\\([a-zA-Z0-9&,.: _-]+\\) *{ *$" 1)
            ("Variable" "^ *\\$\\([a-zA-Z0-9_]+\\) *:" 1)
            ;; post-css mixin
            ("Function" "^ *@define-mixin +\\([^ ]+\\)" 1)))))

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

;; (use-package dap-mode :defer t)
;; (require 'dap-firefox)
;; (dap-firefox-setup 'force)

;; skewer doesn't support require or import
(use-package skewer-mode
  :ensure t
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
  ;; 	    (insert-buffer buf)
  ;; 	  (insert ""))
  ;; 	(if buf
  ;; 	    (message "got buffer %s" path)
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

(add-auto-mode 'js-mode
               "\\.ja?son$"
               "\\.pac$"
               "\\.jshintrc$")
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

(defun my/common-js-setup ()
  (local-require 'js-comint)
  (subword-mode 1))

(define-hook-setup 'js-mode-hook :mo
  (when (and (not (buffer-file-temp-p))
	     (not (derived-mode-p 'js2-mode)))
    (my/common-js-setup)
    (setq imenu-generic-expression js-common-imenu-regex-list)))

(with-eval-after-load 'js-mode
  ;; '$' is part of variable name like '$item'
  (modify-syntax-entry ?$ "w" js-mode-syntax-table))

(with-eval-after-load 'js2-mode
  (define-keys js2-mode-map
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
  (define-hook-setup 'js2-mode-hook
    (unless (buffer-file-temp-p)
      (my/common-js-setup)
      ;; if use node.js we need nice output
      (js2-imenu-extras-mode)
      (skewer-mode)

      ;; @see https://github.com/mooz/js2-mode/issues/350
      ;; (setq forward-sexp-function nil)
      ))
  ;; }}

  ;; @see https://github.com/felipeochoa/rjsx-mode/issues/33
  (with-eval-after-load 'rjsx-mode
    ;; (define-key rjsx-mode-map "<" nil)
    ))

;; Latest rjsx-mode does not have indentation issue
;; @see https://emacs.stackexchange.com/questions/33536/how-to-edit-jsx-react-files-in-emacs

(use-package typescript-mode :ensure t
  :config 
  (define-hook-setup 'typescript-mode-hook
    (setq imenu-generic-expression js-common-imenu-regex-list)))

;;; Python 

;; @see https://github.com/jorgenschaefer/elpy/issues/1729#issuecomment-880045698
;; Fix exit abnormally with code 1
(use-package elpy :ensure t)

(use-package python :ensure t
  :defer t
  ;; :mode ("\\.py\\'" . python-mode)
  ;; :interpreter ("python" . python-mode)
  :config
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
  ;; run command `pip install jedi flake9` in shell,
  ;; or just check https://github.com/jorgenschaefer/elpy
  (elpy-enable)

  ;; (setq elpy-shell-command-prefix-key "C-c C-f")
  ;; If you don't like any hint or error report from elpy,
  ;; set `elpy-disable-backend-error-display' to t.
  (setq elpy-disable-backend-error-display nil)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-rpc-python-command "python3")
  (setq-default python-indent-offset 4
                python-indent 4)
  (setq python-forward-sexp-function #'python-nav-forward-sexp)

  (define-keys elpy-mode-map
    (kbd "C-c C-c") nil
    [C-return] nil)
  
  ;; http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem/3338#3338
  ;; emacs 24.4+
  (setq electric-indent-chars (delq ?: electric-indent-chars)))

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

(provide 'init-prog-modes)
;;; init-prog-modes ends here
