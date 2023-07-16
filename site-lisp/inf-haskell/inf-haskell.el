;;; inf-haskell.el --- Interaction with an inferior Haskell process -*- lexical-binding: t -*-

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009  Free Software Foundation, Inc.
;; Copyright (C) 2017 Vasantha Ganesh Kanniappan <vasanthaganesh.k@tuta.io>

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: languages, Haskell

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A major mode for the buffer that holds the inferior process
;;
;; Todo:
;;
;; - Check out Shim for ideas.
;; - i-h-load-buffer and i-h-send-region.
;; - implement :set -package
;;
;; -> Make font lock work for strings, directories, hyperlinks
;; -> Make font lock work for key words???
;;
;; Known issues:
;;
;; [1,2] at prompt will cause unmatched [] error from
;; `comint-completion-at-point' which invokes
;; `comint-filename-completion' from
;; `comint-dynamic-complete-functions'
;;
;;; Code:

(require 'comint)
(require 'shell)             ; For directory tracking.
;; (require 'etags)
(require 'compile)
(require 'haskell-customize)
(require 'cl-lib)
(require 'haskell-string)

(defgroup inf-haskell nil
  "Settings for REPL interaction via `inf-haskell-mode'."
  :link '(custom-manual "(haskell-mode)inf-haskell-mode")
  :prefix "inf-haskell-"
  :prefix "haskell-"
  :group 'haskell)

(defcustom inf-haskell-hook nil
  "The hook that is called after starting inf-haskell."
  :type 'hook)

;; (defcustom inf-haskell-show-debug-tips t
;;   "Will show debug tips if non-nil on inf-haskell process start."
;;   :type 'boolean)

(defvar inf-haskell-set+c-p nil
  "Set `:set +c` in `inf-haskell-init' if non-nil.")

(defun haskell-program-name-with-args ()
  "Return the command with the arguments to start the repl based on the
directory structure."
  (cl-ecase (haskell-process-type)
    (ghci       (cond ((eq system-type 'cygwin) `("ghcii.sh" ,@haskell-process-args-ghci))
                      (t (append
                          (if (listp haskell-process-path-ghci)
                              haskell-process-path-ghci
                            (list haskell-process-path-ghci))
                          haskell-process-args-ghci))))
    (cabal-repl `(,haskell-process-path-cabal "repl" ,@haskell-process-args-cabal-repl))
    (stack-ghci `(,haskell-process-path-stack "ghci" ,@haskell-process-args-stack-ghci))))

(defconst inf-haskell-info-xref-re
  "-- Defined at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?$")

(defconst inf-haskell-module-re
  "-- Defined in \\(.+\\)$"
  "Regular expression for matching module names in :info.")

(defvar inf-haskell-multiline-prompt-re
  "^\\*?[[:upper:]][\\._[:alnum:]]*\\(?: \\*?[[:upper:]][\\._[:alnum:]]*\\)*| "
  "Regular expression for matching multiline prompt.
the one inside :{ ... :} blocks.")

;; (defconst inf-haskell-error-regexp
;;   )
(defconst inf-haskell-keywords
  `(("error:" (0 'error))
	(,(regexp-opt '("Exception:"))
	 (0 'font-lock-warning-face))
	(,(rx ?‘ (group (and upper (+? anychar))) ?’)	; types
	 (1 'haskell-type-face))))

(rx-let ((src-rx (or "<interactive>"
					 ;; (and "/")
                     (+? (not whitespace)))))
  (defvar inf-haskell-error-regexp
	(rx (group src-rx) ":"
		(group (+ num)) ":"			; line num
		(group (+ num)) "-" (group (+ num)) ": "
		(or "error"
			"Exception")
		":"))

  (defconst inf-haskell-error-regexp-alist
	`(;; Format of error messages used by GHCi.
	  ;; <file path>:<line>:<col>: warning
	  ;; <file path>:<line>:<col start>:<col end>: warning
	  (,(rx (group src-rx) ":"
			(group (+ num)) ":"				  ; line num
			(group (+ num)) "-" (group (+ num)) ; col range
			(? (and ": " (group "warning"))))
	   1 2 (3 . 4) (5) nil (5 '(face nil font-lock-multiline t)))
	  ;; <file path>:(<beg line>,<beg col>)-(<end line>,<end col>): warning
	  (,(rx (group src-rx) ":"
			"(" (group (+ num)) "," (group (+ num)) ")-" ; start pos
			"(" (group (+ num)) "," (group (+ num)) ")"	; end pos
			(? (and ": " (group "warning"))))
	   ;; "^\\(.+?\\):\\([0-9]+\\):\\(\\([0-9]+\\):\\)?\\( \\|\n *\\)\\([Ww]arning\\)?"
	   1 (2 . 4) (3 . 5) (6) nil (6 '(face nil font-lock-multiline t)))
	  ;;
	  ;; <file path>:<line>:<col>
	  (,(rx bol
			(group src-rx) ":"			; src file path
			(group (+ num)) ":"			; start pos
			(group (+ num))				; end pos
			(? (and ": " (group "warning"))))
	   1 2 3 (4) nil (4 'bold)
	   )
	  (,(rx (group src-rx) ":"			; src file path
			(group (+ num)) ":"			; start pos
			(group (+ num))				; end pos
			)
	   1 2 3 1				   ; 0 = info, 2 = real error, 1 = warning
	   )
	  ("^\\*\\*\\* Exception: \\(.+?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\)): .*"
	   1 ,@(if (fboundp 'compilation-fake-loc) '((2 . 4) (3 . 5))
			 '(2 3)))
	  ;; GHCi uses two different forms for line/col ranges, depending
	  ;; on whether it's all on the same line or not :-( In Emacs-23,
	  ;; I could use explicitly numbered subgroups to merge the two
	  ;; patterns.
	  ("^\\*\\*\\* Exception: \\(.+?\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\): .*"
	   1 2 ,(if (fboundp 'compilation-fake-loc) '(3 . 4) 3))
	  ;; Info messages.  Not errors per se.
	  ,@(when (fboundp 'compilation-fake-loc)
		  `(;; Other GHCi patterns used in type errors.
			("^[ \t]+at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)$"
			 1 2 (3 . 4) 0)
			;; Foo.hs:318:80:
			;;     Ambiguous occurrence `Bar'
			;;     It could refer to either `Bar', defined at Zork.hs:311:5
			;;                  or `Bar', imported from Bars at Frob.hs:32:0-16
			;;                       (defined at Location.hs:97:5)
			("[ (]defined at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\))?$" 1 2 3 0)
			("imported from .* at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)$"
			 1 2 (3 . 4) 0))))
	"Regexps for error messages generated by inferior Haskell processes.
The format should be the same as for `compilation-error-regexp-alist'."))

(defconst haskell-prompt-regexp
  ;; "^[[:alnum:].*_() |λ]*> "
  "^ghci> "
  "Ignore everything before the first '> '.
This allows us to
correctly interpret multi-line input even when modules are imported.")

(defvar inf-haskell-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-z" 'run-haskell)
    (define-key map "\C-c\C-l" 'inf-haskell-load-file)
    (define-key map "\C-c\C-r" 'inf-haskell-reload)
	(define-key map (kbd "C-M-x") 'inf-haskell-eval-defun)
    map))

(defsubst inf-haskell--in-comment-p ()
  (nth 4 (syntax-ppss)))

(defun inf-haskell-load-package (package &optional process)
  "PACKAGE."
  (interactive "sPackage: ")
  (inf-haskell-get-result
   (format ":set -package %s" package)
   process))

(defun inferior-haskell-type (sym &optional process)
  "Find the type of SYM with `:type' ghci feature."
  (inf-haskell-get-result (format ":type %s" sym) process))

(defun inf-haskell-eldoc-at-point (callback &rest _ignored)
  "Use CALLBACK on haskell type of symbol at point.
Intended for `eldoc-documentation-functions' (which see)."
  (unless (inf-haskell--in-comment-p)
	(when-let* ((sym (thing-at-point 'symbol))
				(docstring (inferior-haskell-type
							sym
							(inf-haskell-get-process?))))
	  ;; (string-match-p
	  ;;  inf-haskell-error-regexp
	  ;;  "<interactive>:1:1-2: error: Variable not in scope: 变爻")
	  (unless
		  (string-match-p inf-haskell-error-regexp docstring)
		;; DONE: don't show error, i.e. :type not found
		(funcall callback docstring
				 ;; :thing sym
				 :face 'font-lock-variable-name-face)
		))))

(defun inf-haskell-minor-mode-init-hooks ()
  (setq-local eldoc-documentation-function
			  (lambda ()
				(inf-haskell-eldoc-at-point (eldoc--make-callback :eager))))
  (add-hook 'completion-at-point-functions
			'inf-haskell-completion-at-point nil 'local))

;; (haskell-doc-mode)
;;;###autoload
(define-minor-mode inf-haskell-minor-mode
  "Minor mode for interacting with inf-haskell from Haskell source file."
  :group 'inf-haskell
  :lighter "Inf-Haskell"
  :keymap inf-haskell-minor-mode-map
  (if inf-haskell-minor-mode
      (when (inf-haskell-get-process?)
	    (inf-haskell-minor-mode-init-hooks))
	(remove-hook 'eldoc-documentation-functions
				 'inf-haskell-eldoc-at-point
				 'local)
	;; (setq-local eldoc-documentation-function 'inf-haskell-eldoc-at-point)
	(remove-hook 'completion-at-point-functions
				 'inf-haskell-completion-at-point 'local)))

(defvar inf-haskell-mode-map
  (let ((map (make-sparse-keymap)))
	;; (set-keymap-parent map comint-mode-map)
    (define-key map "\C-c\C-d" 'comint-kill-subjob)
    map))

(defun inf-haskell-kill-query-function ()
  "Uses `process-kill-buffer-query-function' to remove to be
deleted inf-haskell from `inf-haskell-buffer-alist'"
  (when (process-kill-buffer-query-function)
	(let ((project-root
		   (or default-directory
               inf-haskell-project-root
			   (error "inf-haskell-project-root is nil!"))))
	  (setq inf-haskell-buffer-alist
			(assoc-delete-all project-root
							  inf-haskell-buffer-alist
							  #'string=))
	  t)))

(define-derived-mode inf-haskell-mode comint-mode "Inf-Haskell"
  "Major mode for interacting with an inferior Haskell process."
  :group 'inf-haskell

  (setq-local paragraph-start haskell-prompt-regexp)

  (setq-local comint-input-autoexpand nil
			  comint-prompt-read-only t
			  comint-prompt-regexp haskell-prompt-regexp
			  comint-output-filter-functions
			  '(comint-truncate-buffer
				ansi-color-process-output
				comint-postoutput-scroll-to-bottom
				comint-watch-for-password-prompt))
  ;; (setq-local )
  (setq-local kill-buffer-query-functions #'inf-haskell-kill-query-function)
  ;; Setup directory tracking.
  (setq-local shell-cd-regexp ":cd")
  (condition-case nil
      (shell-dirtrack-mode 1)
    (error ;The minor mode function may not exist or not accept an arg.
     (setq-local shell-dirtrackp t)
     (add-hook 'comint-input-filter-functions 'shell-directory-tracker
               nil 'local)))

  ;; Setup `compile' support so you can just use C-x ` and friends.
  (setq-local compilation-error-regexp-alist
              inf-haskell-error-regexp-alist
			  compilation-first-column 1) ;GHCI counts from 1
  ;; Use compilation-minor-mode but without its bindings
  ;; because things like mouse-2 are simply too annoying.
  (compilation-minor-mode 1)
  (let ((map (make-sparse-keymap)))
    (dolist (keys '([menu-bar] [follow-link]))
      ;; Preserve some of the bindings.
      (define-key map keys (lookup-key compilation-minor-mode-map keys)))
    (add-to-list 'minor-mode-overriding-map-alist
                 (cons 'compilation-minor-mode map)))
  (font-lock-add-keywords nil inf-haskell-keywords)
  (add-hook 'inf-haskell-hook 'inf-haskell-init))

(defvar-local inf-haskell-project-root nil
  "Caching project root dir for current buffer.")

(defvar inf-haskell-buffer-alist '()
  "An alist of (project-root-dir . inf-buffer).")

(defun inf-haskell-start-process (project-root)
  "Start an inferior haskell process at PROJECT-ROOT.
With universal prefix \\[universal-argument], prompts for a COMMAND,
otherwise uses `haskell-program-name-with-args'.
It runs the hook `inf-haskell-hook' after starting the process and
setting up the inf-haskell buffer."
  (setq default-directory project-root)
  (let* ((command (haskell-program-name-with-args))
		 (buf (apply 'make-comint
					 "haskell"			; TODO: unique name
					 (car command)
					 nil
					 (cdr command)))
         (proc (get-buffer-process buf)))
	(with-current-buffer buf
      (inf-haskell-mode)
	  (run-hook-with-args 'inf-haskell-hook proc))
    (push (cons project-root buf)
		  inf-haskell-buffer-alist)
	proc))

(defun inf-haskell-proc-alive-p (proc)
  "PROC for (process-live-p)."
  (not (eq 'stop (process-status proc))))

;; (defun inf-haskell-find-project-root ()
;;   (setq-local inf-haskell-project-root
;;               (or (haskell-cabal-find-dir)
;; 	              (completing-read
;; 		           "Choose: "
;; 		           (cl-adjoin
;; 		            default-directory
;; 		            (mapcar #'car inf-haskell-buffer-alist)
;; 		            :test #'string=)))))

;; (process-status (inf-haskell-process))
(defun inf-haskell-find-buffer (proj-root)
  (cdr (assoc proj-root inf-haskell-buffer-alist #'string=)))

(defun inf-haskell-get-process? (&optional proj-root force)
  "Start process if FORCE and no active process for PROJ-ROOT."
  (when (null proj-root)
	(setq proj-root
          (or inf-haskell-project-root
			  (setq-local
			   inf-haskell-project-root
			   (cond (force
					  (or (haskell-cabal-find-dir)
						  (completing-read
						   "Choose: "
						   (cl-adjoin
							default-directory
							(mapcar #'car inf-haskell-buffer-alist)
							:test #'string=))))
					 ;; (inf-haskell-find-project-root)
					 ((inf-haskell-find-buffer default-directory)
					  default-directory)
					 (t (user-error
						 "No session started for %s"
						 default-directory)))))))
  (or (let ((buf (inf-haskell-find-buffer proj-root)))
		(and (buffer-live-p buf)
			 (comint-check-proc buf)
			 (inf-haskell-proc-alive-p (get-buffer-process buf))
			 (get-buffer-process buf)))
	  (inf-haskell-start-process proj-root)))

(defun inf-haskell-get-process (&optional proj-root)
  "Get `inf-haskell-process' at PROJ-ROOT or start new process."
  (inf-haskell-get-process? proj-root :force))

;;;###autoload
(defun run-haskell ()
  "Show the inf-haskell buffer.  Start the process if needed."
  (interactive)
  (let ((proc (inf-haskell-get-process)))
    (pop-to-buffer (process-buffer proc))))

(defun inf-haskell-init (proc)
  (inf-haskell-get-result ":set -v1" proc)
  (when inf-haskell-set+c-p
    (inf-haskell-get-result ":set +c" proc)))

;; haskell-mode override
;; (defalias 'inferior-haskell-get-result 'inf-haskell-get-result)

(defun inf-haskell-echo (str &optional process)
  "Insert STR to PROCESS buffer."
  (let ((process
		 (or process (inf-haskell-get-process))))
	(with-current-buffer (process-buffer process)
	  (insert str))))

(defun inf-haskell-send-string (string &optional process)
  "Send STRING to inferior Haskell PROCESS."
  (let ((process
		 (or process (inf-haskell-get-process)))
		(comint-preoutput-filter-functions
		 (list
		  (let ((once nil))
			(lambda (str)
			  ;; (message "processing: %s" str)
			  (if once
				  str
				(setq once t)
				(if (string-prefix-p "\n" str)
					str
				  (concat "\n" str))))))))
	(with-current-buffer (process-buffer process)
	  ;; (comint-skip-input)
	  (comint-kill-input))
	;; (message "start processing: %s" string)
    (comint-send-string process string)
    (comint-send-string process "\n")))

(defun inf-haskell-load-file (file-name &optional process)
  "Load file FILE-NAME to the shell.  Use PROCESS."
  (interactive (list (or (buffer-file-name)
						 (read-file-name "File to load: "))))
  (inf-haskell-send-string (format ":load %s" file-name) process))

(defun inf-haskell-reload (&optional process)
  "Send :reload to haskell PROCESS."
  (interactive)
  (inf-haskell-send-string ":reload" process))

(defun inf-haskell-send-region (start end)
  "Send region at START to END to inf haskell."
  ;; Flash region
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer 0.2 nil 'delete-overlay overlay))
  ;; (haskell-interactive-mode-set-prompt exp)
  ;; (with-current-buffer (haskell-interactive-buffer)
  ;; 	(haskell-interactive-mode-return))
  (let* ((expr (s-trim-left (buffer-substring start end)))
		 (output
		  (inf-haskell-get-multiline-results expr)))
	output))

(defun inf-haskell-eval-defun (arg)
  "Send defun region to inf-haskell.
If ARG is non-nil, insert the result back into buffer."
  (interactive "P")
  (save-excursion
	(let* ((result (if (inf-haskell--in-comment-p)
					   (inf-haskell-send-region
						(save-excursion
						  (beginning-of-line)
						  (search-forward comment-start))
						(line-end-position))
					 (unless (region-active-p)
					   (mark-paragraph))
					 (prog1 (inf-haskell-send-region
							 (region-beginning)
							 (region-end))
					   (deactivate-mark))))
		   (result
			(if (string-empty-p result)
				"Empty output."
			  result)))
	  (if arg
		  (insert result)
		(message result)))))

;;; completions

(defun inf-haskell--last-prompt ()
  "Return last prompt start and end."
  comint-last-prompt)

(defun inf-haskell-get-completions (process input)
  "Do completion at point using PROCESS for IMPORT or INPUT.
When IMPORT is non-nil takes precedence over INPUT for
completion."
  ;; (message "getting %s" input)
  (let ((rawstr
         (s-trim
          (inf-haskell-get-result
           (format ":complete repl 5 \"%s\"" input)
		   process))))
    (when rawstr
      ;; parse REPL response if any
      (let* ((s1 (split-string rawstr "\r?\n" t))
             (cs (remove
				  nil
				  (mapcar #'haskell-string-literal-decode
						  (cdr s1))))
             (h0 (car s1))) ;; "<limit count> <all count> <unused string>"
        (save-match-data
		  (unless (string-match
                   (rx (group (+ digit))
					   whitespace
					   (group (+ digit))
					   whitespace
					   (group ?\" (* nonl) ?\"))
                   h0)
            (error "Invalid `:complete' response '%s'" h0))
          (let ((cnt1 (match-string 1 h0))
				(h1 (haskell-string-literal-decode (match-string 3 h0))))
			(unless (= (string-to-number cnt1) (length cs))
              (error "Lengths inconsistent in `:complete' response"))
			(cons h1 cs)))))))

(defun inf-haskell-completion-at-point (&optional process)
  "Function for `completion-at-point-functions' in `haskell-comint-mode'.
Optional argument PROCESS forces completions to be retrieved
using that one instead of current buffer's process."
  (interactive)
  (unless (inf-haskell--in-comment-p)
	(unless process
	  (setq process (inf-haskell-get-process?)))
	(let ((line-start (if (derived-mode-p 'haskell-comint-mode)
                          ;; Working on a shell buffer: use prompt end.
                          (cdr (inf-haskell--last-prompt))
						(line-beginning-position)))
          (start nil)
          (end nil))
	  (cond ((string-match-p
			  (rx (* space) word-start "import" word-end space)
			  (buffer-substring-no-properties line-start (point)))
			 ;; Import statement
			 (setq start line-start
				   end (point)))
			((pcase-let ((`(,start% . ,end%)
						  (bounds-of-thing-at-point 'symbol)))
			   (setq start start%
					 end end%)
			   t)))
      (and start
		   end
		   (list start end
				 (inf-haskell-get-completions
				  process (buffer-substring start end))
				 ;; (completion-table-dynamic
				 ;;  (apply-partially #'inf-haskell-get-completions process))
				 )))))

;;; Get results

(defvar inf-haskell-comint-prompt-regexp
  (concat
   "\r?\n?"
   ;; Remove initial caret from calculated regexp
   (rx (+? (not space))
	   "> ")
   ;; (replace-regexp-in-string
   ;; 	(rx string-start ?^) ""
   ;; 	haskell-prompt-regexp)
   (rx eos))
  "Regexp used to determine prompt location, in order to extract output.")

(defun inf-haskell-send-string-no-output (string
										  &optional
										  process additional-filter)
  "Send STRING to PROCESS and inhibit output.
Return the output.  ADDITIONAL-FILTER is applied before output is
processed if specified."
  (let* ((process (or process (inf-haskell-get-process)))
		 (buf (process-buffer process)))
	(with-current-buffer buf
	  (save-match-data
		(let* ((output "")
			   (comint-preoutput-filter-functions
				(list (if additional-filter
						  (lambda (str)
							(setq output
								  (concat output (funcall additional-filter str)))
							"")
						(lambda (str)
						  (setq output (concat output str))
						  ""))))
			   (inhibit-quit t)
			   )
		  (or (with-local-quit
				(comint-send-string process string)
				(comint-send-string process "\n")
				(while (not (string-match
							 ;; XXX: It seems on macOS an extra
							 ;; carriage return is attached at the end
							 ;; of output, this handles that too.
							 inf-haskell-comint-prompt-regexp
							 output))
				  ;; Wait until match
				  (accept-process-output process))
				;; (s-trim-left)
				(replace-regexp-in-string
				 "ghci| " ""
				 (substring output 0 (match-beginning 0))
				 :fixedcase :literal))
			  (with-current-buffer buf
				(comint-interrupt-subjob))))))))

(defun inf-haskell-get-multiline-results (expr &optional process)
  (inf-haskell-get-result
   (format ":{\n%s\n:}" expr)	   ; wrap expr in multiline commands
   process
   ;; (lambda (str)
   ;; 	 ;; (message "processing: %s" str)
   ;; 	 (if (string-match-p "ghci| " str)
   ;; 		 ""
   ;; 	   str))
   ))

(defun inf-haskell-get-result (expr &optional process
										additional-filter)
  "Getting result of EXPR from PROCESS w/o using prompt regexp.
Check `inf-haskell-send-string-no-output' for ADDITIONAL-FILTER.
This is useful, for non regular prompt such as context tracking
ones."
  (let* ((eoo-str "---end of output---")
		 (inf-haskell-comint-prompt-regexp
		  ;; TODO: use this as default
		  (concat (rx (? "\n")
					  (+? not-newline))
				  (regexp-quote eoo-str)))
		 (wrapped (format "%s\n%s"
						  expr
						  (format "putStr %S" eoo-str))))
	;; (message "init processing: %s" wrapped)
	(inf-haskell-send-string-no-output
	 wrapped process
	 ;; (lambda (str)
	 ;;   (message "processing: %s" str)
	 ;;   str)
	 additional-filter
	 )))

;; (inf-haskell-get-result "1")
;; (inf-haskell-get-result (format ":complete repl 5 \"zip\""))
;; (inf-haskell-get-result "[1,2]")

(provide 'inf-haskell)
;;; inf-haskell.el ends here
