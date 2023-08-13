;;; init-misc --- miscellaneous -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; Miscellaneous configurations

;;; Code:

(require-package 'pos-tip)
(autoload 'popup-tip "popup")
(autoload 'scrap-interface "scrap")

(setq message-log-max 700)				; uses less space
;; Set `auto-window-vscroll' to nil to avoid triggering `format-mode-line'.
(setq auto-window-vscroll nil)
;; (setq scroll-step 0
;;		 scroll-conservatively 0)

(with-eval-after-load 'midnight
  ;; (remove-hook 'midnight-hook 'elfeed-db-save-safe)
  (autoload 'session-save-session "session")
  (defun midnight-save-setup ()
	(recentf-save-list)
	(when (boundp 'emms-history-save) (emms-history-save))
	(session-save-session))
  (add-hook 'midnight-hook 'midnight-save-setup))

;; {{ misc
(use-package emacs
  :defer 2
  :config
  ;; midnight
  ;; purges buffers which haven't been displayed in configured period
  (setq-default midnight-period (* 3600 12)) ;; 12 hours
  (midnight-mode 1)

  (mouse-wheel-mode 1)
  ;; (global-hl-line-mode +1)				; highlight current line

  (when emacs-29? (setq delete-selection-temporary-region t))

  (delete-selection-mode 1)
  (savehist-mode 1)

  (recentf-mode +1)
  (global-subword-mode +1)

  (transient-mark-mode +1)				; mark highlight

  (autoload 'server-running-p "server")
  (unless (server-running-p)
	(server-mode 1))					; use this for emacsclient -n
  (save-place-mode 1)					; save cursor places in files

  (global-auto-revert-mode 1)

  (let ((font "Menlo"))
    ;; (set-face-attribute 'default nil :height 140)
	(when (find-font (font-spec :name font))
	  (set-face-attribute 'default nil
						  :font font
						  :height 135))))

;; (setq system-time-locale "C")
(with-eval-after-load 'imenu
  (setq-default imenu-max-item-length 60
				imenu-auto-rescan t))

(setq-default
 ;; buffers-menu-max-size 10
 case-fold-search t
 ediff-split-window-function #'split-window-horizontally
 ediff-window-setup-function #'ediff-setup-windows-plain
 ;; grep-highlight-matches t
 ;; grep-scroll-output t
 tooltip-delay 0.5
 ;; require-final-newline t ; bad idea, could accidentally edit others' code
 ;; NO automatic new line when scrolling down at buffer bottom
 next-line-add-newlines nil
 truncate-lines nil
 truncate-partial-width-windows nil
 ;; Show a marker in the left fringe for lines not after buffer end
 indicate-empty-lines t
 line-spacing 2)

(setq mouse-yank-at-point nil
	  mouse-highlight t
	  mouse-wheel-progressive-speed nil)

;; visible-bell has some issue
;; @see https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/issues/9#issuecomment-97848938
(setq visible-bell nil 					; flashes white in dark themes
	  ring-bell-function 'ignore		; C-wheelup triggers this tons
	 )

;; trash configuration
(if (eq system-type 'darwin)
	(setq delete-by-moving-to-trash t
		  trash-directory "~/.Trash/")
  (setq delete-by-moving-to-trash nil))

;; OSX: Fix frame small gaps for --with-no-titlebar
(setq frame-resize-pixelwise t)

;; Flash the mode-line setup
;; @see https://www.emacswiki.org/emacs/AlarmBell for more

(setq save-interprogram-paste-before-kill t ; sync kill-ring and clipboard
	  confirm-kill-emacs 'y-or-n-p
	  history-delete-duplicates t)

(with-eval-after-load 'calc
  (setq calc-symbolic-mode t
		calc-angle-mode 'rad))

;; Keep dumb, so processes in comint behaves properly
(setq-default comint-terminfo-terminal 'dumb)

(setq-default tab-width 4
              ;; tabs make it difficult to use git or diff
			  indent-tabs-mode nil)
(setq tab-always-indent 'complete)

;; potentially can be made into a minor mode that wraps the current
;; indent-line-function
(defun indent-for-tab-command@move-delim (orig-func &optional arg)
  "Move cursor out of any delimiters."
  (let ((tick (buffer-chars-modified-tick))
		(point (point)))
	(funcall orig-func arg)
	(when (and
		   ;; check if buffer is not modified
		   (eq tick (buffer-chars-modified-tick))
		   ;; and cursor has not moved
		   (eq point (point))
		   ;; and char under cursor is of delim syntax
		   (memq (char-syntax (following-char)) '(?\) ?\")))
	  ;; (just-one-space 0)
	  (forward-char 1))))

(advice-add 'indent-for-tab-command :around
			'indent-for-tab-command@move-delim)

(defun backward-delim (&optional arg)
  "Move cursor backwards of any delimiters."
  (interactive)
  ;; and char under cursor is of delim syntax
  (when (memq (char-syntax (char-after (1- (point)))) '(?\( ?\"))
	;; (just-one-space 0)
	(forward-char 1)))

(global-set-key [backtab] 'backward-delim)

;; @see http://blog.binchen.org/posts/effective-code-navigation-for-web-development.html
;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties '(read-only t face minibuffer-prompt)
	  enable-recursive-minibuffers t)

;;;;
;; Font
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html
;;;
(defun completing-read-fonts (font)
  "`completion-read' style FONT selection."
  (interactive
   (list (completing-read
		  "Selector font: "
		  (--filter (find-font (font-spec :name it))
					'("DejaVu Sans Mono"
					  ;; "Bitstream Vera Sans Mono"
					  ;; "TerminessTTFNerdFont"
					  "Inconsolata"
					  ;; "Liberation Mono"
					  "Fira Code"
					  "Source Code Pro"
					  "Ubuntu Mono"
					  "Iosevka"
					  "JetBrains Mono"
					  "Monaco"
					  "SF Mono"			; decent
					  "Menlo"			; osx default
					  "CozetteVector"
					  "Iosevka SS02"
					  "Iosevka Curly"
					  "Courier Prime"
					  "Anonymous Pro"
					  "PT Mono"
					  "Cascadia Code")))))
  (set-face-attribute 'default nil :font font)
  ;; Update fixed-pitch
  (set-face-attribute 'fixed-pitch nil :font font :height 1.0)
  ;; (set-face-attribute 'default nil :height 140 :family "Monospace")
  ;; (chinese/fix-font)
  (message "Font: %s" font))

(use-package ace-link :ensure t
  :defer t
  :config (ace-link-setup-default)
  :init (global-set-key [?\M-o] 'ace-link))

(use-package isearch
  :config
  (defun my/isearch-at-point-maybe (arg)
	"Enhanced `isearch', also respect `isearch-regexp'"
	(interactive "P")
    ;; 01/11/23 Removed redundant code for forward thing at point
	;; instead use M-s M-. instead
    ;; (cond ((equal '(4) arg)
	;;        (isearch-forward-thing-at-point))
    ;;       (:else))
	(save-excursion
	  (goto-char (window-start))
	  (isearch-forward isearch-regexp :non-recursive-edit)))

  (defun isearch-within-defun-cleanup ()
	(widen)
	(remove-hook 'isearch-mode-end-hook 'isearch-within-defun-cleanup 'local))

  (defun isearch-within-defun ()
	(interactive)
	(narrow-to-defun)
	(add-hook 'isearch-mode-end-hook 'isearch-within-defun-cleanup 0 'local)
	(isearch-forward-thing-at-point))

  (defun my/isearch-query-replace ()
	(interactive)
	(autoload 'vr/query-replace "visual-regexp")
	(when isearch-forward				; goto start of candidate
	  (isearch-repeat-backward))
	(isearch-exit)					; needs to quit before
	(call-interactively 'vr/query-replace))

  (define-key isearch-mode-map [?\M-q] #'my/isearch-query-replace)
  :init
  (setq isearch-lazy-count t			; enable match numbers count
		isearch-lazy-highlight t
		lazy-count-prefix-format "%s/%s "
		lazy-highlight-cleanup t))

(with-eval-after-load 'whitespace
  (setq whitespace-style
		'(face							; visualize things below:
		  trailing						; trailing blanks
		  ;; space-after-tab				; spaces before tab
		  indentation::tabs					; tabs at line beg

		  ;; tabs							; tabs (show by face)
		  ;; lines
		  ;; tab-mark					   ; tabs (show by symbol)
		  lines-tail					; lines go beyond `fill-column'
		  ;; empty							; empty lines
		  newline						;
		  missing-newline-at-eof))
  (setq whitespace-tab-regexp "^\\(	+\\)"))

(defun log-done ()
  "Log a TODO item to DONE with `org-time-stamp'."
  (interactive)
  (beginning-of-line)
  (when (search-forward "TODO" (line-end-position) 'no-error)
	(replace-match
	 (format "DONE %s"
			 (with-temp-buffer
			   (org-insert-time-stamp nil)
			   (buffer-string))))))

(use-package hideshow :ensure nil
  :defer t
  :init
  (defun hs/display-code-line-counts (ov)
	(when (eq 'code (overlay-get ov 'hs))
	  (overlay-put ov 'display
				   (format "%s... %d lines"
						   (save-excursion
							 (goto-char (overlay-start ov))
							 (beginning-of-defun)
							 ;; (message "%s %s"
							 ;;			 (overlay-start ov)
							 ;;			 (line-beginning-position))
							 (if (checkdoc--next-docstring)
								 (format "\n%s"
										 (buffer-substring
								  (line-beginning-position)
										  (progn (forward-sexp 1) (point))))
							   ""))
						   (count-lines (overlay-start ov)
										(overlay-end ov))))))
  (setq hs-set-up-overlay #'hs/display-code-line-counts))

;;; temp lambda

(defvar my/temp-lambda nil
  "Variable holder for temporary lambda construct.")

(defun save-temp-lambda ()
  "Save Kmacro for Lisp code."
  (interactive)
  (let ((exp-under-cursor (sexp-at-point)))
	(setf my/temp-lambda exp-under-cursor)
	(if (null exp-under-cursor)
		(message "Resetting temp lambda to `nil'!")
	  (message "New temp lambda saved. Call `run-temp-lambda' to run it.\n%S"
			   exp-under-cursor))))

(defun run-temp-lambda ()
  "Run Kmacro for Lisp code."
  (interactive)
  (message "%S" (eval-expression my/temp-lambda)))

;; turn on auto-fill-mode, don't use `text-mode-hook' because for some
;; mode (org-mode for example), this will make the exported document
;; ugly!
(add-hook 'change-log-mode-hook #'turn-on-auto-fill)
(add-hook 'cc-mode-hook #'turn-on-auto-fill)

;; {{ show email sent by `git send-email' in gnus
(with-eval-after-load 'gnus
  (when (local-require 'gnus-article-treat-patch)
	(setq gnus-article-patch-conditions
		  '("^@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@"))))
;; }}

;;; auto-save, auto-save-visited - builtin emacs >= 26.1 package
(setq auto-save-timeout 3
	  auto-save-interval 100		   ; 100 characters interval
	  ;; auto-save-default t
	  auto-save-no-message t)
(add-hook 'after-init-hook 'auto-save-mode)

(setq auto-save-visited-interval 2) ;; in seconds
(add-hook 'after-init-hook 'auto-save-visited-mode)

;; (when (local-require 'auto-save)
;;	 (add-to-list 'auto-save-exclude 'file-too-big-p t)
;;	 (add-hook 'after-init-hook 'auto-save-enable))

(with-eval-after-load 'autorevert
  (setq auto-revert-verbose t
		global-auto-revert-non-file-buffers nil))

(with-eval-after-load 'recentf
  (setq recentf-keep '(recentf-keep-default-predicate)
		recentf-max-saved-items 512
		recentf-exclude
		'("/tmp/"
		  ;; "/ssh:"
		  "/sudo:"
		  ;; "recentf$"
		  ;; "company-statistics-cache\\.el$"
		  ".emacs.d/elpa"
		  "share/emacs"					; make work also on Mac Emacs
		  ;; ctags
		  "/TAGS$"
		  ;; global
		  "/GTAGS$"
		  "/GRAGS$"
		  "/GPATH$"
		  ;; binary
		  "\\.mkv$"
		  "\\.mp[34]$"
		  "\\.avi$"
		  "\\.wav$"
		  "\\.docx?$"
		  "\\.xlsx?$"
		  ;; sub-titles
		  "\\.sub$"
		  "\\.srt$"
		  "\\.ass$")))

;; {{ start dictionary lookup
;; use below commands to create dictionary
;; mkdir -p ~/.stardict/dic
;; check out for stardict dictionary resources
;; https://kdr2.com/resource/stardict.html
;;
(with-eval-after-load 'sdcv
  (setq sdcv-dictionary-simple-list
		'("Webster's Revised Unabridged Dictionary (1913)"
		  "汉语大词典 离线版"))
  (setq sdcv-dictionary-complete-list sdcv-dictionary-simple-list))
;; }}

;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(add-hook 'minibuffer-setup-hook
		  (defun my/minibuffer-setup-hook ()
			(when (bound-and-true-p vertico-buffer-mode)
			  (setq-local display-line-numbers nil))
			;; (setq gc-cons-threshold most-positive-fixnum)
			))
(remove-hook 'minibuffer-exit-hook
			 (defun my/minibuffer-exit-hook ()
			   (setq gc-cons-threshold my/normal-gc-cons-threshold)))

(use-package emms :ensure t
  :commands (emms)
  :config
  (emms-all)
  ;; (emms-minimalistic)
  ;; (emms-default-players)
  ;; Only installed player: MPV
  (setq emms-player-list '(emms-player-mpv)
		emms-info-native--id3v2-text-encodings
		'((0 . utf-8)			; was latin-0, seems to be the problem
		  (1 . utf-16) (2 . uft-16be) (3 . utf-8)))
  ;; emms-info-functions
  ;; To handle unicode properly in tag editor
  (emms-i18n-set-default-coding-system 'utf-8 'utf-8)
  (setq emms-source-file-default-directory "~/Music/playlist/"
        emms-info-asynchronously t
		emms-repeat-playlist t)
  (setq emms-mode-line-icon-enabled-p nil
		emms-mode-line-format " [♪ %s]"
		emms-mode-line-mode-line-function
		(lambda () ;; display song name only
		  (or (emms-track-get
               (emms-playlist-current-selected-track)
               'info-playing-time)
              0)
		  (let ((track (emms-playlist-current-selected-track)))
            (format
		     emms-mode-line-format
		     (or (emms-track-get track 'info-title)
		         (file-name-base
                  (emms-track-get track 'name)))
		     ;; emms-playing-time-string
		     ))))
  (util:define-keys emms-playlist-mode-map
	"e" 'emms-tag-editor-edit
	"g" 'emms-pause
	"N" 'emms-next
	"P" 'emms-previous
	"n" 'next-line
	"p" 'previous-line)
  (emms-playing-time-mode 1)

  (with-eval-after-load 'emms-tag-editor
	(setcar 							; id3v2 installed not mid3v2
	 (cdr (assoc "mp3" emms-tag-editor-tagfile-functions))
	 "id3v2")))

;; (emms-track 'playlist (expand-file-name file))
;; (emms-playlist-current-selected-track)

;; {{ emmet (auto-complete html tags)
;; @see https://github.com/rooney/zencoding for original tutorial
;; @see https://github.com/smihica/emmet for new tutorial
;; C-j or C-return to expand the line
(require-package 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'rjsx-mode-hook  'emmet-mode)
;; }}

(with-eval-after-load 'grep
  ;; grep that applies to (rgrep, grep ...) setup
  (dolist (v '("auto"
			   "target"
			   "node_modules"
			   "bower_components"
			   "*dist"
			   ".sass_cache"
			   ".cache"
			   ".npm"
			   "elpa"))
	(add-to-list 'grep-find-ignored-directories v))
  (dolist (v '("*min.js"
			   "*.map"
			   "*.bundle.js"
			   "*.min.css"
			   "tags"
			   "TAGS"
			   "GTAGS"
			   "GRTAGS"
			   "GPATH"
			   "cscope.files"
			   "*.json"
			   "*.log"))
	(add-to-list 'grep-find-ignored-files v))

  ;; wgrep and rgrep, inspired by http://oremacs.com/2015/01/27/my-refactoring-workflow/
  (define-key grep-mode-map
	(kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)

  ;; display long lines in truncated style (end line with $)
  (add-hook 'grep-mode-hook (lambda () (setf truncate-lines nil))))

(use-package emacs-popup
  :init
  (defvar emacs-popup--current-frame nil)
  (defvar emacs-popup-buffer-name "*Emacs Everywhere*")

  (defun emacs-popup-delete-frame ()
	(when (and emacs-popup--current-frame
			   (framep emacs-popup--current-frame)
			   (frame-live-p emacs-popup--current-frame))
	  (remove-hook 'delete-frame-functions 'emacs-popup-finish :local)
	  (let ((copy emacs-popup--current-frame))
		;; make a copy to ensure won't trap in loop of the hooks
		(setq emacs-popup--current-frame nil)
		(delete-frame copy))))

  (defun emacs-popup-finish ()
	(interactive)
	(when-let ((buf (get-buffer emacs-popup-buffer-name)))
	  (when (buffer-live-p buf)
		(clipboard-kill-ring-save
		 (point-min)
		 (point-max))
		;; prevent loop hell
		;; (remove-hook 'kill-buffer-hook 'emacs-popup-delete-frame :local)
		(kill-buffer emacs-popup-buffer-name)))
	;; prevent loop hell
	;; (emacs-popup-delete-frame)
	)

  (defvar emacs-popup-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map "" 'emacs-popup-finish)
	  map))

  (define-minor-mode emacs-popup-mode
	""
	:global nil
	:init-value nil
	:keymap emacs-popup-mode-map
	(if emacs-popup-mode
		(progn
		  (add-hook 'kill-buffer-hook 'emacs-popup-delete-frame
					0 :local)
		  (add-hook 'delete-frame-functions 'emacs-popup-finish
					0 :local))))

  (defun emacs-popup ()
	;; (interactive)
	;; (remove-hook 'delete-frame-functions 'ea-on-delete)
	(emacs-popup-delete-frame)
	(setq emacs-popup--current-frame (make-frame))
	(select-frame emacs-popup--current-frame)
	;; (select-frame-set-input-focus emacs-popup--current-frame)
	(switch-to-buffer emacs-popup-buffer-name)
	(emacs-popup-mode 1)
	)
  (provide 'emacs-popup))

;; {{ https://www.emacswiki.org/emacs/EmacsSession better than "desktop.el" or "savehist".
;; Any global variable matching `session-globals-regexp' is saved automatically.
(use-package session :ensure t
  :defer t
  :config
  (setq session-save-file (my/emacs-d ".session"))
  (setq session-globals-max-size 512)
  (setq session-globals-max-string (* 4 1024)) ; can store 4Mb string

  (setq session-globals-include '((kill-ring 100)
								  (session-file-alist 100 t)
								  (file-name-history 200)
								  search-ring regexp-search-ring))
  (setq session-save-file-coding-system 'utf-8))

;;

(defun my/insert-date (prefix)
  "Insert the current date.	 With single PREFIX, use ISO format.
With two PREFIX arguments, write out the day and month name."
  (interactive "P")
  (insert
   (format-time-string
	(cond ((not prefix) "%D")
		  ((equal prefix '(4)) "%Y-%m-%d")
		  ((equal prefix '(16)) "%d %B %Y")
		  (t "%a %b %d %H:%M %Z %Y")))))

(defun ascii-table ()
  "Show the ascii table in buffer."
  (interactive)
  (pop-to-buffer "*ASCII*")
  (read-only-mode -1)
  (erase-buffer)
  (insert "ASCII characters up to number 254.\n")
  (dotimes (i 255)
	(insert (format "%4d %S\n" i (format "%c" i))))
  (goto-char (point-min))
  (local-set-key "q" 'quit-window)
  (read-only-mode 1))

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Ctrl-X, u/l	to upper/lowercase regions without confirm
;; (put 'downcase-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)

(use-package pomodoro :ensure t
  :defer t
  :config
  (push '(pomodoro-mode-line-string pomodoro-mode-line-string) mode-line-format)
  (setq pomodoro-play-sounds nil		; *.wav is not installed
		pomodoro-break-time 2
		pomodoro-long-break-time 5
		pomodoro-work-time 15))

(use-package which-key :ensure t
  :defer t
  :init
  (setq which-key-allow-imprecise-window-fit t ; performance
		which-key-idle-delay 0.5
		which-key-allow-evil-operators t
		which-key-show-operator-state-maps t
		which-key-max-description-length 25
		which-key-side-window-max-height 0.25
		which-key-frame-max-height 25
		which-key-min-display-lines 2)
  (add-hook 'after-init-hook 'which-key-mode))

(use-package flycheck :ensure t
  :disabled					 ; Apparently emacs26+ flymake became good
  :defer t
  :config
  ;; Otherwise, region selection and background get mixed up
  (custom-set-faces '(flycheck-warning ((t (:background nil)))))
  :init
  ;; (when (require-package 'flycheck-color-mode-line)
  ;;   (setq flycheck-color-mode-line-face-to-color 'mode-line)
  ;;   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
  (setq flycheck-display-errors-function
		'flycheck-display-error-messages-unless-error-list))

(autoload 'golden-ratio-mode "golden-ratio")
(with-eval-after-load 'golden-ratio
  (setq golden-ratio-max-width 120
		golden-ratio-adjust-factor 1.0
		golden-ratio-auto-scale t
		golden-ratio-exclude-modes '(ediff-mode
									 xref--xref-buffer-mode
									 speedbar-mode)))

(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'project-try-makefile)
  (add-to-list 'project-find-functions 'project-try-npm)
  (add-to-list 'project-find-functions 'project-try-lisp)
  (add-to-list 'project-find-functions 'project-try-readme)

  (setq project-find-functions
        '(project-try-readme project-try-lisp project-try-npm
                             project-try-makefile project-try-vc))

  (defun project-try-readme (dir)
	"Project by readme files in, within DIR."
	(when-let ((root (locate-dominating-file dir "readme.org")))
	  (cons 'readme root))
	;; (zerop
	;;  (shell-command
	;;   (format
	;;    "find %s -iname readme.* -iname *.org -o -iname *.md"
	;;    (shell-quote-argument default-directory))))
	)

  (cl-defmethod project-root ((project (head readme)))
	"Method of getting `project-root' for makefile PROJECT."
	(cdr project))

  (defun project-try-makefile (dir)
	"My project for Makefile based projects, within DIR."
	(when-let ((root (locate-dominating-file dir "Makefile")))
	  (cons 'makefile root)))

  (cl-defmethod project-root ((project (head makefile)))
	"Method of getting `project-root' for makefile PROJECT."
	(cdr project))

  (defun project-try-npm (dir)
	"My project-try for JavaScript (Nodejs) projects.
  By locating package.json around DIR."
	(when-let ((root (and (memq major-mode
								'(js-mode js2-mode rjsx-mode))
						  (locate-dominating-file dir "package.json"))))
	  (cons 'npm root)))

  (cl-defmethod project-root ((project (head npm)))
	"Method of getting `project-root' for npm PROJECT."
	(cdr project))

  (defun project-try-lisp (dir)
	"My project-try for CommonLisp (asdf) projects.
  By locating package.json around DIR."
	(when-let ((root (and (memq major-mode '(lisp-mode))
						  (locate-dominating-file
						   dir
						   (lambda (dir)
							 (ignore-error
								 (directory-files dir nil "asd")))))))
	  (cons 'lisp root)))

  (cl-defmethod project-root ((project (head lisp)))
	"Method of getting `project-root' for Lisp PROJECT."
	(cdr project)))

(defun recover-this-file-backups ()
  "Like `recover-this-file', but using backups."
  (interactive)
  (or buffer-file-name
	  (user-error "This buffer is not visiting a file"))

  (let* ((file-name (make-backup-file-name-1 buffer-file-name))
		 (default-directory (file-name-directory file-name)))
	(find-file
	 (completing-read
	  "Backup: "
	  (directory-files
	   default-directory
	   nil
	   (regexp-quote (file-name-nondirectory file-name)))))))

;; (defun clean-backup-dir ()
;;   "Delete the files in the backup dir that are not in the list of `recentf-list'."
;;   (cl-labels ((aux (x)
;; 		   (let ((x (subst-char-in-string ?! ?/ x)))
;; 			 (substring (subst-char-in-string ?! ?/ x)
;; 				0 (- (length x) 5)))))
;; 	(mapcar (lambda (dir-pair)
;; 		  (let* ((dir (cdr dir-pair))
;; 			 (files-to-delete
;; 			  (cl-set-difference
;; 			   (mapcar #'aux (butlast (directory-files dir) 2))
;; 			   recentf-list
;; 			   :test 'string=)))
;; 		(dolist (f files-to-delete)
;; 		  (dolist (f (directory-files
;; 				  dir
;; 				  t
;; 				  (concat "^" (file-name-nondirectory f) ".*")))
;; 			(delete-file f)))
;; 		(cons (car dir-pair)
;; 			  (length files-to-delete))))
;; 		backup-directory-alist)))

(use-package simple-mark
  :init
  (defvar mark-ring-i nil)
  (defun forward-mark ()
	(if mark-ring-i
		(setq mark-ring-i (min (1- (length global-mark-ring)) (1+ mark-ring-i)))
	  (setq mark-ring-i 0))
	(goto-marker (elt global-mark-ring mark-ring-i)))
  (defun backward-mark ()
	(if (integerp mark-ring-i)
		(setq mark-ring-i (max 0 (1- mark-ring-i)))
	  (setq mark-ring-i (length global-mark-ring)))
	(goto-marker (elt global-mark-ring mark-ring-i)))
  (provide 'simple-mark))

(defun goto-marker (marker)
  (let* ((buffer (marker-buffer marker))
		 (position (marker-position marker)))
	(set-buffer buffer)
	(or (and (>= position (point-min))
			 (<= position (point-max)))
		(if widen-automatically
			(widen)
		  (error "Global mark position is outside accessible part of buffer %s"
				 (buffer-name buffer))))
	(goto-char position)
	(switch-to-buffer buffer)))

(defun unpop-global-mark-command ()
  "Unpop off mark ring.  Does nothing if mark ring is empty."
  (interactive)
  (when global-mark-ring
	(let ((marker (car (last global-mark-ring))))
	  (setq global-mark-ring (cons marker (nbutlast global-mark-ring)))
	  (goto-marker marker))))

;; this is global mode only package ..... and that sucks.
(use-package beacon :ensure t
  :defer t
  :config
  (setq beacon-blink-when-point-move-vertically nil
		beacon-blink-when-focused nil)
  (add-to-list 'beacon-dont-blink-major-modes 'eshell-mode)
  (add-hook 'beacon-dont-blink-predicates
            (defun was-wheel-scroll-p ()
			  (memq (event-basic-type last-command-event)
					'(wheel-up wheel-down wheel-right wheel-left))))
  ;; :init
  ;; (remove-hook 'prog-mode-hook 'beacon-mode)
  )

(use-package elfeed :ensure t
  :defer t
  :config
  (with-eval-after-load 'midnight
	(add-hook 'midnight-hook 'elfeed-db-save-safe)
	(add-hook 'midnight-hook 'elfeed-db-gc-safe))
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
		elfeed-show-entry-switch 'display-buffer)
  (setq elfeed-feeds
		'(;; programming
		  ;; ("https://news.ycombinator.com/rss" hacker news)
		  ;; ("https://buttondown.email/hillelwayne/rss" programming)

		  ;; reddit
		  ("https://www.reddit.com/r/programming.rss" programming news)
		  ;; ("https://www.reddit.com/r/ReverseEngineering.rss" hacker news)
		  ;; ("https://www.reddit.com/r/philosophy/top.rss?t=week" philosophy news)
		  ("https://www.reddit.com/r/emacs/top.rss?t=week" emacs news)
		  ("https://www.reddit.com/r/lisp/top.rss?t=week" lisp news)

		  ("https://dustycloud.org/blog/" lisp blog)

		  ;; ("https://www.hltv.org/rss/news" csgo news)
		  ;; how to get youtube channel rss
		  ;; window["ytInitialData"].metadata.channelMetadataRenderer.rssUrl
		  ;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCTkXRDQl0luXxVQrRQvWS6w" dream youtube)
		  ;; blog
		  ;; ("https://mstmetent.blogspot.com/feeds/posts/default?alt=rss" sbcl)
		  ;; ("https://asahilinux.org/blog/index.xml" asahilinux)
		  ))

  ;; Entries older than 2 weeks are marked as read
  ;; (add-hook 'elfeed-new-entry-hook
  ;; 			(elfeed-make-tagger :before "2 weeks ago"
  ;; 								:remove 'unread))

  ;; https://emacstil.com/til/2021/10/31/bookmark-star-feeds-in-elfeed/
  ;; face for starred articles
  (defface elfeed-search-star-title-face
	'((t :foreground "#f77"))
	"Marks a starred Elfeed entry.")

  (defalias 'elfeed-toggle-star
	(elfeed-expose #'elfeed-search-toggle-all 'star))

  (setq elfeed-show-mode-hook
		(list (lambda ()
				(setq-local
				 fill-column 90
				 line-spacing 4
				 shr-max-width (- fill-column 5)
				 visual-fill-column-extra-text-width '(-3 . 0))
				(visual-fill-column-mode 1)))
		elfeed-search-remain-on-entry t
		;; This makes elfeed-show buffer stay in same window!
		elfeed-show-entry-delete 'quit-window)
  (add-to-list 'elfeed-search-face-alist
			   '(star elfeed-search-star-title-face))

  (util:define-keys elfeed-show-mode-map
	"h" 'backward-char
	"j" 'next-line
	"k" 'previous-line
	"l" 'forward-char
	"q" 'quit-window)

  (util:define-keys elfeed-search-mode-map
	"S" 'elfeed-search-set-filter
	"s" (lambda () (interactive)
		  (let ((p (point)))
			(unwind-protect
				(call-interactively 'elfeed-search-live-filter)
			  (goto-char p))))
	"f" (elfeed-expose #'elfeed-search-toggle-all 'fav)
	"m" 'elfeed-toggle-star				; was not bind
	"u" (elfeed-expose #'elfeed-search-toggle-all 'unread)
	"U" 'elfeed-update-feed)

  (add-hook 'elfeed-search-mode-hook
			(defun elfeed-search-mode-setup ()
			  ;; better readability
			  (setq-local line-spacing 5)))
  :init (global-set-key [?\C-x ?w] 'elfeed))

(use-package guitar-tab
  :disabled
  :init
  (defun guitar-tab-auto-fill ()
	;; (org-table-get-field)
	(when (save-excursion
			(skip-chars-backward "^|")
			(looking-at-p "[ ]+|"))
	  (org-table-get-field nil " - ")))

  (defun guitar-tab-next-entry ()
	"Go below first (6 times), then right across."
	(interactive)
	(org-table-maybe-recalculate-line)
	(if (and org-table-automatic-realign
			 org-table-may-need-update)
		(org-table-align))
	;; (guitar-tab-auto-fill)
	(if (looking-at-p "[ ]*\n")
		;; goto next table if exceeding fill-column
		;; (if (>= (current-column) fill-column)
		;; 	(guitar-tab-next-table))
	    (org-table-insert-column)
	  (let ((col (org-table-current-column)))
		(beginning-of-line 2)
		(unless (bolp) (insert "\n"))	;missing newline at eob
		(when (or (not (org-at-table-p))
				  (org-at-table-hline-p))
		  (beginning-of-line 0)
		  ;; (org-table-insert-row 'below)
		  (org-table-goto-line 0)
		  (cl-incf col 1))
		(org-table-goto-column col)
		(skip-chars-backward "^|\n\r")
		(when (looking-at " ")
		  (forward-char)))))

  (defun guitar-tab-prev-entry ()
	"Go above first (6 times), then right across."
	(interactive)
	(org-table-maybe-recalculate-line)
	(if (and org-table-automatic-realign
			 org-table-may-need-update)
		(org-table-align))
	(let ((col (org-table-current-column)))
	  (beginning-of-line 0)
	  ;; (unless (bolp) (insert "\n"))	;missing newline at eob
	  (when (or (not (org-at-table-p))
				(org-at-table-hline-p))
		(beginning-of-line 2)
		;; goto end of table
		(while (or (org-at-table-p)
				   (org-at-table-hline-p))
		  (beginning-of-line 2))
		(beginning-of-line 0)
		(cl-decf col 1))
	  (org-table-goto-column col)
	  ;; (skip-chars-forward "^|\n\r")
	  ;; (when (looking-at " ")
	  ;; 	(backward-char))
	  ))

  (defun guitar-tab-prev-table ()
	(interactive)
	(while (not (or (org-at-table-p)
					(org-at-table-hline-p)))
	  (forward-line -1))
	(while (or (org-at-table-p)
			   (org-at-table-hline-p))
	  (beginning-of-line -1))
	(beginning-of-line 2)				; forward ~2 lines
	(org-table-next-field))

  (defun guitar-tab-next-table ()
	"Go to the start of next table"
	(interactive)
	(while (or (org-at-table-p)
			   (org-at-table-hline-p))
	  (forward-line 1))
	(forward-line 1)

	(if (or (org-at-table-p)
			(org-at-table-hline-p))
		(progn (beginning-of-line)
			   (org-table-next-field))
	  (when (eobp) (insert "\n"))	;missing newline at eob
	  (condition-case err
		  (org-next-block 1 nil "^[ \t]*|")
		(user-error
		 ;; insert new table her
		 (insert (make-string 6 ?|))
		 (cl-loop repeat (- 6 1)
				  do (insert ?\n ?|))
		 (org-table-align)
		 ))))

  (defun guitar-tab-insert-column (char)
	(interactive (list (event-basic-type last-command-event)))
	(pcase-let ((`(,i . ,j)
				 (guitar-tab-cusor-pos-to-ij)))
	  ;; (guitar-tab-align)
	  (save-excursion
		(dotimes (_ 5)
		  (insert char)
		  ;; (forward-line 1)
          (org-table-next-row))
		(insert char))))

  (defun guitar-tab-in-listp ()
	(save-excursion
	  (beginning-of-line)
	  (looking-at-p "^[ \t](")))

  (defun guitar-tab--list-n? (fn)
	"Run FN n? times until scan-error.
FN is a list traversal operation."
	(and (guitar-tab-in-listp)
		 (cl-loop for j from 0
				  while (condition-case err (progn (funcall fn 1) t)
						  (scan-error nil))
				  finally return j)))

  (defun guitar-tab-cusor-pos-to-ij ()
	;; escape to top level
	(save-excursion
      (and (guitar-tab-in-listp)
		   (let* ((j (guitar-tab--list-n? 'backward-sexp))
				  (i (progn (backward-char)
						    (guitar-tab--list-n? 'backward-sexp))))
		     (cons i j)))))

  (defvar guitar-tab-mode-map
	(let ((map (make-sparse-keymap)))
	  (define-key map [?f] 'guitar-tab-next-entry)
	  (define-key map [?b] 'guitar-tab-prev-entry)
	  (define-key map [?n] 'guitar-tab-next-table)
	  (define-key map [?p] 'guitar-tab-prev-table)
	  (define-key map [?/] 'guitar-tab-insert-column)
	  map))

  (define-minor-mode guitar-tab-mode
	""
	:global nil
	:init-value nil
	;; :keymap guitar-tab-mode-map
    ;; (if guitar-tab-mode
    ;;     (let ((org-modern-tag nil)
    ;;           (org-modern-block nil)
    ;;           (org-modern-checkbox nil)
    ;;           (org-modern-todo nil)
    ;;           (org-modern-priority nil)
    ;;           (org-modern-todo nil)
    ;;           (org-modern-table t)
    ;;           (org-modern-table-vertical 1))
    ;;       (org-modern-mode 1))
    ;;   (org-modern-mode -1))
    )
  ;; (define-derived-mode org-mode )

  (provide 'guitar-tab))

(use-package skeletor :ensure t
  :defer t
  :config
  (setq skeletor-completing-read-function 'completing-read)
  (skeletor-define-template "verilog"
	:title "Verilog"
	:no-license? t
	:no-git? t)
  (skeletor-define-template "vhdl"
	:title "VHDL"
	:no-license? t
	:no-git? t)
  (defun skeletor-underscore-proj-name ()
	(replace-regexp-in-string "-" "_" skeletor-project-name)))

(use-package holymotion
  ;; ERROR of holymotion-make-motion undefined function??
  :disabled 
  :straight (holymotion :type git
                        :host github
                        :repo "Overdr0ne/holymotion"
                        :branch "main")
  :config
  ;; (holymotion-make-motion holymotion-forward-sexp #'puni-forward-sexp)
  ;; (holymotion-make-motion holymotion-backward-sexp #'puni-backward-sexp)
  nil
  )

(provide 'init-misc)
;;; init-misc.el ends here
