;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Contains essential configuration, should be load before anything
;; else.

;;; Code:

;;; Set up $PATH and needs to be run first
(use-package exec-path-from-shell :ensure t
  :init
  (when (memq window-system '(mac ns x))
	(setq exec-path-from-shell-variables
		  '("PATH" "MANPATH"
			"SSH_AUTH_SOCK" "SSH_AGENT_PID"
			"GUILE_LOAD_COMPILED_PATH"
			"GUILE_LOAD_PATH"
			;; "GPG_AGENT_INFO"
			"LIBRARY_PATH"))
	(exec-path-from-shell-initialize)))

;; reply y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

(setq disabled-command-function
	  (lambda (&optional cmd keys)
		(unless cmd (setq cmd this-command))
		(unless keys (setq keys (this-command-keys)))
		(message
		 (substitute-command-keys
		  (format "`%s' \\[%s] is disabled"
				  cmd cmd)))))

;; {{ code is copied from https://liu233w.github.io/2016/09/29/org-python-windows.org/
;; Set up LANGUAGE-NAME and CODING-SYSTEM at Windows.
;; For example
;; - "English" and 'utf-16-le
;; - "Chinese-GBK" and 'gbk
(set-language-environment "English")
(prefer-coding-system 'utf-8)
;; }}

;; {{ narrow region
;; @see https://gist.github.com/mwfogleman/95cc60c87a9323876c6c
;; fixed to behave correctly in org-src buffers; taken from:
;; https://lists.gnu.org/archive/html/emacs-orgmode/2019-09/msg00094.html
(defun narrow-or-widen-dim (&optional use-indirect-buffer)
  "If the buffer is narrowed, it widens.
Otherwise, it narrows to region, or Org subtree.  If
USE-INDIRECT-BUFFER is not nil, use `indirect-buffer' to hold the
widen content."
  (interactive "P")
  (cond
   ((and (not use-indirect-buffer) (buffer-narrowed-p))
    (widen))

   ;; narrow to region
   ((region-active-p)
    (narrow-to-region (region-beginning) (region-end)))

   ;; narrow to specific org element
   ((derived-mode-p 'org-mode)
    (cond
     ((ignore-errors (org-edit-src-code)) t)
     ((ignore-errors (org-narrow-to-block) t))
     ((ignore-errors (org-narrow-to-element) t))
     (t (org-narrow-to-subtree))))

   ((derived-mode-p 'diff-mode)
    (save-excursion
      ;; If the (point) is already beginning or end of file diff,
      ;; the `diff-beginning-of-file' and `diff-end-of-file' return nil
      (when-let* ((b (progn (diff-beginning-of-file) (point)))
		  (e (progn (diff-end-of-file) (point))))
        (when (< b e)
          (narrow-to-region-indirect-buffer-maybe
	   b e use-indirect-buffer)))))

   ((derived-mode-p 'prog-mode) (narrow-to-defun))
   (t (error "Please select a region to narrow to"))))
;; }}

(cl-case system-type
  (gnu
   ;;; credit: yorickvP on Github
   (defvar wl-copy-process nil)
   (defun wl-copy (text)
	 "Paste TEXT to clipboard."
	 (let ((string (buffer-substring-no-properties beg end)))
       ;; added redirection to /dev/null for immediate return
       ;; @see https://emacs.stackexchange.com/questions/39019/xclip-hangs-shell-command
       ;; (shell-command (format "wl-copy %S &> /dev/null" string) nil)
       (setq wl-copy-process
			 (make-process :name "wl-copy"
						   :buffer nil
						   :command '("wl-copy" "-f" "-n")
						   :connection-type 'pipe))
       (process-send-string wl-copy-process text)
       (process-send-eof wl-copy-process)))
   (setq interprogram-cut-function 'wl-copy)
   (setq interprogram-paste-function
		 (lambda ()
		   (if (and wl-copy-process (process-live-p wl-copy-process))
			   ;; should return nil if we're the current paste owner
			   nil
			 (shell-command-to-string "wl-paste -n | tr -d \r")))))
  (darwin
   (setq mac-option-modifier 'meta
		 mac-command-modifier 'super
		 mac-right-option-modifier 'none)
   ;; (lookup-key global-map [magnify-up])
   ;; mac-magnify-text-scale-or-overview-tab-group
   (util:define-keys global-map
	 ;; disable turn on and off fullscreen
	 [S-magnify-up] 'ignore
	 [S-magnify-down] 'ignore
     [?\s-v] 'yank
     [?\s-s] 'save-buffer
     [?\C-\s-f] 'toggle-frame-fullscreen)

   (with-eval-after-load 'select
     ;; prevent mark (selection) save to kill-ring
     (setq select-enable-primary nil))))

;; Key fixes
;; @see https://emacs.stackexchange.com/questions/20240/how-to-distinguish-c-m-from-return
(when (display-graphic-p)
  (defun util:swap-chars (c1 c2 &optional unbind)
	(define-key input-decode-map c1 (and (not unbind) c2))
	(define-key input-decode-map c2 (and (not unbind) c1)))
  ;; (util:swap-chars "9" "(" t)
  ;; (util:swap-chars "8" "*" t)
  (util:define-keys input-decode-map
	;; [?\C-i] [C-i]           ; let C-i be C-i instead of TAB
	;; [?\C-m] nil						  ; let ?\C-m is RET
	;; kbd C-[ is ECS, map it to C-g
	[?\C-\[] [C-\[]))

(with-eval-after-load 'quail
  ;; (pop quail-keyboard-layout-alist)
  (add-to-list 'quail-keyboard-layout-alist
			   (cons "colemak"
					 (concat "                              "
							 "§±1!2@3£4$5%6^7&8*9(0)-_=+    "
							 "  qQwWfFpPgGjJlLuUyY;:[{]}    "
							 "  aArRsStTdDhHnNeEiIoO'\"\\|    "
							 "`~zZxXcCvVbBkKmM,<.>/?        "
							 "                              ")))

  ;; Mac keyboard
  ;; §±  1!  2@  3£  4$  5%  6^  7&  8*  9( 0)  -_  =+
  ;;  qQ  wW  fF  pP  gG  jJ  lL  uU  yY  ;:  [{  ]}
  ;;   aA  rR  sS  tT  dD  hH  nN  eE  iI  oO  '"  \|
  ;; `~ zZ  xX  cC  vV  bB  kK  mM  ,<  .> /?

  (quail-define-package
   "Colemak" "English" "Colemak" t
   "English ASCII Input simulating Colemak")

  (quail-define-rules
   ;; First Row, numeric, same as qwerty.
   ;; ("1" ?!) ("2" ?[) ("3" ?{) ("4" ?}) ("5" ?\() ("6" ?=) ("7" ?*)
   ;; ("8" ?\)) ("9" ?+) ("0" ?\]) ("-" ?!) ("=" ?#) ("`" ?$)

   ;; ("!" ?%) ("@" ?7) ("#" ?5) ("$" ?3) ("%" ?1) ("^" ?9) ("&" ?0)
   ;; ("*" ?2) ("(" ?4) (")" ?6) ("_" ?8) ("+" ?`) ("~" ?~)

   ;; Second Row
   ;; ("q" ?q)
   ;; ("w" ?w)
   ("e" ?f)
   ("r" ?p)
   ("t" ?g)
   ("y" ?j)
   ("u" ?l)
   ("i" ?u)
   ("o" ?y)
   ("p" ?\;)
   ;; ("[" ?\[)
   ;; ("]" ?\])

   ;; ("Q" ?Q)
   ;; ("W" ?W)
   ("E" ?F)
   ("R" ?P)
   ("T" ?G)
   ("Y" ?J)
   ("U" ?L)
   ("I" ?U)
   ("O" ?Y)
   ("P" ?:)
   ;; ("{" ?\{)
   ;; ("}" ?\})

   ;; Home Row
   ;; ("a" ?a)
   ("s" ?r)
   ("d" ?s)
   ("f" ?t)
   ("g" ?d)
   ("h" ?h)
   ("j" ?n)
   ("k" ?e)
   ("l" ?i)
   (";" ?o)
   ;; ("'" ?\')
   ;; ("\\" ?\\)

   ;; ("A" ?A)
   ("S" ?R)
   ("D" ?S)
   ("F" ?T)
   ("G" ?D)
   ("H" ?H)
   ("J" ?N)
   ("K" ?E)
   ("L" ?I)
   (":" ?O)
   ;; ("\"" ?\")
   ;; ("|" ?|)

   ;; Bottom Row
   ;; ("z" ?z)
   ;; ("x" ?x)
   ;; ("c" ?c)
   ;; ("v" ?v)
   ;; ("b" ?b)
   ("n" ?k)
   ;; ("m" ?m)
   ;; ("," ?,)
   ;; ("." ?.)
   ;; ("/" ?/)

   ;; ("Z" ?Z)
   ;; ("X" ?X)
   ;; ("C" ?C)
   ;; ("V" ?V)
   ;; ("B" ?B)
   ("N" ?K)
   ;; ("M" ?M)
   ;; ("<" ?<)
   ;; (">" ?>)
   ;; ("?" ?\?)
   ))

(define-key global-map [C-\[] [?\C-g])

;; {{ Write backup files to its own directory
;; @see https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-and-Backup.html
(defvar my/binary-file-name-regexp
  "\\.\\(avi\\|wav\|pdf\\|mp[34g]\\|mkv\\|exe\\|3gp\\|rmvb\\|rm\\)$"
  "Is binary file name?")

(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not (string-match-p my/binary-file-name-regexp name)))))

;; {{ cache files
(use-package no-littering :ensure t
  :defer t
  :config
  (with-eval-after-load 'recentf
	(add-to-list 'recentf-exclude no-littering-var-directory)
	(add-to-list 'recentf-exclude no-littering-etc-directory))
  (setq auto-save-file-name-transforms
		`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))
;; }}

(setq backup-directory-alist
	  `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

(setq backup-by-copying t		; don't clobber symlinks
      delete-old-versions t
      version-control t			; use versioned backups
      kept-new-versions 6
      kept-old-versions 2)

;; Don't make backups of files, not safe
;; @see https://github.com/joedicastro/dotfiles/tree/master/emacs
(setq vc-make-backup-files nil)
;; }}

;; (mouse-avoidance-mode 'none)		   ; no need mac osx hide mouse on timeout

;;; Use the system clipboard
;; @see https://www.emacswiki.org/emacs/CopyAndPaste
;; So `C-y' could paste from clipbord if you are NOT using emacs-nox
(setq select-enable-clipboard t  ;; if t might cause sway to crash?
      select-enable-primary t)

(defun buffer-too-big-p ()
  "If buffer is more than 3000 lines, each line >= 60 bytes."
  (cl-destructuring-bind (line longest-line-len mean-line-len)
	  (buffer-line-statistics)
	(if (and (boundp 'so-long-target-modes)
			 (apply 'derived-mode-p so-long-target-modes))
		(> longest-line-len so-long-threshold)
	  (if (derived-mode-p 'text-mode)
		  (and (> line 3000)
			   (> mean-line-len 60))
		(error "Buffer mode (%s) is not supported" major-mode)))))

;; files, very very long line counter-measurement
(use-package so-long
  :defer t
  :config
  (setq so-long-predicate 'buffer-too-big-p) ; was so-long-statistics-excessive-p
  :init (add-hook 'emacs-startup-hook #'global-so-long-mode))

(use-package tramp
  :defer t
  :config
  ;; to ignore 'Remote file error: Forbidden reentrant call of Tramp'
  ;; (setq debug-ignored-errors
  ;;       (cons 'remote-file-error debug-ignored-errors))

  ;; with-eval-after-load 'tramp
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))
  ;; @see https://github.com/sly20bnr/spacemacs/issues/1921
  ;; If you tramp is hanging, you can uncomment below line.
  ;; (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  (setq tramp-verbose 3)
  (setq tramp-debug-buffer t)
  (setq tramp-default-method "ssh")
  (setq tramp-chunksize 8192))

(defun reopen-with-sudo (file)
  "Reopen a FILE with sudo."
  (interactive (list (buffer-file-name (current-buffer))))
  (find-file (concat "/sudo::" file)))

;; Undo
;; Store more undo history to prevent loss of data
(setq undo-limit 800000
      undo-no-redo t
      undo-strong-limit 800000
      undo-outer-limit 800000)

(setq kill-ring-max 100)				; default is 60

(use-package undohist :ensure t
  :defer t
  :commands (undohist-save-safe undohist-recover-safe)
  :config
  (setq undohist-ignored-files '(".epub$"))
  ;; (undohist-initialize) does the following:
  (when (not (file-directory-p undohist-directory))
    (make-directory undohist-directory t))
  :init
  (add-hook 'before-save-hook #'undohist-save-safe)
  (add-hook 'find-file-hook #'undohist-recover-safe))

;; Visual undo
(use-package vundo :ensure t
  :defer t
  :commands (vundo)
  :config
  (setq vundo-compact-display nil	; Take less on-screen space.
		;; vundo-unicode-symbols
        vundo-glyph-alist vundo-ascii-symbols)

  ;; Use `HJKL` VIM-like motion, also C-a/C-e to jump around.
  (util:define-keys vundo-mode-map
    "l" #'vundo-forward
    "h" #'vundo-backward
    "j" #'vundo-next
    "k" #'vundo-previous
    [?\C-a] #'vundo-stem-root
    [?\C-e] #'vundo-stem-end
    [return] #'vundo-confirm))

;; winner undo/redo
(add-hook 'emacs-startup-hook #'winner-mode)
(setq winner-dont-bind-my-keys t)
(with-eval-after-load 'winner
  ;; I don't need that many records
  (setq winner-ring-size 64))

;; uniquify
;; Nicer naming of buffers for files with identical names
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; hippie-expand Since we got company-ispell and `M-x
;; toggle-company-ispell' Done, now we just use it as a clause in our
;; make-hippie-expand-function (as above)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; Uncomment these binds, as they are unused or should be redundant
;; (global-set-key (kbd "M-/") 'hippie-expand)
;; (define-key minibuffer-local-map [escape] 'keyboard-escape-quit)

(provide 'init-essential)
;;; init-essential.el ends here
