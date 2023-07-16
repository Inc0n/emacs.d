;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

;; {{ @see https://coredumped.dev/2020/01/04/native-shell-completion-in-emacs/
(with-eval-after-load 'shell
  ;; Enable auto-completion in `shell'.
  (native-complete-setup-bash)
  (define-key shell-mode-map [?\C-w] 'backward-kill-word)

  (add-hook 'shell-mode-hook 'shell-mode-hook-setup)
  (defun shell-mode-hook-setup ()
	"Set up `shell-mode'."
	(setq-local comint-process-echoes t)
	;; hook `completion-at-point', optional
	(add-hook 'completion-at-point-functions
			  #'native-complete-at-point nil t)))
;; }}

(setq eshell-scroll-to-bottom-on-input 'all
	  ;; this can be annoying, when a process is running in eshell
	  ;; all key presses are sent to that, instead of being recognized
	  eshell-send-direct-to-subprocesses nil)

(with-eval-after-load 'eshell
  (autoload 'time-stamp-string "time-stamp.el")
  ;; When was prompt prompted? Inspired from:
  ;; https://redandblack.io/blog/2020/bash-prompt-with-updating-time/
  (setq eshell-prompt-regexp
        (mapconcat
         (lambda (str) (concat "\\(" str "\\)"))
         '("^[^#$\n]* [#$] "			; default
           ;; "^\\(mysql\\|[ ]\\{4\\}[-\"'`]\\)> "
           "^>>> "						; python
           ;; "^ftp> "
           )
         "\\|"))
  (setq eshell-prompt-function
		(lambda ()
		  (concat
		   ;; "\n"							; to increase readability
		   "┌─"
		   (propertize (time-stamp-string "%H:%M ") 'face 'bold)
		   (abbreviate-file-name (eshell/pwd))
		   "\n"
		   (if (= (user-uid) 0)
			   " # "
			 (format "└─%s $ "
					 (if (= eshell-last-command-status 0)
						 (propertize "√" 'face 'success)
					   (propertize (format "?%d" eshell-last-command-status)
								   'face 'error)))))))
  (setq eshell-list-files-after-cd t	; ls after cd
		eshell-prefer-lisp-functions nil)
  ;; aliases should be defined in:
  ;; `eshell-aliases-file'
  ;; "/Users/xception/.emacs.d/eshell/alias"
  (with-eval-after-load 'esh-mode
	(util:define-keys eshell-mode-map
	  [?\M-q] 'quit-window
	  [?\C-l] 'eshell/clear
	  [?\C-c ?l] (lambda () (interactive)
				   ;; TODO, if buffer to big, ask and delete without
				   ;; kill
				   (let ((inhibit-read-only t))
					 (erase-buffer))
				   (eshell-send-input))
	  [?\C-w] 'backward-delete-word))

  ;; eshell/tomp3 complains:
  ;; no catch for 'eshell-replace-command
  (defun tomp3 (&rest args)
	(let* ((args (eshell-stringify-list
				  (flatten-tree args)))
		   (audio-file-in (car args))
		   (audio-file-out
			(or (cadr args)
				(concat (file-name-base audio-file-in)
						".mp3"))))
	  (unless (file-exists-p audio-file-in)
		(user-error "tomp3: \"%s\" not found"
					audio-file-in))
	  (when (file-exists-p audio-file-out)
		(user-error "tomp3: audio file out, (below) already exist\n%s"
					audio-file-out))
	  (throw 'eshell-replace-command
			 (eshell-parse-command
			  "*ffmpeg"
			  (list
			   "-i" audio-file-in
			   "-vn" "-ar" "48000" "-ac" "2" "-b:a" "256"
			   audio-file-out)))))

  ;; inspired by
  ;; https://www.reddit.com/r/emacs/comments/x4duoi/comics_in_emacs_eshell/
  (defun eshell/image (&rest args)
	"Display image in eshell with ARGS.
Note: ensure comic images live in `wizardzines-comics-path', named with
command name and no extension."
	(require 'iimage)
	(eshell-eval-using-options
	 "image" args
	 '((?h "help" nil nil "show this usage screen")
	   ;; :external "ecomic"
	   :show-usage
	   :usage "COMMAND

Show COMMAND images within eshell buffer")
	 (let* ((command (nth 0 (eshell-stringify-list (eshell-flatten-list args))))
			(image-fpath (concat default-directory command)))
	   (unless (file-exists-p image-fpath)
		 (user-error "image: \"%s\" not found :-(" command))
	   ;; (eshell-buffered-print "\n")
	   (add-text-properties 0 (length image-fpath)
							`(display
							  ,(create-image
								image-fpath nil nil
								:max-width (* 7 (window-width)))
							  modification-hooks
							  (iimage-modification-hook))
							image-fpath)
	   (eshell-buffered-print image-fpath)
	   (eshell-flush))))

  (setq eshell-history-size 256)

  (defalias 'eshell/v 'eshell-exec-visual))

(use-package eat :ensure t
  :after eshell
  :config (add-hook 'emacs-load-hook 'eat-eshell-visual-command-mode))

(use-package eshell-vterm :ensure t
  :disabled
  :after eshell
  :config (eshell-vterm-mode))

(with-eval-after-load 'term
  ;; utf8
  (defun my/term-use-utf8 ()
    ;; "Ensure the ansi-term has the utf8 encoding."
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (add-hook 'term-exec-hook #'my/term-use-utf8))

(with-eval-after-load 'comint
  ;; Don't echo passwords when communicating with interactive programs:
  ;; Github prompt is like "Password for 'https://user@github.com/':"
  ;; (setq comint-password-prompt-regexp
  ;; 		(format "%s\\|^ *Password for .*: *$" comint-password-prompt-regexp))
  (setq comint-prompt-read-only t
		comint-buffer-maximum-size 2048)
  (add-to-list 'comint-output-filter-functions 'comint-truncate-buffer)
  (define-key comint-mode-map
    ;; Don't show trailing whitespace in REPL.
    (kbd "M-;") #'comment-dwim))

(with-eval-after-load 'gdb-mi
  (define-key gdb-inferior-io-mode-map "q" 'quit-window)
  (add-hook 'comint-output-filter-functions #'ansi-color-process-output)
  (add-hook 'gud-mode-hook 'ansi-color-for-comint-mode-on))

(use-package vterm :ensure t
  :commands (vterm)
  ;; :load-path"~/.emacs.d/site-lisp/emacs-libvterm/"
  :config
  ;; (add-to-list 'vterm-eval-cmds '("quick-view" posframe-quick-view-file))
  (setq vterm-environment
		(list (format "EMACS_PATH=%s" (file-truename user-emacs-directory))))

  ;; setting these keys to nil to allow them to function normally
  (define-key vterm-copy-mode-map
	[?\M-w] #'vterm-copy-mode-done)

  (util:define-keys vterm-mode-map
	[?\C-\M-k] [?\M-d]
    [?\C-\M-b] 'vterm--self-insert
    [?\C-\M-f] 'vterm--self-insert
	;; [?\C-x ?\C-f] nil
	;; (lambda () (interactive)
	;;   (vterm-send "C-a")
	;;   (vterm-send "C-k")
	;;   (vterm-send-string "cd ")
	;;   (vterm-send-string
	;;    (file-name-directory (read-file-name "File: "))))
	;; free the following key
    ;; [?\M->] nil
    [?\M-w] nil
    [?\C-s] nil)

  (defun vterm-cd (directory)
    (interactive
     (list (let ((arg current-prefix-arg))
			 (cond ((null arg) default-directory)
				   ((listp arg) (read-directory-name ""))
				   (:else (user-error "Unexpcted prefix arg %s" arg))))))
    (let* ((vterm-buffers
			(--filter
			 (let ((name (buffer-name it)))
			   (and (string-match-p "vterm" name)
					(not (string-match-p "org" name))
					(with-current-buffer it
					  (eq major-mode 'vterm-mode))))
			 (buffer-list)))
		   (vterm-buffer
			(cond ((null vterm-buffers)
				   (user-error "No vterm buffer alive"))
				  ((null (cdr vterm-buffers)) (car vterm-buffers))
				  (:else
				   (completing-read "Vterm: "
									(mapcar 'buffer-name vterm-buffers))))))
      (pop-to-buffer vterm-buffer)
      (vterm-send-string "cd ")
      (vterm-send-string directory)
      (vterm-send-return)))

  (add-hook 'vterm-mode-hook
			(defun vterm-mode-setup ()
			  (vterm-send-string
			   "source $EMACS_PATH/etc/emacs-vterm-zsh.sh\n"))))

(defun open-shell ()
  (interactive)
  (if (null prefix-arg)
	  (vterm)
	(let* ((win (selected-window))
		   (dir (with-current-buffer (window-buffer win)
				  default-directory)))
	  (vterm-cd dir)
	  (setq prefix-arg nil))))

(defun open-shell ()
  "Interface for skhd integration."
  (interactive)
  ;; Not using as this forces pop-to-buffer-same-window
  ;; (eshell)
  (require 'eshell)						; load this first
  (let* ((win (selected-window))
		 (dir (with-current-buffer (window-buffer win)
				default-directory))
		 (buf (get-buffer-create eshell-buffer-name)))
	(switch-to-buffer buf)
	(unless (derived-mode-p 'eshell-mode)
      (eshell-mode))
	(when prefix-arg
	  ;; ls after cd complains since this is not interactive
	  (ignore-error 'error (eshell/cd dir))
	  ;; refresh prompt
	  (eshell-send-input)
	  (setq prefix-arg nil))))

(defun my/shell-cd (directory)
  (interactive
   (list (let ((arg current-prefix-arg))
		   (cond ((null arg) default-directory)
				 ((listp arg) (read-directory-name ""))
				 (:else (user-error "Unexpcted prefix arg %s" arg))))))
  ;; eshell-mode
  (with-current-buffer (open-shell)
	(insert "cd " directory)
	(pcase major-mode
	  ('eshell-mode (eshell-send-input))
	  ('shell-mode (comint-send-input)))))

(provide 'init-shell)
;;; init-shell.el ends here
