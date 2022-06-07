;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Like "init-misc.el", the difference is this file is always loaded

;;; Code:

;;; Set up $PATH and needs to be run first
(use-package exec-path-from-shell :ensure t)
(when (memq window-system '(mac ns x))
  (setq exec-path-from-shell-variables
	'("PATH" "MANPATH"
	  "SSH_AUTH_SOCK" "SSH_AGENT_PID"
	  ;; "GPG_AGENT_INFO"
	  "LIBRARY_PATH"))
  (exec-path-from-shell-initialize))

;; reply y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; {{ code is copied from https://liu233w.github.io/2016/09/29/org-python-windows.org/
;; Set up LANGUAGE-NAME and CODING-SYSTEM at Windows.
;; For example
;; - "English" and 'utf-16-le
;; - "Chinese-GBK" and 'gbk
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
;; }}

;; {{ narrow region

;; @see https://gist.github.com/mwfogleman/95cc60c87a9323876c6c
;; fixed to behave correctly in org-src buffers; taken from:
;; https://lists.gnu.org/archive/html/emacs-orgmode/2019-09/msg00094.html
(defun narrow-or-widen-dim (&optional use-indirect-buffer)
  "If the buffer is narrowed, it widens.
Otherwise, it narrows to region, or Org subtree.
If USE-INDIRECT-BUFFER is not nil, use `indirect-buffer' to hold the widen content."
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
   (setq interprogram-cut-function 'wl-copy)
   (setq interprogram-paste-function 'wl-paste))
  (darwin
   (setq mac-option-modifier 'meta
	 mac-command-modifier 'super
	 mac-right-option-modifier 'none)
   (define-keys global-map
     [?\s-v] 'yank
     [?\s-s] 'save-buffer
     [?\C-\s-f] 'toggle-frame-fullscreen)
   (define-key input-decode-map (kbd "M-3") "#")

   (with-eval-after-load 'select
     ;; prevent mark (selection) save to kill-ring
     (setq select-enable-primary nil))))

;; Key fixes
;; @see https://emacs.stackexchange.com/questions/20240/how-to-distinguish-c-m-from-return
(define-keys input-decode-map
  ;; [?\C-i] [C-i]           ; let C-i be C-i instead of TAB
  ;; actually let's use this one in place of evil-escape
  ;; [?\C-\[] nil                      ; let C-[ be C-[ instead of ESC
  [?\C-m] [C-m]				; let C-m be C-m instead of RET
  [?\C-\[] [C-\[])

(define-key global-map [C-\[]
  [?\C-g])

;; to disable C-[
;; (define-keys global-map
;;   [C-\[] nil
;;   [?\C-\[] esc-map)
;; (define-keys input-decode-map [?\C-\[] nil)

;; {{ Write backup files to its own directory
;; @see https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-and-Backup.html
(defvar my/binary-file-name-regexp
  "\\.\\(avi\\|wav\|pdf\\|mp[34g]\\|mkv\\|exe\\|3gp\\|rmvb\\|rm\\)$"
  "Is binary file name?")

(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not (string-match-p my/binary-file-name-regexp name)))))

;; no littering is good enough
;; (let ((backup-dir (expand-file-name "~/.backups")))
;;   (unless (file-exists-p backup-dir)
;; 	(make-directory backup-dir))
;;   (setq backup-directory-alist (list (cons "." backup-dir))))

(setq backup-by-copying t		; don't clobber symlinks
      delete-old-versions t
      version-control t			; use versioned backups
      kept-new-versions 6
      kept-old-versions 2)

;; Don't make backups of files, not safe
;; @see https://github.com/joedicastro/dotfiles/tree/master/emacs
(setq vc-make-backup-files nil)
;; }}

;;; Use the system clipboard
;; @see https://www.emacswiki.org/emacs/CopyAndPaste
;; So `C-y' could paste from clipbord if you are NOT using emacs-nox
(setq select-enable-clipboard t  ;; if t might cause sway to crash?
      select-enable-primary t)

;;; credit: yorickvP on Github
(defvar wl-copy-process nil)
(defun wl-copy (text)
  "Paste te-xt to clipboard."
  (let ((string (buffer-substring-no-properties beg end)))
    ;; added redirection to /dev/null for immediate return
    ;; @see https://emacs.stackexchange.com/questions/39019/xclip-hangs-shell-command
    ;; (shell-command (format "wl-copy %S &> /dev/null" string) nil)
    (setq wl-copy-process (make-process :name "wl-copy"
					:buffer nil
					:command '("wl-copy" "-f" "-n")
					:connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process)))

(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil		    ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))

(use-package tramp :config
  ;; with-eval-after-load 'tramp
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))
  ;; @see https://github.com/syl20bnr/spacemacs/issues/1921
  ;; If you tramp is hanging, you can uncomment below line.
  ;; (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  (setq tramp-verbose 3)
  (setq tramp-debug-buffer t)
  (setq tramp-default-method "ssh")
  (setq tramp-chunksize 8192))

;; Undo
;; Store more undo history to prevent loss of data
(setq undo-limit 8000000
      undo-no-redo t
      undo-strong-limit 8000000
      undo-outer-limit 8000000)

(use-package undohist :ensure t
  :defer t
  :init
  (autoload 'undohist-initialize "undohist")
  (undohist-initialize))

;; {{ Visual undo
(use-package vundo
  ;; :ensure t
  ;; :defer t
  :straight (vundo :type git :host github :repo "casouri/vundo")
  :config
  (setq vundo-compact-display nil	; Take less on-screen space.
        vundo-glyph-alist vundo-ascii-symbols
	;; vundo-unicode-symbols
	)
  
  ;; Use `HJKL` VIM-like motion, also C-a/C-e to jump around.
  (define-keys vundo-mode-map
    "l" #'vundo-forward
    "h" #'vundo-backward
    "j" #'vundo-next
    "k" #'vundo-previous
    [?\C-a] #'vundo-stem-root
    [?\C-e] #'vundo-stem-end
    ;; "q" #'vundo-quit
    ;; [?\C-g] #'vundo-quit
    [return] #'vundo-confirm)
  :init (autoload 'vundo "vundo"))
;; }}

;; winner undo/redo
(add-hook 'after-init-hook #'winner-mode)
(setq winner-dont-bind-my-keys nil)

;; to ignore 'Remote file error: Forbidden reentrant call of Tramp'
;; (setq debug-ignored-errors
;;       (cons 'remote-file-error debug-ignored-errors))
;; }}

;; uniquify
;; Nicer naming of buffers for files with identical names
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; hippie-expand
;; Since we got company-ispell and `M-x toggle-company-ispell'
;; Done, now we just use it as a clause in our make-hippie-expand-function (as above)
(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))
(global-set-key (kbd "M-/") 'hippie-expand)

(define-key minibuffer-local-map [escape] 'keyboard-escape-quit)

(provide 'init-essential)
;;; init-essential.el ends here
