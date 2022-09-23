;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun util/shell-command-to-lines (command)
  "Return lines of COMMAND output."
  (split-string (shell-command-to-string command)
		        "[\r\n]+" t))

(defun util/make-file (file-path &optional init-content)
  "Create the file at FILE-PATH if not exist.
And insert INIT-CONTENT if non-nil."
  (unless (file-exists-p file-path)
    (with-temp-buffer
      (when (stringp init-content)
	(insert init-content))
      (write-file (file-truename file-path)))))

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun add-intepreter-mode (mode pattern)
  "Add entries to `interpreter-mode-alist' to use `MODE' for `PATTERN'."
  (add-to-list 'interpreter-mode-alist (cons pattern mode)))

(defun add-to-list/s (list-var elements &optional append)
  "Add multiple ELEMENTS to the `LIST-VAR'."
  (dolist (elm elements)
    (add-to-list list-var elm append))
  (symbol-value list-var))

(defun region-or-min-max-points ()
  (if (region-active-p)
      (list (region-beginning) (region-end))
    (list (point-min) (point-max))))

(defun region-or-line-beg-end-points (&optional n)
  "Get region or line points N move line forward."
  (if (region-active-p)
      (list (region-beginning) (region-end))
    (list (line-beginning-position)
          (save-excursion
            (when (and (integerp n)
                       (> (abs n) 1))
              (forward-line
               ;; off set by 1, which is the current line
               (if (> n 0) (1- n) (1+ n))))
            ;; save guard with `point-max'
            (min (point-max)
                 (1+ (line-end-position)))))))

(defsubst util/selected-str ()
  "Get string of selected region."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun util/use-selected-string-or-ask (hint &optional default-string)
  "Use selected region or ask for input.
If HINT is empty, throw an error.
Optional argument DEFAULT-STRING default string to return from `read-string'."
  (cond ((or (not (stringp hint))
             (string-empty-p hint))
         (error "Empty prompt, %s" hint))
        ((region-active-p) (util/selected-str))
        ((stringp default-string)
         (read-string (concat hint " (" default-string "): ")
                      default-string
                      nil))
        (t (read-string (concat hint ": ") "" nil))))

(defun util/thing-at-point ()
  "Get thing at point.  Gotten from `ivy-thing-at-point'.
If region is active get region string.
Else use `thing-at-point' to get current string 'symbol."
  (cond ((use-region-p)
		 (buffer-substring-no-properties (region-beginning) (region-end)))
        ((and (not (= (point-max) (point)))
			  (char-equal ?\s (char-after)))
         "")
        ;; ((thing-at-point 'url))
        ((let ((s (thing-at-point 'symbol :no-properties)))
           (and (stringp s)
				(if (string-match "\\`[`']?\\(.*?\\)'?\\'" s)
					(match-string 1 s)
				  s))))
        (:else "")))

(defun util/thing-at-point/deselect ()
  "Get thing at point.
If region is active get region string and deactivate."
  (prog1 (util/thing-at-point)
    (when (region-active-p)
      (deactivate-mark))))

;; files, very very long line counter-measurement
(use-package so-long :ensure nil
  :defer 1
  :config
  (setq so-long-predicate 'buffer-too-big-p) ; was so-long-statistics-excessive-p
  (defun buffer-too-big-p ()
	"If buffer is more than 3000 lines, each line >= 60 bytes."
	(cl-destructuring-bind (line longest-line-len mean-line-len)
		(buffer-line-statistics)
	  (if (apply 'derived-mode-p so-long-target-modes)
		  (> longest-line-len so-long-threshold)
		(if (derived-mode-p 'text-mode)
			(and (> line 3000)
				 (> mean-line-len 60))
		  (error "Buffer mode (%s) is not supported" major-mode)))))
  :init (global-so-long-mode +1))

(defun delete-this-buffer-and-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (if-let ((file-name (buffer-file-name)))
      (when (y-or-n-p (format "Really delete file and buffer '%s'? "
                              (file-name-nondirectory file-name)))
        ;; (if (vc-backend file-name)
        ;;     (vc-delete-file file-name))
        (delete-file file-name)
        (kill-this-buffer))
    (message "No file is currently being edited")))

;; from http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun rename-this-buffer-and-file ()
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive)
  (let* ((filename (buffer-file-name))
         (new-name (read-file-name "New name: " filename)))
    (if (and (file-exists-p filename)
             (vc-backend filename))
        (vc-rename-file filename new-name)
      (rename-file filename new-name 1) ;; will ask for confirmation
      (message "rename dir: %s" (file-directory-p new-name))
	  (if (file-directory-p new-name)
          (progn
            ;; append the original name with new directory
            (setq new-name (expand-file-name filename new-name))
            (setq default-directory (expand-file-name filename
                                                      default-directory))
            ;; (cd (file-name-directory new-name))
            ))
      (rename-buffer (buffer-name) t)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil))))

(defun copy-this-buffer-and-file ()
  "Copy the current buffer and file it is visiting.
If the old file is under version control, the new file is added into
version control automatically."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (file-exists-p filename))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (copy-file filename new-name t)
        (rename-buffer new-name t)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)
        ;; (when (vc-backend filename)
        ;;   (vc-register))
		))))

;;

(defun custom/reset-var (symbl)
  "Reset SYMBL to its standard value."
  (interactive (list (intern (read-string "Symbol: "))))
  (set symbl (eval (car (get symbl 'standard-value)))))

(defmacro define-hook-setup (hook-name &rest body)
  "Macro helper for `add-hook' to HOOK-NAME.
It will defun setup hook function with BODY.  The setup hook
function will have the name `HOOK-NAME'-setup"
  (declare (debug (&define name
                           [&optional additional-name]
                           [&optional lambda-doc]
                           [&rest def-body]))
           (indent defun)
           (doc-string 2))
  (unless (or (symbolp hook-name)
              (and (listp hook-name) (eq (car hook-name) 'quote)))
    (warn "bad define-hook-setup name %s" hook-name))
  (let* ((hook-name (if (listp hook-name)
                        (cadr hook-name)
                      hook-name))
         (hook-setup-fn-name
          (intern (concat (symbol-name hook-name)
                          (if (keywordp (car body))
                              (concat "-"
                                      (substring (symbol-name (car body)) 1))
                            "")
                          "-setup"))))
    `(add-hook ',hook-name
               (defun ,hook-setup-fn-name ()
                 ,@(if (keywordp (car body))
                       (cdr body)
                     body)))))

(defmacro define-keys (keymap &rest key-binds)
  "Macro helper for `define-key' with KEYMAP and KEY-BINDS."
  ;; (declare (debug (&define name
  ;;                          [&rest def-body])))
  `(progn
     ,@(cl-loop for (key bind) on key-binds by #'cddr
                collect `(define-key ,keymap ,key ,bind))
     ,keymap))

;; (defmacro -command (&rest sequence-of-cmds)
;;   `(lambda ()
;;      (interactive)
;;      ,@(cl-loop for cmd in sequence-of-cmds
;;                 collect
;;                 (cl-typecase cmd
;;                   (string cmd)
;;                   (vector `(call-interactively
;;                             (or (key-binding ,cmd)
;;                                  'undefined)))
;;                   (list cmd)
;;                   (symbol `(call-interactively ',cmd))))))

(defun my/unbound-symbol (arg)
  "Unbound for ARG be it function or symbol."
  (interactive
   (list
    ;; save guard against potential hazardous unbound.
    (or (save-excursion
          (beginning-of-defun)
          (forward-char)
          ;; only allow unbinding defun or defvar
          (if (member (thing-at-point 'symbol t) '("defun" "defvar"))
              (progn (forward-sexp 2) (thing-at-point 'symbol t))))
        (let ((str (thing-at-point 'symbol t)))
            (if (y-or-n-p (format "Unbound symbol `%s'?" str))
                str
              (user-error "Can only unbound symbols of defun or defvar, got %S"
                          (thing-at-point 'symbol t)))))))
  ;; (message "unexpected %s" arg)
  (cond ((stringp arg)
         ;; This is needed because all `thing-at-point' returns string
         (my/unbound-symbol (intern arg)))
        ((symbolp arg)
	     (if (or (macrop arg) (functionp arg))
	         (progn (fmakunbound arg)
		            (message "fmakunbounded %s" arg))
	       (makunbound arg)
	       (message "makunbounded %s" arg)))
        (t (message "unexpected %s" arg))))

;; https://emacs.stackexchange.com/questions/46664/switch-between-horizontal-and-vertical-splitting
(defun rotate-two-split-window ()
  "Toggle two window layout vertically or horizontally."
  (interactive)
  (if (= (count-windows) 2)
      (let ((this-win (selected-window))
            (next-win (next-window)))
        (let ((this-win-edges (window-edges this-win))
              (next-win-edges (window-edges next-win))
              (this-win-buffer (window-buffer this-win))
              (next-win-buffer (window-buffer next-win)))
          (delete-window next-win)
          (let ((new-next-win
                 (funcall (if (= (car this-win-edges)
                                 (car next-win-edges))
                              #'split-window-horizontally
                            #'split-window-vertically))))
            ;; if this window was the 2nd window
            (when (not (and (<= (car this-win-edges)
                                (car next-win-edges))
                            (<= (cadr this-win-edges)
                                (cadr next-win-edges))))
              (setq this-win (next-window)
                    new-next-win (selected-window)))
            (select-window this-win)
            (set-window-buffer this-win this-win-buffer)
            (set-window-buffer new-next-win next-win-buffer))))
    (message "can only rotate two windows at a time")))

(provide 'init-utils)
;;; init-utils.el ends here
