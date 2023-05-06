;;; init-git --- -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require-package 'magit)
(with-eval-after-load 'magit
  ;; (define-key magit-mode-map [C-tab] 'nil)
  )

;; {{
(setq vc-handled-backends '(Git SVN Hg))
;; @see https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
;; open files faster but you can't check if file is version
;; controlled. other VCS functionality still works.
(when nil (remove-hook 'find-file-hook #'vc-refresh-state))
;; }}

(defun my/git-commit-id ()
  "Select commit id from current branch."
  (let* ((git-cmd "git --no-pager log --date=short --pretty=format:'%h|%ad|%s|%an'")
         (collection (util/shell-command-to-lines git-cmd))
         (item (completing-read "git log:" collection)))
    (car (split-string item "|" t))))

(defun my/git-show-commit-internal ()
  "Show git commit."
  (when-let ((id (my/git-commit-id)))
    (shell-command-to-string (format "git show %s" id))))

(defun my/git-show-commit ()
  "Show commit using ffip."
  (interactive)
  (let ((ffip-diff-backends
		 '(("Show git commit" . my/git-show-commit-internal))))
    (ffip-show-diff 0)))

(defun git-get-current-file-relative-path ()
  "Get relative path of current file for Git."
  (replace-regexp-in-string (concat "^" (file-name-as-directory default-directory))
                            ""
                            buffer-file-name))

;; git checkout <filename>
;; git add <filename>

(defvar git-commit-message-history nil)
(defun git-commit-tracked ()
  "Run 'git add -u' and commit."
  (interactive)
  (let* ((hint "Commit tracked files. Please input commit message (Enter to abort):")
         (msg (read-from-minibuffer hint
                                    nil
                                    nil
                                    nil
                                    'git-commit-message-history)))
    (cond
     ((and msg (> (length msg) 3))
      (shell-command "git add -u")
      (shell-command (format "git commit -m \"%s\"" msg))
      (message "Tracked files is committed."))
     (t
      (message "Do nothing!")))))

;; {{
(defun my/git-extract-based (target lines)
  "Extract based version from TARGET from LINES."
  (cl-loop with regexp-target = (regexp-quote target)
	   for i from 0
	   for line in lines
	   until (string-match regexp-target line)
	   finally return
	   ;; find child of target commit
	   (and (< 0 i)
		(< i (length lines))
		(replace-regexp-in-string "^tag: +"
					  ""
					  (car (split-string (nth (1- i) lines)
							     " +"))))))

(defun my/git-rebase-interactive (&optional user-select-branch)
  "Rebase interactively on the closest branch or tag in git log output.
If USER-SELECT-BRANCH is not nil, rebase on the tag or branch selected by user."
  (interactive "P")
  (let* ((cmd "git --no-pager log --decorate --oneline -n 1024")
         (lines (util/shell-command-to-lines cmd))
         (targets (mapcan (lambda (e)
                            (and (string-match "^[a-z0-9]+ (\\([^()]+\\)) " e)
                                 (not (string-match "^[a-z0-9]+ (HEAD " e))
				 (list (match-string 1 e))))
                          lines))
         (based (cond
		 ((or (not targets) (eq (length targets) 0))
		  (message "No tag or branch is found to base on."))
		 ((or (null user-select-branch)) (eq (length targets) 1)
		  ;; select the closest/only tag or branch
		  (my/git-extract-based (nth 0 targets) lines))
		 (t
		  ;; select the one tag or branch
		  (my/git-extract-based (completing-read "Select based: " targets)
					lines)))))
    ;; start git rebase
    (when based
      (magit-rebase-interactive based nil))))
;; }}

;; {{ git-gutter use ivy
(defun my/reshape-git-gutter (gutter)
  "Re-shape GUTTER for `ivy-read'."
  (let ((linenum-start (aref gutter 3))
        (linenum-end (aref gutter 4))
        (target-line "")
        (target-linenum 1)
        (max-line-length 0))
    (save-excursion
      (while (<= linenum-start linenum-end)
        (goto-char (point-min))
        (forward-line linenum-start)
        (let ((tmp-line
               (string-trim (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position)))))
          (when (> (length tmp-line) max-line-length)
	    (setq target-linenum linenum-start)
	    (setq target-line tmp-line)
	    (setq max-line-length (length tmp-line)))

          (setq linenum-start (1+ linenum-start)))))
    ;; build (key . linenum-start)
    (cons (format "%s %d: %s"
                  (if (eq 'deleted (aref gutter 1))
		      "-"
		    "+")
                  target-linenum
		  target-line)
          target-linenum)))

(defun my/goto-git-gutter ()
  (interactive)
  (if git-gutter:diffinfos
      (let* ((cands (mapcar #'my/reshape-git-gutter
			    git-gutter:diffinfos))
             (e (completing-read "git-gutters:" cands))
             (e (cdr (assoc e cands))))
	;; (unless (numberp e) (setq e (cdr e)))
	(goto-line e))
    (message "NO git-gutters!")))
;; }}

(defun my/git-files-in-rev-command (rev level)
  "Return git command line to show files in REV and LEVEL."
  (concat "git diff-tree --no-commit-id --name-only -r "
          rev
          (make-string (or level 0) ?^)))

(defun my/git-root-dir ()
  "Git root directory."
  ;; (project-try-vc default-directory)
  (locate-dominating-file default-directory ".git"))

(defun my/git-find-file-in-commit (&optional arg)
  "Find file in previous commit with ARG.
If ARG is 1, find file in previous commit."
  (interactive "P")
  (let* ((rev (concat "HEAD" (and (eq arg 1) "^")))
         (prompt (format "Find file from commit %s" rev))
         (cmd (my/git-files-in-rev-command rev arg))
         (default-directory (my/git-root-dir))
         (file (completing-read prompt (util/shell-command-to-lines cmd))))
    (find-file file)))

(defun util/in-one-line-p (b e)
  "Check if B and E positions are in the same line."
  (save-excursion
    (goto-char b)
    (and (<= (line-beginning-position) e)
         (<= e (line-end-position)))))

(defun my/git-log-trace-definition ()
  "Similar to `magit-log-trace-definition' but UI is simpler.
If multi-lines are selected, trace the definition of line range.
If only one line is selected, use current selection as function name to look up.
If nothing is selected, use the word under cursor as function name to look up."
  (interactive)
  (when buffer-file-name
	(let* ((range-or-func
			(if (region-active-p)
				(if (util/in-one-line-p (region-beginning) (region-end))
					(format ":%s" (util/selected-str))
				  (format "%s,%s"
						  (line-number-at-pos (region-beginning))
						  (line-number-at-pos (1- (region-end)))))
			  (format ":%s" (thing-at-point 'symbol))))
		   (cmd (format "git log -L%s:%s"
						range-or-func
						(file-truename buffer-file-name)))
		   (result (shell-command-to-string cmd)))
	  (when (string-match-p "no match" result)
		;; mark current function and try again
		(mark-defun)
		(setq range-or-func (format "%s,%s"
									(line-number-at-pos (region-beginning))
									(line-number-at-pos (1- (region-end)))))
		(setq cmd (format "git log -L%s:%s" range-or-func (file-truename buffer-file-name))))
	  ;; (message cmd)
	  ;; TODO Deal with the output
	  (shell-command-to-string cmd))))

(provide 'init-git)
;;; init-git.el ends here
