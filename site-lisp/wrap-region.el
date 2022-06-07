;;; Commentary:
;;
;;; Code:

(defvar-local wrap-region-pairs-alist
  '((?\( . ("(" . ")"))
    (?\[ . ("[" . "]"))
    (?\{ . ("{" . "}"))

    (?\) . ("( " . " )"))
    (?\] . ("[ " . " ]"))
    (?\} . ("{ " . " }"))

    (?# . ("#{" . "}"))
    (?< . ("<" . ">")))
  "The pairs alist that will be used in `wrap-region-actiavte'.")

(defvar wrap-region-mode-pairs
  '((emacs-lisp-mode
     (?` . ("`" . "'"))))
  "The additional pairs that will be prepended to
`wrap-region-pairs-alist' according to `major-mode'.")

(defvar wrap-region-fallback-handler #'wrap-region-default-fallback-handler
  "The fallback handler to handle when key is not defined in
`wrap-region-pairs-alist'.")

(defvar wrap-region-evil-deactivate-region-after-wrap t)
(defvar-local wrap-region--last-region nil)

;;; Functions

(defun wrap-region-default-fallback-handler (key)
  "This default handler for `wrap-region'.
It wrap around region with KEY."
  `(,(string key) . ,(string key)))

(defun wrap-region-major-mode-hook-setup ()
  "Set up surround shortcuts."
  (when-let ((mode (assq major-mode wrap-region-mode-pairs)))
    (setq wrap-region-pairs-alist (append mode wrap-region-pairs-alist))))
(add-hook 'after-change-major-mode-hook 'wrap-region-major-mode-hook-setup)

(defun wrap-region-insert (pair region)
  "Insert PAIR around REGION."
  (pcase-let ((`(,beg . ,end) region))
    (setq-local wrap-region--last-region
		(cons beg (1+ end)))
    (save-excursion
      (goto-char end)
      (insert (cdr pair)))
    (goto-char beg)
    (insert (car pair))))

(defun wrap-region--region ()
  (unless (region-active-p)
    (mark-sexp))
  ;; (if (region-active-p))
  (cons (region-beginning)
	(region-end))
  ;; (bounds-of-thing-at-point 'sexp)
  )

(defun wrap-region--get-pair ()
  (let* ((key (read-char-exclusive "Press key..." ))
         (pair (assq key wrap-region-pairs-alist)))
    (or (cdr pair)
	(funcall (or wrap-region-fallback-handler
		     #'wrap-region-default-fallback-handler)
		 key))))

;;;###autoload
(defun wrap-region-activate ()
  (interactive)
  (if (eq last-command 'wrap-region-activate)
      (wrap-region-swap wrap-region--last-region)
    (let ((region			; ensure this gets evaluated priority
	   (wrap-region--region)))
      (wrap-region-insert
       (wrap-region--get-pair)
       region))))

;;;###autoload
(defun wrap-region-evil-activate ()
  (interactive)
  (when (bound-and-true-p evil-mode)
    (evil-with-state insert
      (call-interactively 'wrap-region-activate))
    (when wrap-region-evil-deactivate-region-after-wrap
      (deactivate-mark))))

(defun wrap-region-swap (region)
  "Swap the wrapped REGION BEG and END with a different pair of keys."
  (pcase-let ((`(,beg . ,end) region)
	      (pair (wrap-region--get-pair)))
    (save-excursion
      (goto-char end)
      (delete-char 1)
      (insert (cdr pair)))
    (goto-char beg)
    (delete-char 1)
    (insert (car pair))))

(provide 'wrap-region)
;;; wrap-region.el ends here