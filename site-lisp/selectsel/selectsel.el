;;; selectsel.el --- the counsel for selectrum

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Danny He <o28c14@gmail.com>
;; Package-Requires: ((emacs "24.1"))
;; Version: 1.0.0
;; Keywords: extensions elisp
;; Prefix: selectrum
;; Separator: -

;;; Commentary:

;; The counsel package for selectrum
;;
;;; Code:

(require 'cl-lib)

;; utils

;;;###autoload
(defun selectsel--replace-search (cands)
  "Replace the regex-str in CANDS with `read-string'."
  (cond
   ((string-empty-p selectrum--last-input) (message "enter some text first!"))
   (t (let* ((regex-str selectrum--last-input) ;; target string to be replaced
	     (cands (seq-remove (lambda (x) (not (string-match-p regex-str x)))
				cands))
	     (cands-fn
	      (lambda (in)
		(let ((newstr (concat regex-str in)))
		  ;; add face to regex-str to be replaced
		  (add-face-text-property
		   0 (length regex-str)
		   '(compilation-mode-line-fail :strike-through t)
		   nil newstr)
		  (mapcar (lambda (cand)
			    (replace-regexp-in-string regex-str newstr cand nil t))
			  cands))))
             (selectrum--last-input regex-str) ;; preserve outter seesion
	     (cand
	      (let ((enable-recursive-minibuffers t)
                    (selectrum-preprocess-candidates-function #'identity))
                (selectrum--read (format "Replace \"%s\" with: "
					 (substring-no-properties regex-str))
				 cands-fn))))
	(let ((last-input selectrum--last-input)
	      (buf (seq-find (lambda (x) (not (minibufferp x)))
			     (buffer-list)))
	      (line-num ;; start replacement starting from selected cand line
	       (string-to-number
                (or (get-text-property 0 'selectrum-candidate-display-prefix cand)
                    ""))))
	  (with-selected-window (get-buffer-window buf)
	    (with-current-buffer buf
	      (query-replace-regexp
               regex-str last-input nil
	       (save-excursion
		 (goto-char (point-min))
		 (forward-line (1- line-num))
		 (point))
	       (point-max) nil)
	      (exit-minibuffer))))))))

;; selectsel-swiper

(defun selectsel--swiper-candidates (&optional beg end)
  (let ((inhibit-field-text-motion t)
	(beg (or beg (point-min)))
	(end (or end (point-max))))
    (save-excursion
      (goto-char beg)
      (cl-loop
       with n-lines = (count-lines beg end)
       with number-format = (concat
			     "%" (number-to-string
				  (ceiling (log (1+ n-lines) 10))) "d ")
       repeat n-lines
       for num from 1
       for line-end = (line-end-position)
       for buffer-line = (buffer-substring (point) line-end)
       when (not (string-empty-p buffer-line)) ; Just skip empty lines.
       collect
       ;; (propertize
       ;;  (format number-format num)
       ;;  'face 'completions-annotations)
       (propertize buffer-line
		   'selectrum-candidate-display-prefix
		   (format number-format num))
       do (goto-char (1+ line-end))))))

(defvar selectsel-swiper-history nil
  "Submission history for `selectsel-swiper'.")

(defun selectsel-rename-in-defun ()
  (interactive)
  (let ((selectrum--last-input
	 (util/thing-at-point/deselect)))
    (save-excursion
      (beginning-of-defun)
      (push-mark nil t t)
      (selectsel--replace-search
       (let* ((beg (point))
	      (end (progn (end-of-defun)
			  (point))))
	 (selectsel--swiper-candidates beg end))))))

(defun selectsel-swiper (&optional initial-input)
  "Search for a matching line and jump to the beginning of its text.
Obeys narrowing.  Can have INITIAL-INPUT"
  (interactive)
  (let* ((cands (selectsel--swiper-candidates))
	 (current-line-number (line-number-at-pos (point) t))
         (selectrum-minibuffer-map
	  (let ((map (make-sparse-keymap)))
	    (set-keymap-parent map selectrum-minibuffer-map)
	    (define-key map (kbd "M-q")
	      (lambda ()
		(interactive)
		(selectsel--replace-search cands)))
	    map))
         ;; (selectrum-preprocess-candidates-function #'identity)
         (selectrum-move-default-candidate nil)
	 (chosen-line
          (selectrum--read "Selectrum Swiper: "
			   cands
			   :default-candidate (nth (1- current-line-number) cands)
			   :initial-input initial-input
			   :history 'selectsel-swiper-history
			   :require-match t))
         (chosen-line-number-str
	  (get-text-property 0 'selectrum-candidate-display-prefix chosen-line)))
    (when chosen-line-number-str
      (push-mark (point) nil)
      (forward-line (- (string-to-number chosen-line-number-str)
		       current-line-number))
      (selectsel--yank-search selectrum--last-input))))

;; imenu


(defun selectsel--imenu-candidates ()
  (require 'imenu)
  (let* ((imenu-auto-rescan t)
         (imenu-auto-rescan-maxout (if current-prefix-arg
                                       (buffer-size)
                                     imenu-auto-rescan-maxout))
         (items (imenu--make-index-alist t))
         (items (delete (assoc "*Rescan*" items) items)))

    (when (eq major-mode 'emacs-lisp-mode)
      (when-let ((fns (cl-remove-if #'listp items :key #'cdr)))
	    (setq items (nconc (cl-remove-if #'nlistp items :key #'cdr)
			               `(("Function" ,@fns))))))
    (cl-labels ((get-candidates
		         (alist &optional prefix)
                 (cl-mapcan
                  (lambda (elm)
                    (if (imenu--subalist-p elm)
                        (get-candidates
                         (cl-loop for (e . v) in (cdr elm)
                                  collect
                                  (cons e (if (integerp v) (copy-marker v) v)))
                         (concat prefix
				                 (and prefix ".")
				                 (car elm)))
                      (list (propertize
			                 (car elm)
			                 'selectrum-candidate-display-prefix
			                 (when prefix
			                   (concat (propertize prefix 'face
						                           'font-lock-keyword-face)
				                       ": "))
                             'imenu-marker
                             (if (overlayp (cdr elm))
				                 (overlay-start (cdr elm))
			                   (cdr elm))))))
                  alist)))
      (get-candidates items))))

(defun selectsel-imenu ()
  "`imenu' interfacing with `completing-read'."
  (interactive)
  (let* ((cands (selectsel--imenu-candidates))
	     (cand (completing-read "imenu items: " cands
                                nil
                                t))
         (marker (get-text-property 0 'imenu-marker cand)))
    (imenu marker)))

;; selectrum-rg

(defvar selectsel--rg-history nil
  "History for `selectrum-rg'.")

(defvar selectrum-rg-base-cmd
  "rg -M 240 --with-filename --no-heading --line-number --color never -S -e <R>"
  "Selectrum rg base cmd, can be used to set to use different command to grep.")

(autoload 'grep-expand-template "grep" "")
(autoload 'counsel--elisp-to-pcre "counsel" "")

(defun selectsel--rg-preprocess-candidates (cands)
  (mapcar (lambda (c)
	    (if (not (string-match "\\`\\([^:]+\\):\\([^:]+\\):\\(.*\\)" c))
		c
	      (let ((file-name (match-string 1 c))
		    (line-num (match-string 2 c))
		    (match (match-string 3 c)))
		(add-face-text-property ;; file name
		 0 (length file-name)
		 'compilation-info
		 nil file-name)
		(add-face-text-property ;; line number
		 0 (length line-num)
		 '(:underline nil :inherit compilation-line-number)
		 nil line-num)
		(propertize match
			    'selectrum-candidate-display-prefix
			    (concat file-name ":" line-num ":")))))
          cands))

(defun selectsel-rg (&optional initial-input)
  "My selectrum interface to rg, takes on INITIAL-INPUT."
  (interactive (list (util/thing-at-point)))
  (let* ((command selectrum-rg-base-cmd)
	 (selectrum-preprocess-candidates-function
          'selectsel--rg-preprocess-candidates)
	 (cands (lambda (in)
		  (if (< (length in) 3)
		      '("Input should more than 3 characters.")
		    (let* ((counsel--regex-look-around "--pcre2")
			   (regex
			    (counsel--elisp-to-pcre in counsel--regex-look-around)))
		      ;; (mapcar prop)
		      (split-string (shell-command-to-string
				     (grep-expand-template command regex))
				    "\n")))))
	 (cand (selectrum--read "rg: " cands
				:initial-input initial-input
				;; :may-modify-candidates t
				:history 'selectsel--rg-history
				:require-match nil))
	 (file-n-line (get-text-property 0 'selectrum-candidate-display-prefix cand)))
    (when (and file-n-line
               (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\'" file-n-line))
      (let ((file-name (match-string-no-properties 1 file-n-line))
	    (line-number (match-string-no-properties 2 file-n-line))
	    (input selectrum--last-input))
        ;; TODO: open in already opened buffer instead of current buffer
	(find-file file-name)
	(goto-char (point-min)) ;; reset to line 1
	(forward-line (1- (string-to-number line-number)))
	(selectsel--yank-search input)))))

;; selectrum-ffip

(defvar selectsel-search-file-max-depth 3
  "The maximum depth `selectrum-search-file-list' will reach into, default is 3.")

(defun selectsel--ffip-project-root ()
  "Return project root or `default-directory'."
  (if-let ((project-root
	    ;; (cond ((listp ffip-project-file))
	    ;; 		(t (locate-dominating-file default-directory
	    ;; 								 ffip-project-file)))
	    (cl-some (apply-partially #'locate-dominating-file
                                      default-directory)
		     ffip-project-file)))
      (file-name-as-directory project-root)
    default-directory))

(defun selectsel--dir-tree-list (root-path)
  "Generate a list of files recursively starting from ROOT-PATH
as deep as `selectrum--search-file-max-depth'"
  (cl-labels
      ((aux
        (dir-path depth)
        (cond ((< depth selectsel-search-file-max-depth)
	       (if (file-directory-p dir-path)
		   (cl-reduce (lambda (acc f)
				(let ((f (format "%s/%s" dir-path f)))
				  (if (file-directory-p f)
				      (append (aux f (1+ depth))
					      acc)
				    (cons f acc))))
			      (directory-files dir-path nil "[^.]")
			      :initial-value nil)
		 (list dir-path)))
	      ((file-directory-p dir-path) (list (file-name-as-directory dir-path)))
              (t (list dir-path)))))
    (let ((default-directory root-path))
      (aux "." 0))))

(defun selectsel-ffip ()
  "Find a file in project."
  (interactive)
  (let* ((collection (selectsel--dir-tree-list (selectsel--ffip-project-root)))
		 (cand (completing-read "Search files:" collection)))
    (when cand
      (find-file cand))))


(defun selectsel-recentf-directories (&optional initial-input)
  "Completion interface for recent directories."
  (interactive (list (and (region-active-p)
		                  (util/selected-str))))
  (let* ((directories
          (cl-remove-duplicates
           (mapcar (lambda (x)
                     (abbreviate-file-name (file-name-directory x)))
                   recentf-list)
           :test #'string=))
         (default-directory
           (completing-read
            "Find recent directories: "
	        directories
            nil t initial-input)))
    (call-interactively 'find-file)))

(defun selectsel-git-recentf (&optional initial-input)
  (interactive (list (and (region-active-p)
		                  (util/selected-str))))
  (find-file
   (completing-read "Find recent file (git): "
                    (inc0n/git-recent-files)
                    nil t initial-input)))

(defun selectsel--hash-coloured-modes ()
  "Selectsel created hashed table coloured modes."
  (let ((modes (make-hash-table :test #'equal)))
    (mapc (lambda (var)
	        (when (and (boundp var)
		               (symbol-value var))
	          (puthash
	           (or (get var :minor-mode-function) var)
	           (propertize (symbol-name var)
			               'face
			               'compilation-mode-line-exit)
	           modes)))
	      minor-mode-list)
    modes))

(defvar selectsel--M-x-history nil
  "History for `selectsel-M-x'.")

(defun selectsel-M-x ()
  ""
  (interactive)
  (command-execute
   (intern
    (completing-read
     "M-x "
     (let ((modes (selectsel--hash-coloured-modes))
	       (cmds nil))
       (obarray-map
	    (lambda (sym)
	      (when (commandp sym)
	        (push
	         (or (gethash sym modes)
		         (symbol-name sym))
	         cmds)))
	    obarray)
       cmds)
     nil
     'require-match
     nil
     'selectsel--M-x-history))
   'record))

(defun selectsel--preprocess-files (cands)
  "Selectsel created hashed table coloured mode.
Argument CANDS list of files to process."
  (mapcar (lambda (d)
	    (if (directory-name-p d)
		(propertize d 'face 'font-lock-keyword-face)
	      d))
	  (sort cands
		(lambda (x y)
		  (cond ((eq (directory-name-p x)
			     (directory-name-p y))
			 (string< x y))
			(t (directory-name-p x)))))))

(defun selectsel-find-file ()
  (interactive)
  (let ((selectrum-preprocess-candidates-function 'selectsel--preprocess-files)
        (selectrum-max-window-height 15)) ;; display more files
    (selectrum-read-file-name "File file: ")))
;;

;; quick repeat

(defvar selectsel-quick-navigate-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" [?\C-n])
    (define-key map "p" [?\C-p])
    (define-key map "q" #'abort-recursive-edit)
    map))

(defun selectsel-quick-repeat ()
  "Quick navigation variant of `selectrum-repeat'."
  (interactive)
  (let ((selectrum-minibuffer-map selectsel-quick-navigate-map)
        (minibuffer-message-timeout 0.3))
    (minibuffer-message "quick navigate n/p/q")
    (selectrum-repeat)))

(defun selectsel-choice-read (choices)
  "Quick CHOICES that should be a list of symbols."
  (set-keymap-parent selectsel-quick-navigate-map vertico-map)
  (let ((vertico-map selectsel-quick-navigate-map))
    ;; selectrum-minibuffer-map
    (intern
     (completing-read
      "Choose: "
      (mapcar #'symbol-name choices)))))

(defun selectrum-org-headlines-candidates (&optional scope)
  "See `org-map-entries' for the definition of SCOPE."
  (org-map-entries
   (lambda ()
     (cl-destructuring-bind (level1 level2 todo priority text tags)
	     (org-heading-components)
       (let ((headline
	          (mapconcat ;; concat list
	           'identity
	           (delq ;; remove nil
                nil
		        (list (make-string level1 ?*)
		              ;; (and priority (format "[#%c]" priority))
		              (mapconcat 'identity
				                 (append
				                  (org-get-outline-path)
				                  (list ""))
				                 "/")
		              todo
		              text
		              tags))
	           " ")))
	     (propertize headline
		             'selectrum-candidate-display-prefix
		             (propertize
		              (format "%s:" (file-name-nondirectory buffer-file-name))
		              'face 'completions-annotations)
		             'marker (point-marker)))))
   nil
   (or scope 'agenda)))


;;;; ChangeLog:

;; 2020-10-15  Danny He <o28c14@gmail.com>
;;
;; 	selectsel: New package
;;
;; 2021-02-08  Danny He <o28c14@gmail.com>
;;
;; 	some tweak to swiper-rg and swiper
;;
;; 2021-03-02
;; added selectsel-list-package

(provide 'selectsel)
;;; selectsel.el ends here
