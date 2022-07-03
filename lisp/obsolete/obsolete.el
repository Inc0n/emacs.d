
;;; Commentary:
;; some of the unused code maybe useful for storage purposes

;;; Code:

(defun how-to-make-transient-map ()
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map "f" #'evil-jump-forward)
     (define-key map "b" #'evil-jump-backward)
     map)
   t))

(defun my/get-total-hours (beg end)
  "Calculate the total time in the region between BEG and END."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (let ((total-hours 0))
      (while (search-forward-regexp "\\([0-9][0-9.]*\\)h" end t)
        (cl-incf total-hours
                 (string-to-number (match-string 1))))
      (message "total-hours=%s" total-hours))))

(defun toggle-env-http-proxy ()
  "Set/unset the environment variable http_proxy used by w3m."
  (interactive)
  (let ((proxy "http://127.0.0.1:8000"))
    (if (string= (getenv "http_proxy") proxy)
        (setenv "http_proxy" "")
      (setenv "http_proxy" proxy))
    (message "env http_proxy is %s now" (or proxy "empty"))))

(defun my/select-from-kill-ring (n)
  "If N > 1, yank the Nth item in `kill-ring'.
If N is nil, use `completing-read' to browse `kill-ring'."
  (interactive "P")
  (let* ((candidates
		  (cl-remove-if
           (lambda (s)
             (or (< (length s) 5)
                 (string-match-p "\\`[\n[:blank:]]+\\'" s)))
           (delete-dups kill-ring)))
         (cand (if (null n)
	               (completing-read "Browse `kill-ring':" candidates)
                 (nth n candidates))))
    (util/insert-str cand)
    (kill-new cand)))

(defun my/company-tab ()
  (interactive)
  (if (cl-find-if (lambda (s) (string-equal company-prefix s))
                  company-candidates)
      (company-abort)
    (company-complete-common)))

;; company completion with just numbering
(defun my/company-number (num)
  "Forward to `company-complete-number'. Unless the number is
	potentially part of the candidate. In that case, insert the
	number."
  (interactive (list (string-to-number (this-command-keys))))
  (let ((n (if (zerop num)
               10
             num))
        (re (concat "^" company-prefix (number-to-string num))))
    (if (or (not (company-tooltip-visible-p))
            (cl-find-if (lambda (s) (string-match re s))
                        company-candidates)
            (null (cdr company-candidates)) ;; if list is single (len==1)
            (> n (length company-candidates))
            (looking-back "[0-9]+\\.[0-9]*" (line-beginning-position)))
        (self-insert-command 1)
      (company-complete-number n))))

(defun company-kill-ring (command &optional arg &rest ignored)
  "A function `company-mode' completion backend existing file names.
Completions works for proper absolute and relative files paths.
File paths with spaces are only supported inside strings.
Argument COMMAND .
Optional argument ARG .
Optional argument IGNORED is ignored."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-kill-ring))
    (prefix (company-grab-word))
    (candidates (let ((regex (concat "^" (regexp-quote arg))))
                  (cl-remove-if
                   (lambda (s)
                     (or (< (length s) 5)
                         (string-match-p "\\`[\n[:blank:]]+\\'" s)
                         (not (string-match-p regex s))))
                   (delete-dups kill-ring))))
    ;; (location (cons (dired-no select
    ;;                  (file-name-directory (directory-file-name arg))) 1))
    ;; (post-completion (company-files--post-completion arg))
    (sorted t)
    (no-cache t)))


;; {{ check attachments
(defun my/message-current-line-cited-p ()
  "Indicate whether the line at point is a cited line."
  (save-match-data
    (string-match (concat "^" message-cite-prefix-regexp)
                  (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position)))))

(defun my/message-says-attachment-p ()
  "Return t if the message suggests there can be an attachment."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (re-search-forward "\\(attach\\|pdf\\|file\\|screen ?shot\\)" nil t))))

(defun my/message-has-attachment-p ()
  "Return t if an attachment is already attached to the message."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (re-search-forward "<#part" nil t))))

(defun my/message-pre-send-check-attachment ()
  "Check attachment before send mail."
  (when (and (my/message-says-attachment-p)
             (not (my/message-has-attachment-p)))
    (unless
		(y-or-n-p "The message suggests that you may want to attach something, but no attachment is found.  Send anyway? ")
      (error "It seems that an attachment is needed, but none was found.  Aborting sending"))))
(add-hook 'message-send-hook #'my/message-pre-send-check-attachment)
;; }}

(defun transient-highlight-symbol-exit ()
  "Transient highlight symbol exit handler."
  (highlight-symbol-remove)
  (transient--do-exit))

;;;###autoload
(define-transient-command my/transient-highlight-symbol ()
  "The transient version of `highlight-symbol'.
Check for `highlight-symbol' for more information such as for SYMBOL."
  :transient-suffix 'my/transient-suffix
  :transient-non-suffix 'transient-highlight-symbol-exit
  ["Highlight symbol"
   ("n" "next" highlight-symbol-next-in-defun)
   ("p" "previous" highlight-symbol-prev-in-defun)
   ("q" "query replace" highlight-symbol-query-replace)]
  (interactive)
  (let ((symbol (or (highlight-symbol--get-symbol)
                    (error "No symbol at point"))))
    (unless (highlight-symbol--symbol-highlighted-p symbol)
      (highlight-symbol symbol)))
  (transient-setup 'my/transient-highlight-symbol))

(defun util/async-shell-command (command)
  "Util function for running a shell COMMAND asynchronously."
  (let ((process (start-process "Shell"
                                nil
                                shell-file-name
                                shell-command-switch
                                command)))
    (set-process-sentinel
     process
     (lambda (process signal)
       (let ((status (process-status process)))
         (when (memq status '(exit signal))
           (if (string= (substring signal 0 -1) "finished")
               (message "done: %s" command)
             (message "failed to run: %s" command))))))))


(defun util/comint-operate-on-input-region (fn)
  "Let FN operate on region of current shell input."
  (funcall fn
		   (process-mark (get-buffer-process (current-buffer)))
           (line-end-position)))

(defun util/comint-kill-current-input ()
  "Kill current input in shell."
  (util/comint-operate-on-input-region 'kill-region))

(defun util/comint-current-input ()
  "Get current input in shell."
  (string-trim
   (util/comint-operate-on-input-region 'buffer-substring-no-properties)))

(defun org-goto-item-between-region (start forward)
  "Find the item between START and END, direction is controlled by FORWARD."
  ;; (memq (org-element-property :type (org-element-at-point))
  ;;       '(plain-list headline))
  ;; (goto-char (line-beginning-position))
  (let ((forward (if forward 1 -1)))
    (forward-line forward)
    (while (invisible-p (org-element-property :begin (org-element-at-point)))
      (forward-line forward)))
  ;; (message "%s" (list (or (org-element-property :value elm)
  ;;                         (org-element-property :raw-value elm))
  ;;                     (org-element-type elm)))
  (and (not (eobp))
       (let ((item (org-in-item-p)))
         (or (if (and item start)
                 (and (/= start item) item)
               item)
             (and (org-at-heading-p) (point))
             ;; (unless (if forward (> (point) end) (< (point) end)))
             (org-goto-item-between-region start forward)))))

(defun read-current-expression ()
  "Read the sexp under the cusor."
  (let* ((mark (mark-marker))
         (old-pos (marker-position mark)))
    (set-marker mark (point) (current-buffer))
    (prog1 (read mark) ; read an expression
      ;; restore old marker
      (set-marker mark old-pos (current-buffer)))))


(defun highlight-copy-as-kill ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'evil-yank)
    ;; (paredit-copy-as-kill)
    (when-let* ((region
                 ;; (cons (point)
                 ;;               (save-excursion
                 ;;                 (paredit-forward)
                 ;;                 (point)))
                 (bounds-of-thing-at-point 'sexp))
                (overlay (make-overlay (car region) (cdr region))))
      (kill-new
       (buffer-substring-no-properties (car region) (cdr region)))
      ;; (overlay-put overlay 'face 'lazy-highlight)
      ;; (run-at-time 0.5 nil (lambda () (delete-overlay overlay)))
      )))

(defun comment-or-uncomment-paragraph (n)
  "Comment out a paragraph starting from the beginning of line text.
Argument N the number of paragraph to operate on."
  (interactive "p")
  (comment-or-uncomment-region
   (line-beginning-position)
   (progn (forward-paragraph (or n 1))
		  (point))))

(defun font-belongs-to (pos fonts)
  "Current font at POS belongs to FONTS."
  (let ((fontfaces (get-text-property pos 'face)))
    (delq nil
          (mapcar (lambda (f)
                    (member f fonts))
                  (if (listp fontfaces)
                      fontfaces
                    (list fontfaces))))))
(defmacro with-package (pkg options &rest body)
  "Macro wrapper for `require-packege' on PKG and BODY.
PKG can be wrapped with options.
Such as :require, :disable"
  (declare (debug (&define name
                           [&optional lambda-doc]
                           [&rest def-body]))
           (indent defun)
           (doc-string 2))
  `(progn ,(when (plist-get options :require)
             `(require-package ,pkg))
          ,@(unless (plist-get options :disable)
              body)))

;; 2022-03-29
;; in favor of `org-export-chinese-read'

(defvar chinese-reading-current nil)
(defvar chinese-reading-window nil)
(defun chinese-reading-filled (buffer-name)
  (interactive (list (buffer-name)))
  (pop-to-buffer "*chinese*")
  (read-only-mode -1)
  (display-line-numbers-mode -1)
  ;; (local-set-key "q" nil)
  ;; (read-only-mode 1)

  (with-current-buffer buffer-name
    (setq chinese-reading-current (list (point-min) nil)
          chinese-reading-window (selected-window)))

  (cl-flet ((goto-chapter (navigation-fn)
                          (erase-buffer)
                          (insert
                           (with-current-buffer buffer-name
                             (goto-char (point-min))
                             (call-interactively navigation-fn)
                             (setq chinese-reading-current (list (point) (org-get-heading)))
                             (org-narrow-to-subtree)
                             (prog1 (buffer-substring-no-properties (point-min) (point-max))
                               (widen))))
                          (ensure-chinese-fill-paragraph
                           (point-min)
                           (point-max))
                          (goto-char (point-min))))
    (reading-experience-on)
    (goto-chapter 'org-next-visible-heading)
    (local-set-key "p" (lambda () (interactive)
                         (goto-chapter 'org-previous-visible-heading)))
    (local-set-key "n" (lambda () (interactive)
                         (goto-chapter 'org-next-visible-heading)))))


(with-eval-after-load 'smartparens
  (define-keys smartparens-mode-map
    (kbd "M-r") #'sp-splice-sexp-killing-around
    (kbd "C-)") #'sp-forward-slurp-sexp
    (kbd "M-(") #'sp-wrap-round
    (kbd "M-D") #'sp-splice-sexp
    ;; barf/slurp
    ;; ("C-<right>" . sp-forward-slurp-sexp)
    (kbd "C-{") #'sp-backward-barf-sexp
    (kbd "C-}") #'sp-forward-barf-sexp
    ;; ("C-<left>" . sp-forward-barf-sexp)
    (kbd "C-(") #'sp-backward-slurp-sexp
    [backspace] #'sp-backward-delete-char
    [C-backspace] #'sp-backward-delete-word
    [M-backspace] #'sp-backward-kill-word
    [?\C-\M-w] #'sp-copy-sexp
    ;; free up 
    [?\C-\M-d] nil			; sp-down-sexp
    [?\C-\M-n] nil			; sp-up-sexp
    [?\C-\M-e] nil)
  (evil-define-key '(normal visual) smartparens-mode-map
    "y" (sexp-and-normal #'mark-sexp  #'evil-yank)
    "gr" (sexp-and-normal #'mark-sexp #'copy-and-paste)
    "gy" (sexp-and-normal #'mark-sexp #'comment-and-copy-line)
    "gc" (sexp-and-normal #'mark-sexp #'comment-or-uncomment-dwim)))

(defun my/remind (keybind func-name)
  "Remind to use KEYBIND to invoke this FUNC-NAME."
  (unless (key-binding keybind)
    (warn "keybind %s -> %s does not exist in global map" keybind func-name))
  (lambda ()
    (interactive)
    ;; TODO: maybe call func-name as well
    (message "use %s to invoke %s"
	     (propertize (key-description keybind) 'face 'help-key-binding)
	     func-name)))

(defun org-count-words (start end)
  "This is the count words version that skips comments.
It will operate between the region from START to END."
  (interactive "r")
  ;; "^[ \t]*#[+ ].*"
  ;; (count-matches (rx line-start (* space) "#" (any " " "+") (* any)))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (cl-loop with end = (point-max)
               for line-beg = (line-beginning-position)
               for line-end = (line-end-position)
               until (= line-end end)
               ;; unless (org-at-comment-p)
	       unless (looking-at "^[ \t]*#[+ ]")
               sum 1 into lines-count
               and sum (count-words line-beg line-end) into words-count
               and sum (- line-end line-beg) into chars-count
               do (goto-char (1+ line-end))
               finally (message "region has %d lines, %d words, %d characters"
                                lines-count words-count chars-count)))))


;; require in %H:%M form see format-time-string
(defvar theme/auto-day-night-switch nil
  "On mac, the switch sometimes is not successful.
this may be related to theme switching while the laptop is on
 sleep??")
;; timers
;; used to keep track of timers for cancellation on update.
(defvar theme/day-timer nil)
(defvar theme/night-timer nil)

(defun theme/setup-day-night-theme-timers ()
  "Initialize the day night timer.  And load the theme of time."
  (when theme/day-timer (cancel-timer theme/day-timer))
  (when theme/night-timer (cancel-timer theme/night-timer))
  (when theme/auto-day-night-switch
    (let ((one-day-secs (* 24 60 60)))
      (setq theme/day-timer
            (run-with-timer theme/day-time one-day-secs #'load-day-theme)
            theme/night-timer
            (run-with-timer theme/night-time one-day-secs #'load-night-theme)))))

(add-hook 'after-init-hook 'theme/setup-day-night-theme-timers)

;; 07-02-2022

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
