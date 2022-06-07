;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:
;; some cool org tricks
;; @see http://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode

;; Org clock

;; (with-eval-after-load 'org-clock
;;   ;; Change task state to STARTED when clocking in
;;   (setq org-clock-in-switch-to-state "STARTED")
;;   ;; Save clock data and notes in the LOGBOOK drawer
;;   (setq org-clock-into-drawer t)
;;   ;; Removes clocked tasks with 0:00 duration
;;   (setq org-clock-out-remove-zero-time-clocks t)

;;   ;; Show the clocked-in task - if any - in the header line
;;   (defun sanityinc/show-org-clock-in-header-line ()
;;     (setq-default header-line-format '((" " org-mode-line-string " "))))

;;   (defun sanityinc/hide-org-clock-from-header-line ()
;;     (setq-default header-line-format nil))

;;   (add-hook 'org-clock-in-hook #'sanityinc/show-org-clock-in-header-line)
;;   (add-hook 'org-clock-out-hook #'sanityinc/hide-org-clock-from-header-line)
;;   (add-hook 'org-clock-cancel-hook #'sanityinc/hide-org-clock-from-header-line)

;;   (define-key org-clock-mode-line-map [header-line mouse-2] #'org-clock-goto)
;;   (define-key org-clock-mode-line-map [header-line mouse-1] #'org-clock-menu))


(when nil
  (defvar my/langtool-check-time 2.0)
  (defvar my/langtool-last-modified-tick nil)
  (defvar my/langtool-timer nil)
  ;; (defun my/langtool flycheck-grammarly--reset-request () nil)
  (defun my/langtool--after-change-functions (&rest _)
    "After change function to check if content change."
    (message "pre-checking")
    (unless (eq my/langtool-last-modified-tick
		(buffer-modified-tick))
      (message "checking")
      (when my/langtool-timer
	(cancel-timer my/langtool-timer))
      (setq my/langtool-timer
            (run-with-timer my/langtool-check-time nil
                            'langtool-check-buffer))))
  (defun my/langtool-mode ()
    (interactive)
    (if (and flycheck-mode (derived-mode-p 'text-mode))
	(progn
	  (message "added langtool after change function")
	  (add-hook 'after-change-functions
		   #'my/langtool--after-change-functions nil t))
      (remove-hook 'after-change-functions
		   #'my/langtool--after-change-functions t))))

(use-package langtool :ensure t
  :init
  (setq langtool-bin "languagetool")
  ;; (add-hook 'flycheck-mode-hook #'my/langtool-setup)
  (use-package langtool-ignore-fonts :ensure t
    :disabled
    :config
    ;; ignore latex
    (langtool-ignore-fonts-add 'org-mode
			       '(;; font-lock-comment-face
				 font-latex-math-face
				 font-latex-string-face))))


(use-package flycheck-languagetool :ensure t
  :init
  (setq flycheck-languagetool-server-jar "/opt/homebrew/Cellar/languagetool/5.7/libexec/languagetool-server.jar")
  (setq flycheck-checker-error-threshold nil))

(require-package 'flycheck-grammarly)
;; (flycheck-grammarly-setup)
(with-eval-after-load 'flycheck
  (setq flycheck-grammarly--show-debug-message t)
  (setq flycheck-grammarly-check-time 0.8))

(add-hook 'org-mode-hook 'org-mode-hook-setup)
(defun org-mode-hook-setup ()
  "My org mode setup."
  ;; set it to "" to fix nil in `org-comment-line-break-function'
  (setq-local fill-prefix "")
  (if (buffer-too-big-p)
      (progn
        ;; yes disable this
        (font-lock-mode -1)
        (when (y-or-n-p "Turn on so long minor mode?")
          (turn-on-so-long-minor-mode)))
    (org-appear-mode 1)
    (org-superstar-mode 1)
    (org-modern-mode 1)
    (flycheck-languagetool-setup)
    (setq prettify-symbols-alist
          (append prettify-symbols-alist
                  `(("->" . ?→)
                    ("<-" . ?←)
                    ("[ ]" . ?☐)        ; checkbox
                    ("[-]" . ?❍)        ; pending
                    ("[X]" . ?☑)        ; checked box
                    )))
    ;; disabled prettify symbols
    ;; (prettify-symbols-mode 1)
    ;; (org-fragtog-mode 1)
    (setq my/flyspell-check-doublon nil))
  (when (bound-and-true-p evil-mode)
    (evil-normal-state 1))
  (setq-local truncate-lines nil)
  (setq-local electric-pair-inhibit-predicate
              (lambda (char)
                (or
                 (electric-pair-default-inhibit char)
                 ;; prevent pairing of <, for org-tempo <s structure insertion
                 (char-equal char ?<)))))

(with-eval-after-load 'org
  (add-to-list 'org-export-backends 'md) ; markdown export

  ;; org-startup-options
  (setq org-startup-with-latex-preview nil
        org-startup-indented t
        org-startup-folded t
	org-hide-leading-stars nil
        org-pretty-entities t)
  ;; org
  (setq org-tags-column -50
        org-scanner-tags '(noexport)

        org-outline-path-complete-in-steps t
        org-todo-keywords
        '((sequence "TODO(t/!)" "STARTED(s@)" "NEXT(n@)" "HOLD(h@/!)" "|" "DONE(d@/!)")
          (sequence "PROJECT(p@)" "|" "CANCELLED(c@/!)"))
        org-imenu-depth 5)

  ;; org behaviour
  (setq org-cycle-emulate-tab t
        org-log-done 'note
        org-edit-src-content-indentation 2
        org-edit-timestamp-down-means-later t
        org-fast-tag-selection-single-key 'expert
        org-catch-invisible-edits 'smart ; try not to accidently do weird stuff in invisible regions
        org-return-follows-link nil
        org-log-state-notes-into-drawer nil
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-use-sub-superscripts t
        org-insert-heading-respect-content t
        org-archive-location "%s_archive::datetree/")

  ;; org export
  (setq org-export-in-background nil
        org-export-with-sub-superscripts '{} ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}
        org-confirm-babel-evaluate nil
        org-export-use-babel t)

  (define-key org-mouse-map [mouse-2] nil) ; disable `org-open-at-mouse'

  (define-keys org-mode-map
    [?\C-\S-p] 'org-move-subtree-up
    [?\C-\S-n] 'org-move-subtree-down
    [C-return] 'my/org-insert
    [?_] 'my/sub-superscript
    [?^] 'my/sub-superscript)

  (defun my/sub-superscript (char)
    "Insert ^{} or _{}, when not in org-src-block."
    (interactive (list (event-basic-type last-command-event)))
    (if (or (org-in-src-block-p)
            (save-excursion
              (backward-char 1)
              (looking-at-p " ")))
        (insert char)
      (insert char)
      (insert "{}")
      (backward-char 1)))

  (setq org-directory "~/sources/org/agenda/")
  (setq org-agenda-files (list (concat org-directory "agenda.org")
                               ;; I dont use this file any more
                               ;; (concat org-directory "analysis.org")
                               (concat org-directory "daily.org")
                               (concat org-directory "refile.org")
                               (concat org-directory "projects.org")
                               (concat org-directory "notes.org")
                               (concat org-directory "todo.org")))
  ;; latex fragments
  (setq org-highlight-latex-and-related '(native script entities))

  (when (eq system-type 'darwin)
    (if (executable-find "dvisvgm")
        (setq org-preview-latex-default-process 'dvisvgm)
      (warn "Install dvisvgm with tlmgr for better rendering on osx Emacs org latex fragment")))
  
  ;; org visuals
  (setq org-fontify-todo-headline t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-link-descriptive t
        org-hide-emphasis-markers t

        ;; enable mix emphasis English and nonascii (e.g. chinese)
        org-emphasis-regexp-components
        `(,(concat " \t('\"{"            "[:nonascii:]")
          ,(concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
          " \t\r\n,\"'"
          "."
          1))

  ;; org latex preview scale
  ;; (plist-put org-format-latex-options :background "Transparent")
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))
  (setq org-image-actual-width t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (jupyter . t)
     (eshell . t)
     (shell . t)
     (lisp . t)
     (emacs-lisp . t)
     (julia . t)
     (calc . t)))

  ;; Add box around org-todo, to highlight different if background theme
  ;; too similar
  ;; (set-face-attribute 'org-todo nil :box `(:line-width 1))

  (defun my/enter-evil-insert (&rest args)
    (when (and (bound-and-true-p evil-mode)
               (evil-normal-state-p))
      (evil-insert-state 1)))
  (advice-add 'org-insert-structure-template :after 'my/enter-evil-insert)

  (require 'org-tempo)

  (custom-set-faces
   '(org-document-title ((t (:height 1.2))))
   '(org-level-1 ((t (:height 1.2 :inherit outline-1))))
   '(org-level-2 ((t (:height 1.15 :inherit outline-2))))
   '(org-level-3 ((t (:height 1.12 :inherit outline-3))))
   '(org-level-4 ((t (:height 1.09 :inherit outline-4))))
   '(org-level-5 ((t (:height 1.06 :inherit outline-5))))
   '(org-level-6 ((t (:height 1.03 :inherit outline-6))))
   '(org-level-7 ((t (:height 1.03 :inherit outline-7))))
   '(org-level-8 ((t (:inherit outline-8)))))

  (defun my/latex-auto-ref-link-export (path _desc backend channel)
    "Exporting link using autoref of PATH for latex BACKEND."
    (cond ((eq 'latex backend)
	   ;; (message "unknown channel var %s" (list (car channel)))
	   (format "\\autoref{%s}" path))
	  (t (message "unknown backend %s for path: %s" backend path))))

  ;; setup lst: and table: for \autoref
  (dolist (type '(;; "lst" "table" "fig"
		  "autoref"))
    (org-link-set-parameters type
			     :export 'my/latex-auto-ref-link-export
			     :follow 'org-link-search)))

(with-eval-after-load 'org-indent
  ;; change per level to 1 if not using org-bars
  (setq org-indent-indentation-per-level 1
        org-indent-mode-turns-on-hiding-stars nil))

(with-eval-after-load 'org-list
  (setq org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a."))
        ;; have a. A. a) A) list bullets
        org-list-allow-alphabetical t))

(with-eval-after-load 'org-table
  (setq org-table-formula-constants
        '(("c" . "299792458.")
          ("pi" . "3.14159265358979323846"))))

(with-eval-after-load 'org-src
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
  (setq org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t))

(with-eval-after-load 'org-refile 
  ;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
  (setq org-refile-targets '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
			     ("agenda.org" :regexp . "Past"))
        org-refile-use-outline-path 'file))

(with-eval-after-load 'ob-python
  (setq org-babel-python-command "python3"))

(require-package 'ob-async)
(require-package 'jupyter)

;; This allows org C-c C-c to use org-capture
;; (define-hook-setup org-ctrl-c-ctrl-c-final-hook :capture
;;   (org-capture)
;;   ;; return t to indicate C-c C-c actions has been taken by this function
;;   t)

(defun org-capture-prepare-org-capture ()
  "Indent org capture buffer for org capture."
  (when (eq major-mode 'org-mode)
    (org-insert-property-drawer)
    (org-set-property "ACTIVATED"
                      ;; (format-time-string "[%Y-%m-%d]")
                      (org-format-time-string
                       (org-time-stamp-format nil 'inactive)))
    ;; (org-insert-time-stamp (org-current-time) nil 'inactive)
    ;; (org-time-stamp-inactive '(16))
    (org-indent-region (point-min) (point-max))))

(defun my/org-capture-dated-entry ()
  "Mini version of `org-capture' in current buffer."
  (interactive)
  (let ((org-capture-templates
         '(("d" "Entry on date" entry (file+olp+datetree buffer-file-name)
            "\* %?\n" :time-prompt t :clock-resume t))))
    (org-capture nil "d")))

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-mode-hook 'org-capture-prepare-org-capture)
  ;; https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode
  (setq org-capture-templates
	`(("t" "Do on date" entry (file+olp+datetree "daily.org")
           "\* TODO %?\n%t\n%a\n" :time-prompt t :clock-resume t)
          ("T" "Todo" entry (file "todo.org") "* TODO %?")
          ("N" "Next" entry (file "todo.org") "* NEXT %?")
	  ("n" "Note" entry (file "notes.org") "* Note (%a)\n%?")
	  ;; ("n" "note" entry (file "note.org")
	  ;;  "* %? :NOTE:\n%U\n%a\n")
	  ;; ("d" "Deadline" entry (file+headline "agenda.org" "Deadline")
          ;; "* NEXT %?\nDEADLINE: %t")
          ("d" "Daily" entry (file+olp+datetree "daily.org") "* TODO %?\n%a\n")
          ("D" "Diary" entry (file+olp+datetree "diary.org") "* %?\n")
	  ;;  "* %?\n%U\n"
          ("a" "Analysis" entry (file "analysis.org")
	   "* TODO %? [%<%Y-%m-%d %a>]\n")
          ("e" "Event" entry (file+headline "agenda.org" "Future")
	   ,(concat "* %? :event:\n"
		    "SCHEDULED: <%<%Y-%m-%d %a %H:00>>")
           :time-prompt t)
          ;; ("r" "Respond" entry (file "agenda.org")
	  ;;  ,(concat "* NEXT Respond to %:from on %:subject\n"
	  ;;   		"SCHEDULED: %t\n"
	  ;;   		"%U\n"
	  ;;   		"%a\n"))
	  ("s" "Schedule" entry (file+headline "agenda.org" "Future")
	   "* %?\nSCHEDULED: %t"
           :time-prompt t)
          ("m" "Meeting" entry  (file+headline "agenda.org" "Future")
	   ,(concat "* %? :meeting:\n"
		    "<%<%Y-%m-%d %a %H:00>>"))
	  ("r" "Rant" entry  (file "notes.org")
	   "* %T %? :rant:\n" :jump-to-captured t)
	  ("p" "Project" entry (file+olp+datetree "daily.org")
           "* PROJECT %?\n"))))

(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        '(("n" "Agenda and all TODOs"
           ((tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "Completed today")))
            (agenda ""
                    ((org-agenda-skip-function 'org-agenda-skip-if-past-schedule)
                     (org-deadline-warning-days 7)))
            (todo "PROJECT"
                  ((org-agenda-overriding-header "Projects")))
            ;; (tags-todo "TODO")
            (todo ""
                  ((org-agenda-overriding-header "Next")
                   (org-agenda-skip-function 'org-agenda-skip-if-not-next)))
            (todo "TODO"
                  (;; (org-agenda-format-date "")
                   (org-agenda-overriding-header "Todo")
                   (org-agenda-prefix-format
                    "  %-8:c %-10(my/org-agenda-dates-ago 'short)")
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-skip-function 'org-agenda-skip-if-not-activated)
                   (org-agenda-sorting-strategy '(tsia-down ts-up time-down))))
            (todo "HOLD"    ((org-agenda-overriding-header "Maybe")))))
          ("p" "Projects"
           ((todo "PROJECT" ((org-agenda-overriding-header "Projects")))
            (todo "HOLD"    ((org-agenda-overriding-header "Maybe")))))
	  ("d" "Daily"
	   (;; (agenda "" ((org-agenda-files
            ;;              (concat org-directory "daily.org"))
            ;;             (org-agenda-skip-function
	    ;;              'org-agenda-skip-if-past-schedule)))
            (todo "NEXT"
                  ((org-agenda-files
                    `(,(concat org-directory "daily.org")))))
            (todo "TODO"
                  ((org-agenda-files
                    `(,(concat org-directory "daily.org")))))
            (todo "HOLD"
                  ((org-agenda-files
                    `(,(concat org-directory "daily.org")))))))
          ("b" "buffer summary"
	   ((agenda "" ((org-agenda-files (list buffer-file-name))))))))
  (setq org-agenda-start-on-weekday nil
        org-agenda-span 14
        ;; org-agenda-include-diary t
        ;; {{ org 8.2.6 has some performance issue. Here is the workaround.
        ;; @see http://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
        org-agenda-inhibit-startup t       ;; ~50x speedup
        org-agenda-use-tag-inheritance nil ;; 3-4x speedup
        ;; }}
        org-agenda-tags-column 80
	org-agenda-window-setup 'current-window))

(defun my/org-agenda-dates-ago (&optional short-representation)
  "Produce a time ago reference for activated TODOs."
  (if-let ((activation (org-entry-get nil "ACTIVATED"))
           (days-ago (abs (org-time-stamp-to-now
                           (org-entry-get nil "ACTIVATED")))))
      (cl-multiple-value-bind (divider date-suffix)
          (cond ((> days-ago 365) (cl-values 365 "year"))
                ((> days-ago 30)  (cl-values 30 "month"))
                (:else            (cl-values 1 "day")))
        (let ((more-p
               (> (% days-ago divider) 0)))
          (format "%s%2d%s ago"
                  (if more-p ">" "<") ;; return single space to keep them aligned
                  (/ days-ago divider)
                  (if short-representation
                      (substring date-suffix 0 1)
                    (concat " " date-suffix
                            (and (> (/ days-ago divider) 1)
                                 "s"))))))
    "n/a"))

(use-package org-remark
  :disabled
  :straight (org-remark :type git :host github :repo "nobiot/org-remark"))

(use-package org-modern :ensure t
  :defer t
  :init
  (setq org-modern-keyword "‣"
        ;; let org-superstar stylize the stars
	org-modern-list '((43 . "•") (45 . "–") (42 . "◦"))
        org-modern-star nil
        org-modern-hide-stars nil))

(use-package org-appear :ensure t
  :defer t
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autoentities nil
        org-appear-autolinks t))

(use-package org-superstar :ensure t
  :defer t
  :config
  (set-face-attribute 'org-superstar-leading nil :foreground "dark gray")
  :init
  (setq-default org-superstar-prettify-item-bullets nil
                ;; '(?◉ ?◈ ?✸ ?▣)
                org-superstar-headline-bullets-list '(?✸) ;⬘▣
                org-superstar-item-bullet-alist '((?+ . ?•) (?- . ?–))
                ;; org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?➤) (?- . ?ⅰ))
                ;; '((?* . ?▶) (?+ . ?⬘) (?- . ?⬙))
                org-superstar-leading-bullet "."
                org-superstar-todo-bullet-alist '(("TODO" . ?○)
                                                  ("DONE" . ?●)
                                                  ;; org-bars over this
                                                  ("CANCELLED" . ?☒)
                                                  ("NEXT" . ?✪)
                                                  ;; ("PROJECT" . ?Π)
                                                  ("HOLD" . ?⊘)))
  (setq org-superstar-cycle-headline-bullets nil
        org-superstar-special-todo-items t)
  ;; fix jupyter ANSI color sequence 
  ;; @see https://github.com/nnicandro/emacs-jupyter/issues/366
  (defun display-ansi-colors ()
    ;; ansi-color-compilation-filter
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'org-babel-after-execute-hook #'display-ansi-colors))

;; ☐☑⦷

(defun my/org-insert (&optional arg)
  "Insert item or heading depending on context.
- Always insert heading when ARG is non-nil.
- Insert an item below if point not on bullet.
- Insert a same level heading, if at start of one.
- Insert a subheading if within the paragraphs of a heading."
  (interactive "P")
  (cond ((or (org-at-heading-p) arg)
         (org-insert-heading-respect-content))
        ((org-at-item-p)
         ;; (if (= (line-beginning-position) (point)))
	 (if (bound-and-true-p evil-mode)
	     (evil-move-end-of-line)
           (end-of-line))
	 (org-insert-item))
        ((not (= (point) (save-excursion
                           (org-up-heading-safe)
                           (point))))
         ;; (org-at-heading-or-item-p)
         (let ((org-insert-heading-respect-content nil))
           (org-insert-subheading nil)))
        ;; this happens when in a org file with no heading
        (:else (org-insert-heading-respect-content))))

(defun org-agenda-skip-if-not-next ()
  "If this function return nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (message "%s" (org-get-todo-state))
  (if (or (member (org-get-todo-state) '("STARTED" "NEXT")))
      ;; return nil mean should not skip
      nil
    (save-excursion (org-end-of-subtree t))))

(defun org-agenda-skip-if-past-schedule ()
  "Returns nil, to indicate the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued.
This function filter out heading that doesn't have a schedule entry.
Or its TODO state is not \"NEXT\"."
  (when-let ((subtree-end (save-excursion (org-end-of-subtree t)))
	     (schedule (org-entry-get nil "SCHEDULED"))
             (now (time-to-seconds (current-time))))
    (when-let ((scheduled-seconds
		(time-to-seconds
		 (org-time-string-to-time schedule))))
      (and (not (string= (org-get-todo-state) "NEXT")) ;; never skip todo NEXT state
           (< scheduled-seconds now)
           subtree-end))))

(defun org-agenda-skip-if-not-activated ()
  "Skip all entries if it does not have the property ACTIVATED."
  (if-let ((subtree-end (save-excursion (org-end-of-subtree t)))
	   (activated (org-entry-get nil "ACTIVATED")))
      (if activated
          nil                           ; return nil if should not skip
        subtree-end)
    subtree-end))

;; (define-hook-setup org-tab-first-hook :indent (org-indent-line))

(defun org-goto-visible-element (arg)
  "Move cursor to the previous visible item or heading.
ARG will repeat the operation ARG number of times.
FORWARD will go forward unless nil"
  (if (org-in-item-p)
      (dotimes (i (if (fixnump arg)
                      (abs arg)
                    1))
        (org-next-item))
    (org-next-visible-heading arg)))

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

(defun org-latex-pdf-count-words ()
  "This is the count words version that skips comments.
It will operate between the region from START to END."
  (interactive)
  (when (y-or-n-p "Latex export current buffer to pdf first? ")
    (org-latex-export-to-pdf))
  (message "Word Count Estimate: \n\tLines\tWords\tCharacters\n%s"
	   (s-trim-right
	    (shell-command-to-string
	     (format "pdftotext %s /dev/stdout | wc"
		     (org-export-output-file-name ".pdf"))))))

(with-eval-after-load 'ox-html
  (setq org-html-validation-link nil)
  (defun org-export-chinese-read ()
    (interactive)
    (let ((org-html-head-include-default-style nil)
          (org-html-head (format
                          "<link rel='stylesheet' type='text/css' href=%S/>"
                          (my/emacs-d "site-lisp/org-html.css")))
          ;; (org-html-preamble-format
          ;;  '(("en" "<p class=\"author\">Author: %a</p>")))
          (org-html-postamble nil)
          ;; see `org-latex-export-dictionary'
          (org-export-default-language "zh-CN")
          (file (file-truename "~/sources/org/tmp.html")))
      (org-export-to-file 'html file)
      (browse-url file))))

(with-eval-after-load 'ox-publish
  (setq org-publish-project-alist
        '(("github website"
           :exclude "setup.org"
           ;; :style "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />"
           ;; :recursive t
           :base-directory "~/sources/git/Inc0n.github.io/src"
           :base-extension "org"
           :publishing-directory "~/sources/git/Inc0n.github.io/"
           :publishing-function org-html-publish-to-html
           :headline-levels 3))))

(autoload 'org-insert-dblock:org-gantt-chart "org-gantt")
(autoload 'org-dblock-write:org-gantt-chart "org-gantt")

;;; org latex
(with-eval-after-load 'ox-latex
  ;;
  (setq org-latex-image-default-width ".5\\textwidth"
	org-latex-default-quote-environment "quote"
	org-latex-subtitle-separate t
	org-latex-subtitle-separate nil)

  (add-to-list/s 'org-latex-listings-langs
                 '((text "Text")
                   (javascript "Javascript")
				   (jupyter-python "Python")
                   (asm "Assembler")
                   (calc "Python")
				   (yaml "yaml")))
  
  (defun my-latex-export-example-blocks (text backend info)
    "Export example blocks as custom results env."
    (when (org-export-derived-backend-p backend 'latex)
      (with-temp-buffer
        (insert text)
        ;; replace verbatim env by 'lstlisting'
        (goto-char (point-min))
        (while (re-search-forward "\\\\\\(begin\\|end\\){verbatim}" nil t)
          (replace-match "\\\\\\1{lstlisting}"))
        (buffer-substring-no-properties (point-min) (point-max)))))

  (add-to-list 'org-export-filter-example-block-functions 'my-latex-export-example-blocks)
  ;; (setq org-export-filter-example-block-functions nil)

  (setq org-latex-minted-options '(("breaklines" "true")
                                   ("breakanywhere" "true")
								   ("frame" "single")))
  (setq org-latex-caption-above '(table src-block)
        ;; Our hack for using auto ref to generate our nice labels
        ;; org-latex-image-default-scale
        org-latex-listings t
        org-latex-listings-options '(("breaklines" "true")
                                     ("basicstyle" "\\small\\ttfamily")
                                     ("frame" "single"))
        org-latex-prefer-user-labels t)

  ;; Support Unicode latex export
  (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))

  ;; Export org-mode in Chinese into PDF
  (setq org-latex-compiler "xelatex")
  (setq org-latex-pdf-process
        '("%latex -interaction nonstopmode -output-directory %o %f"
          "%bib %b"
          "%latex -interaction nonstopmode -output-directory %o %f"
          "%latex -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-packages-alist
        '(("a4paper, margin=1.1in" "geometry" nil)
          "\\usepackage{amsmath, amssymb}"
          ;; "\\usepackage[utf8]{inputenc}"
          ("" "fontspec" nil)
          ("" "xeCJK" nil)
          ;; TODO: fix this
          ;; "\\setCJKmainfont{AaFangSong (Non-Commercial Use)}"
          ("" "listings")
          ("" "xcolor")
          ("" "parskip" nil)
          ("" "float" nil))))

;; kmacro for converting markdown link to an org link
(fset 'markdown-link-to-org-link
      (kmacro-lambda-form
       [?v ?% ?S ?\] ?l ?% ?l ?l ?l ?v ?h ?% ?h ?x ?h ?h ?% ?a ?\[ ?\] ?\C-b escape ?p ?l ?l ?% ?l ?l ?x ?x]
       0 "%d"))

(use-package org-ql :ensure t
  :config
  ;; (completing-read "" (lambda ()))
  (defun my/org-ql-search ()
    "`org-ql-search' my personal org files directory."
    (interactive)
    (org-ql-search
      (org-ql-search-directories-files
       :directories '("~/sources/org")
       :recurse t)
      (read-string "Query: " (when org-ql-view-query
                               (format "%S" org-ql-view-query))))))

(use-package org-roam :ensure t
  :after org
  :init
  (setq org-roam-v2-ack t) ;; acknowledge upgrade and remove warning at startup
  :config
  (setq org-roam-directory (file-truename "~/sources/org/roam/"))
  (setq org-roam-db-location (concat org-roam-directory "org-roam.db"))
  (org-roam-setup)
  (global-set-key (kbd "C-c r i") 'org-roam-node-insert)
  (global-set-key (kbd "C-c r c") 'org-roam-node-find)
  (global-set-key (kbd "C-c r l") 'org-roam-buffer-toggle))


(with-eval-after-load 'oc
  (defun my/latex-cite-insert (context arg)
    (let* ((ref (car (citar-select-refs :rebuild-cache arg))))
      ;; (message "%s" ref)
      (insert (format "\\cite{%s}" (car ref)))))

  (org-cite-register-processor 'my/latex
    :insert
    ;; (org-cite-make-insert-processor
    ;;  #'citar-org-select-key
    ;;  #'citar-org-select-style)
    #'my/latex-cite-insert)
  (setq org-cite-insert-processor 'my/latex))

(use-package citar :ensure t
  :after org-cite
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

;; (use-package ob-rescript
;;   :init
;;   (defun org-babel-execute:rescript (body params)
;;     "Execute a block of rescript code with org-babel."
;;     (let ((in-file (org-babel-temp-file "rescript" ".res"))
;;           (verbose (or (cdr (assq :verbose params)) 0)))
;;       (with-temp-file in-file
;;         (insert body))
;;       ;; TODO: separate out bsc and node
;;       (org-babel-eval
;;        (format "bsc %s | node"
;;                ;; (when verbose "-v")
;;                (org-babel-process-file-name in-file))
;;        "")))
;;   (provide 'ob-rescript))

(require-package 'org-present)
(with-eval-after-load 'org-present
  (define-keys org-present-mode-keymap 
    [right] #'org-present-next
    [left]  #'org-present-prev
    "q" #'org-present-quit)

  (setq org-present-text-scale 4)

  (add-hook 'org-present-mode-hook
            (defun my/org-present-setup ()
              (org-present-big)
              (org-display-inline-images)
              ;; (org-present-hide-cursor)
              (org-present-read-only)
	      (display-line-numbers-mode -1)
	      (tab-bar-mode -1)
	      (evil-emacs-state)))
  (add-hook 'org-present-mode-quit-hook
            (defun my/org-present-exit-cleanup ()
              (org-present-small)
              ;; (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write)
	      (tab-bar-mode +1)
	      (display-line-numbers-mode +1)
	      (evil-normal-state))))

(provide 'init-org)
;;; init-org.el ends here
