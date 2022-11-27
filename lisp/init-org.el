;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
;; some cool org tricks
;; @see http://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode

;; How to make flymake less angry
(eval-when-compile (require 'use-package))

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

(add-hook 'org-mode-hook 'org-mode-hook-setup)
(defun org-mode-hook-setup ()
  "My org mode setup."
  ;; set it to "" to fix nil in `org-comment-line-break-function'
  (setq-local fill-prefix "")
  (if (buffer-too-big-p)
      (progn
        (font-lock-mode -1)				; yes disable this
        (when (y-or-n-p "Turn on so long minor mode?")
          (so-long-minor-mode)))
    (org-appear-mode 1)
    ;; (org-superstar-mode 1)
    (org-modern-mode 1)
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
  (setq-local truncate-lines nil)
  (setq-local electric-pair-inhibit-predicate
              (lambda (char)
                (or
                 (electric-pair-default-inhibit char)
                 ;; prevent pairing of <, for org-tempo <s structure insertion
                 (char-equal char ?<)))))

(with-eval-after-load 'org
  ;; (add-to-list 'org-export-backends 'md) ; markdown export

  ;; org-startup-options
  (setq org-startup-with-latex-preview nil
        org-startup-indented t
        org-startup-folded 'show2levels)
  ;; org
  (setq org-tags-column -50
        org-scanner-tags '(noexport)

        org-outline-path-complete-in-steps t
        org-todo-keywords
        '((sequence "TODO(t@/!)" "NEXT(n@)" "STARTED(s@)" "HOLD(h@/!)" "|" "DONE(d@/!)")
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
        org-log-state-notes-into-drawer t
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

  (util:define-keys org-mode-map
	[C-return] 'my/org-insert
    [?_] 'my/sub-superscript
    [?^] 'my/sub-superscript

    [?\C-\M-u] 'org-up-element
    [?\C-\M-e] 'org-next-block
    [?\C-\M-n] 'org-next-visible-heading
    [?\C-\M-p] 'org-previous-visible-heading
    [?\C-\M-d] 'org-forward-heading-same-level

    [?\M-N] 'org-move-subtree-down
    [?\M-P] 'org-move-subtree-up

    [?\C->] 'org-shiftmetaright		; org-do-demote
    [?\C-<] 'org-shiftmetaleft		; org-do-promote
    [?\M-+] 'org-latex-pdf-count-words

    ;; [?\C-c ?o ?u] 'org-update-statistics-cookies
    [?\C-c ?o ?e] 'org-babel-execute-subtree)

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
                               (concat org-directory "daily.org")
                               (concat org-directory "refile.org")
                               (concat org-directory "projects.org")
                               ;; (concat org-directory "notes.org")
                               (concat org-directory "todo.org")))
  ;; latex fragments
  (setq org-highlight-latex-and-related '(native script entities))

  (when (eq system-type 'darwin)
    (if (executable-find "dvisvgm")
        (setq org-preview-latex-default-process 'dvisvgm)
      (warn "Install dvisvgm with tlmgr for better rendering on osx Emacs org latex fragment")))

  ;; org visuals
  (setq org-fontify-todo-headline nil
        org-fontify-done-headline nil
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line nil
        org-link-descriptive t
		org-hide-emphasis-markers t		; org-appear needs this
		org-hide-leading-stars nil
        org-pretty-entities t

        ;; enable mix emphasis English and nonascii (e.g. chinese)
        org-emphasis-regexp-components
        `(,(concat " \t('\"{"            "[:nonascii:]")
          ,(concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
          " \t\r\n,\"'"
          "."
          1))

  ;; org latex preview scale
  ;; (plist-put org-format-latex-options :background "Transparent")
  (setq org-format-latex-options
		(plist-put org-format-latex-options :scale 1.4))
  (setq org-image-actual-width t
		org-display-remote-inline-images 'cache)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (jupyter . t)
	 (haskell . t)
     (eshell . t)
     (shell . t)
     (lisp . t)
     (vterm-ob . t)
     (gdb . t)
     (calc . t)))

  ;; Add box around org-todo, to highlight different if background
  ;; theme too similar
  ;; (set-face-attribute 'org-todo nil :box `(:line-width 1))

  (require 'org-tempo)

  ;; deprecate in favor of ef-themes-heading
  ;; (custom-set-faces
  ;;  '(org-document-title ((t :height 1.2)))
  ;;  '(org-level-1 ((t :height 1.2 :inherit outline-1)))
  ;;  '(org-level-2 ((t :height 1.15 :inherit outline-2)))
  ;;  '(org-level-3 ((t :height 1.12 :inherit outline-3)))
  ;;  '(org-level-4 ((t :height 1.09 :inherit outline-4)))
  ;;  '(org-level-5 ((t :height 1.06 :inherit outline-5)))
  ;;  '(org-level-6 ((t :height 1.03 :inherit outline-6)))
  ;;  '(org-level-7 ((t :height 1.03 :inherit outline-7)))
  ;;  '(org-level-8 ((t :inherit outline-8))))

  (defun my/latex-auto-ref-link-export (path _desc backend channel)
    "Exporting link using autoref of PATH for latex BACKEND."
    (cond ((eq 'latex backend)
		   ;; (message "unknown channel var %s" (list (car channel)))
		   (format "\\autoref{%s}" path))
		  (t (message "unknown backend %s for path: %s" backend path))))

  ;; setup lst: and table: for \autoref
  (org-link-set-parameters
   "autoref"
   :export #'my/latex-auto-ref-link-export
   :follow #'org-link-search)

  (org-link-set-parameters
   "imghttps"
   :image-data-fun #'org-cache-https-image)

  ;; TODO: implement caching on chosen image,
  ;; find consistant image path ixdimg? same as latex?
  (defun org-cache-https-image ()
	(let ((buf
		   (or (url-retrieve-synchronously
				(concat (substring protocol 3) ":" link))
			   (error "Download of image \"%s\" failed" link))))
      (with-current-buffer buf
		(goto-char (point-min))
		(re-search-forward "\r?\n\r?\n")
		(buffer-substring-no-properties (point) (point-max))))))

(with-eval-after-load 'org-faces
  (setq org-cycle-level-faces nil))

(with-eval-after-load 'ob-core
  ;; fix jupyter ANSI color sequence
  ;; @see https://github.com/nnicandro/emacs-jupyter/issues/366
  (defun display-ansi-colors ()
    ;; ansi-color-compilation-filter
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'org-babel-after-execute-hook #'display-ansi-colors))

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
  (define-key org-src-mode-map "\C-c\C-c"'org-edit-src-exit)
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
  (setq org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t))

(with-eval-after-load 'org-refile
  ;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
  (setq org-refile-targets
		'(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
		  ("agenda.org" :regexp . "Past"))
        org-refile-use-outline-path 'file))

(with-eval-after-load 'ob-python
  (setq org-babel-python-command "python3"))

(require-package 'ob-async)
(use-package jupyter :ensure t :defer t)

(with-eval-after-load 'jupyter-org-extensions
  ;; disable bad key bind,
  ;; (define-key jupyter-org-interaction-mode-map [remap jupyter-org-hydra/body] 'ignore)
  (define-key jupyter-org-interaction-mode-map "\C-ch" 'nil))

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

(defun my/org-recapture-as-datetree-entry (beg end)
  (interactive (progn (unless (region-active-p)
						(org-mark-subtree))
					  (prog1 (list (region-beginning)
								   (region-end))
						(deactivate-mark))))
  ;; (org-heading-components)
  (save-match-data
	(save-excursion
	  (when (re-search-forward org-ts-regexp-both nil t)
		(org-element-timestamp-parser)
		(let* ((dct
				(decode-time (org-time-string-to-time (match-string 0))))
			   (date (list (nth 4 dct)	; month
						   (nth 3 dct)	; day
						   (nth 5 dct))))
		  ;; (setq date (list 1 1 (cl-third date)))
		  ;; (message "%s" date)
		  ;; (org-datetree-find-date-create d)
		  ;; (org-open-link-from-string
		  ;;  (format "[[*%s]]" (cl-third date)))
		  (let ((txt (delete-and-extract-region beg end)))
			;; org-find-olp
			(org-datetree-file-entry-under
			 txt
			 date)))))))

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
                    ((org-agenda-skip-function ;; 'org-agenda-skip-if-past-schedule
					  '(org-agenda-skip-entry-if
						'todo
						'("DONE" "PROJECT")))
                     ;; (org-deadline-warning-days 7)
					 (org-agenda-span 7)
					 (org-agenda-start-on-weekday 1)))
            (todo ""
                  ((org-agenda-overriding-header "Next")
                   (org-agenda-skip-function
					'(org-agenda-skip-entry-if 'timestamp
											   'nottodo
											   '("STARTED" "NEXT")))))
            (todo "PROJECT"
                  ((org-agenda-overriding-header "Projects")))
            (todo "TODO"
                  (;; (org-agenda-format-date "")
                   (org-agenda-overriding-header "Todo")
                   (org-agenda-prefix-format
                    "  %-8:c %-10(my/org-agenda-dates-ago 'short)")
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-skip-function ;; 'org-agenda-skip-if-not-activated
					'(org-agenda-skip-entry-if 'timestamp))
                   (org-agenda-sorting-strategy '(tsia-down ts-up time-down))))
            (todo "HOLD" ((org-agenda-overriding-header "Maybe")))))
          ("p" "Projects"
           ((todo "PROJECT" ((org-agenda-overriding-header "Projects")))
            (todo "HOLD" ((org-agenda-overriding-header "Maybe")))))
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
  (setq org-agenda-include-diary nil
        ;; {{ org 8.2.6 has some performance issue. Here is the workaround.
        ;; @see http://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
        org-agenda-inhibit-startup nil	   ; ~50x speedup
        org-agenda-use-tag-inheritance nil ; 3-4x speedup
        ;; }}
        org-agenda-tags-column 80
		org-agenda-window-setup 'current-window)
  (defun org-agenda-mode-setup ()
	(toggle-truncate-lines -1)
	(toggle-word-wrap 1)
	(visual-fill-column-mode 1)
	;; (visual-line-mode 1)
	)
  (add-hook 'org-agenda-mode-hook 'org-agenda-mode-setup))

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

(use-package org-modern :ensure t
  :commands (org-modern-mode)
  :config
  (set-face-attribute 'org-modern-block-name
					  nil
					  :weight 'bold
					  :box '(:line-width 1))
  (set-face-attribute 'org-modern-label nil :inherit '(fixed-pitch))
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka Curly")
  (setq org-modern-progress nil
		org-modern-footnote nil)		; its bad in table...
  ;; '(((underline t)
  ;; 	 (bold t))
  ;; 	(raise 0.3) (height 1.0))
  :init
  (setq org-modern-statistics nil
		org-modern-table nil
		org-modern-keyword nil		 ; "‣"
		org-modern-list '((43 . "•") (45 . "–") (42 . "◦"))
        org-modern-star '("✸") ;; '("◉" "○" "◇" ?◈)
		;; ""  ?⬘ ◉ ▣
		org-modern-hide-stars nil))

(use-package org-appear :ensure t
  :commands (org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autoentities nil
        org-appear-autolinks t))
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
		 (end-of-line)
		 (org-insert-item))
        ((not (= (point) (save-excursion
                           (org-up-heading-safe)
                           (point))))
         ;; (org-at-heading-or-item-p)
         (let ((org-insert-heading-respect-content nil))
           (org-insert-subheading nil)))
        ;; this happens when in a org file with no heading
        (:else (org-insert-heading-respect-content))))

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
  (and (null (org-entry-get nil "ACTIVATED"))
	   (save-excursion (org-end-of-subtree t))))

;; (util:define-hook-setup org-tab-first-hook :indent (org-indent-line))

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

(with-eval-after-load 'ox-odt
  (setq org-odt-preferred-output-format "docx"))

(with-eval-after-load 'ox-html
  (setq org-html-validation-link nil)
  (defun org-export-chinese-read ()
    "Export org file for better chinese reading in html."
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
		  "\\usepackage[utf8x]{inputenc}"
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

(use-package org-roam :ensure t
  :defer t
  :init
  (setq org-roam-v2-ack t) ;; acknowledge upgrade and remove warning at startup
  (global-set-key (kbd "C-c r i") 'org-roam-node-insert)
  (global-set-key (kbd "C-c r c") 'org-roam-node-find)
  (global-set-key (kbd "C-c r l") 'org-roam-buffer-toggle)
  :config
  (setq org-roam-directory (file-truename "~/sources/org/roam/"))
  (setq org-roam-db-location (concat org-roam-directory "org-roam.db"))
  (org-roam-setup))


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

(use-package ob-vterm-ob
  :init
  (defvar ob-vterm-default-instance 0
    "Use existing vterm instance when no session parameter specified.")

  (defun org-babel-vterm-initiate-session (&optional session _params)
    "Initiate a session named session according to params."
    (if-let* ((session
			   (if (and session
						(not (string= session "none")))
				   (concat "vterm-" session)
				 (or ob-vterm-default-instance
					 (user-error "Specify a session name for ob-vterm"))
				 (format "%s<%d>" vterm-buffer-name ob-vterm-default-instance)))
			  (buf (get-buffer session))
			  (alive (vterm-check-proc buf)))
		buf
      (let ((buf (generate-new-buffer session))
			(setup (cdr (assq :setup _params))))
		(save-window-excursion
		  (with-current-buffer buf
			(vterm-mode)
			(when-let ((plist
						(cdr (assoc setup ob-vterm-setup-alist))))
			  (vterm-send-string
			   (or (cl-getf plist :cmd)
				   (user-error "cmd is not defined for %s"
							   setup)))
			  (setq-local vterm-use-vterm-prompt-detection-method nil
						  term-prompt-regexp
						  (or (cl-getf plist :prompt-regexp)
							  (user-error "prompt regexp is not defined for %s"
										  setup))))
			buf)))))

  (defun org-babel-execute:vterm-ob (body params)
    (let ((session (org-babel-vterm-initiate-session
					(cdr (assq :session params))))
		  (full-body body))
      (org-babel-vterm-evaluate session full-body params)))

  (defvar ob-vterm-setup-alist
    '((gdb :cmd "lima\ngdb\n" :prompt-regexp "gef>")
      (termux :cmd "ssh android\n" :prompt-regexp "")))

  (defun ob-vterm-evaluate-string (cmd &optional results-param)
    ;; (vterm-send-C-p)
    ;; (vterm-send-C-n)
    ;; (vterm-send-C-a)
    (vterm-reset-cursor-point)
    (vterm-beginning-of-line)
    (vterm-send-C-k)
    (let ((start (point)))
      (vterm-send-string cmd)
      (vterm-send-string "\n")
      (if (string= results-param "none")
		  ""
		(while (not (= (point)
					   (save-excursion
						 (vterm-next-prompt 1)
						 (point)))
					;; (save-excursion
					;;   (goto-char start)
					;;   (not (re-search-forward
					;; 	  "ob-vterm-end"
					;; 	  nil t)))
					)
		  (accept-process-output vterm--process vterm-timer-delay))
		(let* ((end (save-excursion
					  (vterm-next-prompt 1)
					  (search-backward "\n" nil t))))
		  (buffer-substring
		   (save-excursion
			 (goto-char start)
			 (search-forward "\n" nil t))
		   end)))))

  (defun org-babel-vterm-evaluate (session body &optional params stdin cmdline)
    (let* ((capture-all (cdr (assq :capture params)))
		   (capture-all (member capture-all '("t" "yes")))
		   (results-param (cdr (assq :results params))))
      (save-match-data
		(with-current-buffer session
		  ;; this will goto and clear prompt
		  ;; (vterm-send-C-l)
		  (let ((results
				 (mapcar (lambda (cmd) (ob-vterm-evaluate-string cmd results-param))
						 (split-string body "\n"))))
			(if capture-all
				(mapconcat 'identity results "\n")
			  (car (last results 1))))))))
  (defalias 'vterm-ob-mode 'sh-mode)
  (provide 'ob-vterm-ob))

(use-package ob-gdb
  :init
  (defun org-babel-gdb-initiate-session (&optional session _params)
    "initiate a session named session according to params."
    (when (and session (not (string= session "none")))
      (save-window-excursion
	(or (org-babel-comint-buffer-livep (format "*gud-%s*" session))
	    (progn
	      ;; (shell session)
	      (gud-gdb (format "gdb --fullname %s" session))
	      (let ((process (get-buffer-process (current-buffer))))
		;; (comint-send-string process "gdb\n")
		(setq-local comint-prompt-regexp "gef➤  ")
		(while (save-excursion
			 (goto-char comint-last-input-end)
			 (not (re-search-forward
			       comint-prompt-regexp
			       nil t)))
		  (accept-process-output process)))
	      ;; needed for Emacs 23 since the marker is initially
	      ;; undefined and the filter functions try to use it without
	      ;; checking.
	      (set-marker comint-last-output-start (point))
	      (get-buffer (current-buffer)))))))

  (defun org-babel-execute:gdb (body params)
    (let ((session (org-babel-gdb-initiate-session
		    (cdr (assq :session params))))
	  (full-body
	   (org-babel-expand-body:generic
	    body params (org-babel-variable-assignments:shell params))))
      (org-babel-gdb-evaluate session full-body params)))

  (defun org-babel-gdb-evaluate (session body &optional params stdin cmdline)
    "Pass BODY to the Shell process in BUFFER.
If RESULT-TYPE equals `output' then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals `value' then
return the value of the last statement in BODY."
    (org-babel-comint-in-buffer session
      (let ((beg comint-last-input-start)
	    (output-start comint-last-input-end)
	    (ansi-color-for-comint-mode t) ; disable to preserve color sequence
	    (comint-prompt-regexp "gef➤  "))
	(end-of-buffer)			; goto comint prompt
	(comint-kill-input)		; clear all current input
	(dolist (line (split-string (org-trim body) "\n"))
	  (insert line)
	  (comint-send-input nil t)

	  (while (save-excursion
		   (goto-char comint-last-input-end)
		   (not (re-search-forward
			 comint-prompt-regexp
			 nil t)))
	    (accept-process-output
	     (get-buffer-process (current-buffer)))))
	(let ((inhibit-read-only t))
	  (prog1 (buffer-substring-no-properties
		  ;; delete-and-extract-region
		  output-start
		  (1- (marker-position (car comint-last-prompt))))
	    ;; (ansi-color-apply-on-region
	    ;;  beg (buffer-end 1))
	    )))))
  (provide 'ob-gdb))

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
  (util:define-keys org-present-mode-keymap
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
	      (tab-bar-mode -1)))
  (add-hook 'org-present-mode-quit-hook
            (defun my/org-present-exit-cleanup ()
              (org-present-small)
              ;; (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write)
	      (tab-bar-mode +1)
	      (display-line-numbers-mode +1))))

(provide 'init-org)
;;; init-org.el ends here
