;;; init-misc --- miscellaneous -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; Miscellaneous configurations

;;; Code:

(require-package 'pos-tip)
(autoload 'popup-tip "popup")
(autoload 'scrap-interface "scrap")

;; Avoid potential lag:
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
;; `next-line' triggers the `format-mode-line' which triggers `projectile-project-name'
;; I use find-file-in-project instead of projectile. So I don't have this issue at all.
;; Set `auto-window-vscroll' to nil to avoid triggering `format-mode-line'.
(setq auto-window-vscroll nil)
;; (setq scroll-step 0
;;       scroll-conservatively 0)

;; {{ misc
(add-hook 'after-init-hook 'my/misc-setup)
(defun my/misc-setup ()
  (mouse-wheel-mode +1)
  ;; (global-hl-line-mode +1) ;; highlight current line
  (global-so-long-mode +1) ;; files, very very long line counter-measurement
  (delete-selection-mode -1)
  (savehist-mode 1)

  (recentf-mode +1)
  (global-subword-mode +1)

  (tab-bar-mode 1)
  (which-function-mode -1)

  ;; visual
  ;; (golden-ratio-mode +1)
  ;; (global-whitespace-mode +1)
  (blink-cursor-mode -1)
  (transient-mark-mode +1)              ; mark highlight
  (show-paren-mode +1))

;; (setq system-time-locale "C")
(with-eval-after-load 'imenu
  (setq imenu-max-item-length 60)
  (setq imenu-auto-rescan t))

(setq-default
 buffers-menu-max-size 30
 case-fold-search t
 ediff-split-window-function #'split-window-horizontally
 ediff-window-setup-function #'ediff-setup-windows-plain
 ;; grep-highlight-matches t
 ;; grep-scroll-output t
 set-mark-command-repeat-pop t
 tooltip-delay 0.7
 ;; void problems with crontabs, etc.
 ;; require-final-newline t ; bad idea, could accidentally edit others' code
 mouse-yank-at-point nil
 mouse-highlight nil
 truncate-lines nil
 truncate-partial-width-windows nil
 line-spacing 2)

(setq mouse-wheel-progressive-speed nil)
(setq show-paren-delay 0)

;; visible-bell has some issue
;; @see https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/issues/9#issuecomment-97848938
(setq visible-bell nil
      ring-bell-function 'ignore
      enable-recursive-minibuffers t)

;; trash configuration
(if (eq system-type 'darwin)
    (setq delete-by-moving-to-trash t
          trash-directory "~/.Trash/")
  (setq delete-by-moving-to-trash nil))

;; OSX: Fix frame small gaps for --with-no-titlebar
(setq frame-resize-pixelwise t)

;; Flash the mode-line setup
;; @see https://www.emacswiki.org/emacs/AlarmBell for more

(setq save-interprogram-paste-before-kill t ; kill-ring and clipboard are same?
      confirm-kill-emacs 'y-or-n-p
      history-delete-duplicates t
      ;; NO automatic new line when scrolling down at buffer bottom
      next-line-add-newlines nil)

(setq calc-symbolic-mode t
      calc-angle-mode 'rad)

(setq-default tab-width 4)
(setq tab-always-indent 'complete
      indent-tabs-mode nil)
;; }}

;; @see http://blog.binchen.org/posts/effective-code-navigation-for-web-development.html
;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(require-package 'avy)
(setq avy-style 'at-full)

;; midnight
;; this package purges buffers which haven't been displayed in configured period
(setq midnight-period (* 3600 12)) ;; 12 hours
(add-hook 'after-init-hook 'midnight-mode)
(add-hook 'midnight-hook
          (defun midnight-task ()
            (recentf-save-list)))

(define-advice indent-for-tab-command
    (:around (orig-func &optional arg) move-delim)
  "Move cursor out of any delimiters."
  (let ((tick (buffer-chars-modified-tick)))
    (funcall orig-func arg)
    (when (and (eq tick (buffer-chars-modified-tick))
               (or (not (bound-and-true-p evil-mode))
		   (evil-insert-state-p))
               (memq (char-syntax (following-char)) '(?\) ?\")))
	  (just-one-space 0)
	  (forward-char 1))))

;;;;
;; Font
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html
;;
(defun completing-read-fonts (font)
  "`completion-read' style FONT selection."
  (interactive
   (list (completing-read
          "Selector font: "
          (--filter (find-font (font-spec :name it))
                    '("DejaVu Sans Mono"
                      ;; "Bitstream Vera Sans Mono"
                      ;; "TerminessTTFNerdFont"
                      "Inconsolata"
                      "Liberation Mono"
                      "Fira Code"
                      "Source Code Pro"
                      "Hack"
                      "Iosevka"
                      "Jetbrains Mono"
                      "moneco"     ;; monaco with ligatures
                      "Menlo"      ; osx default
                      ;; "Amiri Typewriter"
                      ;; "Alegreya"
                      "Courier Prime"
                      "Anonymous Pro"
                      "PT Mono"
                      "Cascadia code")))))
  (set-face-attribute 'default nil :font font :weight 'normal :slant 'normal)
  (chinese/fix-font)
  (message "Font: %s" font))

(use-package ace-link :ensure t
  :defer t
  :config (ace-link-setup-default)
  :init (global-set-key [?\M-o] 'ace-link))

(use-package isearch
  :config
  (defun my/isearch-at-point-maybe (arg)
    (interactive "P")
    (call-interactively
     (if arg
	 'isearch-forward-thing-at-point
       'isearch-forward)))

  (defun isearch-within-defun-cleanup ()
    (widen)
    (remove-hook 'isearch-mode-end-hook 'isearch-within-defun-cleanup 'local))

  (defun isearch-within-defun ()
    (interactive)
    (narrow-to-defun)
    (add-hook 'isearch-mode-end-hook 'isearch-within-defun-cleanup 0 'local)
    (call-interactively 'my/isearch-at-point-maybe))

  (defvar isearch-transient-map
    (let ((map (make-sparse-keymap)))
      (define-key map "n" 'isearch-repeat-forward)
      (define-key map "N" 'isearch-repeat-backward)
      (define-key map "p" 'isearch-repeat-backward)
      map)
    "Used for as a transient for `my/isearch'.")

  (defun isearch-transient-interface (&optional additional-on-exit-fn)
    (set-transient-map
     isearch-transient-map
     t					; persistent transient map
     (lambda ()				; cleanup
       (lazy-highlight-cleanup 'force)
       ;; (message "state: %s" (cdr isearch-cmds))
       (when (cdr isearch-cmds)         ; avoid exiting twice
         (isearch-exit))
       (when (functionp additional-on-exit-fn)
	 (funcall additional-on-exit-fn)))))

  (defun my/isearch (no-regexp &optional arg)
    "I-search with transient map interface.
Non-nil INTERACTIVE-P means `isearch', otherwise it assumes
`isearch-string' is set and uses `isearch' as an interface.
TODO: Any prefix ARG will turn the search to occur."
    (interactive (list t current-prefix-arg))
    (isearch-forward-regexp no-regexp nil)
    (isearch-transient-interface))
  
  (define-keys isearch-mode-map
    ;; [backspace] 'undefined
    "\C-r" 'isearch-query-replace
    [?\C-\s-r] 'isearch-query-replace-regexp
    [?\M-q] (lambda () (interactive)
	      (visual-replace-regexp-text-at-point isearch-string))
    [escape] 'isearch-abort)
  :init
  (setq isearch-lazy-count t		; enable match numbers count
        isearch-lazy-highlight t
        lazy-count-prefix-format "%s/%s "
        lazy-highlight-cleanup t))

(use-package find-file-in-project :ensure t
  :defer t
  :config
  (defun my/search-git-reflog-code ()
    (let ((default-directory
            (my/git-root-dir)))
      (ffip-shell-command-to-string
       (format "git --no-pager reflog --date=short -S\"%s\" -p"
               (read-string "Regex: ")))))
  (push 'my/search-git-reflog-code ffip-diff-backends)
  (setq ffip-match-path-instead-of-filename t))

(with-eval-after-load 'bookmark
  ;; use my own bookmark if it exists
  (when (file-exists-p (file-truename "~/.emacs.bmk"))
    (setq bookmark-file (file-truename "~/.emacs.bmk"))))

(with-eval-after-load 'whitespace
  ;; (face-attribute)
  (custom-set-faces
   '(whitespace-tab ((((background light))
                      :background "#ebf1c9" :foreground unspecified)
                     (t
                      :background "#414141" :foreground unspecified)))
   '(whitespace-line ((((background light))
                       :background "#efead6" :foreground unspecified
                       :underline t :weight unspecified)
                      (t
                       :background "#414141" :foreground unspecified
                       :underline t :weight unspecified))))
  ;; (set-face-attribute '())
  (setq whitespace-style
        '(face                          ; visualize things below:
          space-before-tab              ; spaces before tab
          trailing                      ; trailing blanks
          tabs                          ; tabs (show by face)
          tab-mark                      ; tabs (show by symbol)
          indentation::tab
          empty                         ; empty lines at beginning/end of buffer
          lines-tail                    ; lines go beyond `fill-column'
          )))

(defun log-done ()
  "Log a TODO item to DONE with `org-time-stamp'."
  (interactive)
  (beginning-of-line)
  (when (search-forward "TODO" (line-end-position) 'no-error)
    (replace-match (format "DONE %s"
                           (with-temp-buffer
                             (org-insert-time-stamp nil)
                             (buffer-string))))))

(use-package dumb-jump :ensure t
  :defer t
  :init
  ;; enable the xref backend
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 90)
  ;; (remove-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; xref use completing-read to select a target
  ;; (setq xref-show-definitions-function #'selectrum-xref-quick-navigate)
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-default-project "../")
  (setq dumb-jump-rg-search-args "--no-require-git"))

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep))

(use-package hideshow :ensure nil
  :defer t
  :init
  (defun hs/display-code-line-counts (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (format "%s... %d lines"
                           (save-excursion
                             (goto-char (overlay-start ov))
                             (beginning-of-defun)
                             ;; (message "%s %s"
                             ;;          (overlay-start ov)
                             ;;          (line-beginning-position))
                             (if (checkdoc--next-docstring)
                                 (format "\n%s"
                                         (buffer-substring
		                          (line-beginning-position)
                                          (progn (forward-sexp 1) (point))))
                               ""))
                           (count-lines (overlay-start ov)
                                        (overlay-end ov))))))
  (setq hs-set-up-overlay #'hs/display-code-line-counts))

;;; temp lambda

(defvar my/temp-lambda nil
  "Variable holder for temporary lambda construct.")

(defun save-temp-lambda ()
  "Save Kmacro for Lisp code."
  (interactive)
  (let ((exp-under-cursor (sexp-at-point)))
    (if (null exp-under-cursor)
        (message "Not a valid sexp under cursor!")
      (setf my/temp-lambda exp-under-cursor)
      (message "new temp lambda saved. Call `run-temp-lambda' to run it.\n%S" exp-under-cursor))))

(defun run-temp-lambda ()
  "Run Kmacro for Lisp code."
  (interactive)
  (message "%S" (eval my/temp-lambda)))

;; turn on auto-fill-mode, don't use `text-mode-hook' because for some
;; mode (org-mode for example), this will make the exported document
;; ugly!
(add-hook 'change-log-mode-hook #'turn-on-auto-fill)
(add-hook 'cc-mode-hook #'turn-on-auto-fill)

;; {{ show email sent by `git send-email' in gnus
(with-eval-after-load 'gnus
  (when (local-require 'gnus-article-treat-patch)
    (setq gnus-article-patch-conditions
          '("^@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@"))))
;; }}


;; @see http://www.emacswiki.org/emacs/SavePlace
(use-package saveplace
  :defer 1
  :init (save-place-mode 1))

;; use this for emacsclient -n
(use-package server
  :defer 1
  :config (if (server-running-p)
              (message "server already started")
            (message "server started")
            (server-start)))

;;; auto-save, auto-save-visited - builtin emacs >= 26.1 package
(setq auto-save-timeout 3
      auto-save-interval 100           ; 100 characters interval
      auto-save-default nil
      auto-save-no-message t)
(add-hook 'after-init-hook 'auto-save-mode)

(setq auto-save-visited-interval 2) ;; in seconds
(add-hook 'after-init-hook 'auto-save-visited-mode)

;; (when (local-require 'auto-save)
;;   (add-to-list 'auto-save-exclude 'file-too-big-p t)
;;   (setq auto-save-idle 1) ; 1 seconds
;;   (setq auto-save-slient t)
;;   (add-hook 'after-init-hook 'auto-save-enable))

(with-eval-after-load 'autorevert
  (setq auto-revert-verbose t
	global-auto-revert-non-file-buffers nil))
(add-hook 'after-init-hook 'global-auto-revert-mode)

(use-package recentf
  :defer 1
  :config
  (setq recentf-keep '(recentf-keep-default-predicate)
        recentf-max-saved-items 512
        recentf-exclude
        '("/tmp/"
          ;; "/ssh:"
          "/sudo:"
          ;; "recentf$"
          ;; "company-statistics-cache\\.el$"
          ".emacs.d/elpa"
	  "share/emacs"                 ; make work also on Mac Emacs
          ;; ctags
          "/TAGS$"
          ;; global
          "/GTAGS$"
          "/GRAGS$"
          "/GPATH$"
          ;; binary
          "\\.mkv$"
          "\\.mp[34]$"
          "\\.avi$"
          "\\.wav$"
          "\\.docx?$"
          "\\.xlsx?$"
          ;; sub-titles
          "\\.sub$"
          "\\.srt$"
          "\\.ass$")))

;; {{ start dictionary lookup
;; use below commands to create dictionary
;; mkdir -p ~/.stardict/dic
;; check out for stardict dictionary resources
;; https://kdr2.com/resource/stardict.html
;;
(with-eval-after-load 'sdcv
  (evil-set-initial-state 'sdcv-mode 'motion)
  (setq sdcv-dictionary-simple-list
        '("Webster's Revised Unabridged Dictionary (1913)"
          "汉语大词典 离线版"))
  (setq sdcv-dictionary-complete-list sdcv-dictionary-simple-list))
;; }}

;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(add-hook 'minibuffer-setup-hook
          (defun my/minibuffer-setup-hook ()
            (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          (defun my/minibuffer-exit-hook ()
            (setq gc-cons-threshold my/normal-gc-cons-threshold)))

;; {{ emmet (auto-complete html tags)
;; @see https://github.com/rooney/zencoding for original tutorial
;; @see https://github.com/smihica/emmet for new tutorial
;; C-j or C-return to expand the line
(require-package 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'rjsx-mode-hook  'emmet-mode)
;; }}

(with-eval-after-load 'grep
  ;; eacl and other general grep (rgrep, grep ...) setup
  (dolist (v '("auto"
               "target"
               "node_modules"
               "bower_components"
               "*dist"
               ".sass_cache"
               ".cache"
               ".npm"
               "elpa"))
    (add-to-list 'grep-find-ignored-directories v))
  (dolist (v '("*.min.js"
               "*.map"
               "*.bundle.js"
               "*.min.css"
               "tags"
               "TAGS"
               "GTAGS"
               "GRTAGS"
               "GPATH"
               "cscope.files"
               "*.json"
               "*.log"))
    (add-to-list 'grep-find-ignored-files v))

  ;; wgrep and rgrep, inspired by http://oremacs.com/2015/01/27/my/refactoring-workflow/
  (define-key grep-mode-map
    (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)

  ;; display long lines in truncated style (end line with $)
  (add-hook 'grep-mode-hook (lambda () (setf truncate-lines nil))))

;; {{ https://www.emacswiki.org/emacs/EmacsSession better than "desktop.el" or "savehist".
;; Any global variable matching `session-globals-regexp' is saved automatically.
(use-package session :ensure t
  :defer t
  :config
  (setq session-save-file (my/emacs-d ".session"))
  (setq session-globals-max-size 512)
  (setq session-globals-max-string (* 4 1024)) ; can store 4Mb string

  (setq session-globals-include '((kill-ring 10)
                                  (session-file-alist 100 t)
                                  (file-name-history 200)
                                  search-ring regexp-search-ring))
  (setq session-save-file-coding-system 'utf-8))

(with-eval-after-load 'compile
  (setq compilation-scroll-output t)

  (define-keys compilation-mode-map
    "g" nil                             ; restore 'g' and 'h' keys
    "h" nil
    "r" 'recompile)                     ; rebind recompile to 'r'

  ;; ANSI-escape coloring in compilation-mode
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

  (add-to-list 'compilation-error-regexp-alist-alist
               '(mocha "at [^()]+ (\\([^:]+\\):\\([^:]+\\):\\([^:]+\\))" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'mocha))

;;

(defun my/insert-date (prefix)
  "Insert the current date.  With single PREFIX, use ISO format.
With two PREFIX arguments, write out the day and month name."
  (interactive "P")
  (insert
   (format-time-string
    (cond ((not prefix) "%m-%d-%Y")
          ((equal prefix '(4)) "%Y-%m-%d")
          ((equal prefix '(16)) "%d %B %Y")
          (t "%a %b %d %H:%M %Z %Y")))))

(defun ascii-table ()
  "Show the ascii table in buffer."
  (interactive)
  ;; (completing-read
  ;;  "Ascii characters: "
  ;;  (cl-loop for i from 1 to 255
  ;;           collect (cons (format "%S" (format "%c" i))
  ;;                         (format "%4d" i))
  ;;    ;; (insert (format "%4d %c\n" i i))
  ;;           ))
  (pop-to-buffer "*ASCII*")
  (read-only-mode -1)
  (erase-buffer)
  (insert "ASCII characters up to number 254.\n")
  (dotimes (i 255)
    (insert (format "%4d %S\n" i (format "%c" i))))
  (goto-char (point-min))
  (local-set-key "q" 'quit-window)
  (read-only-mode 1))

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Ctrl-X, u/l  to upper/lowercase regions without confirm
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package pomodoro :ensure t
  :defer t
  :config
  (push '(pomodoro-mode-line-string pomodoro-mode-line-string) mode-line-format)
  (setq pomodoro-play-sounds nil        ; *.wav is not installed
	pomodoro-break-time 2
	pomodoro-long-break-time 5
	pomodoro-work-time 15))

;; epub setup
(autoload 'nov-mode "nov")
(use-package nov :ensure nil
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width t)
  (evil-define-key 'motion nov-mode-map
    [tab] 'shr-next-link
    "q" 'ignore                         ; override quit window
    "R" 'nov-render-document            ; rebind from g to R
    ;; "gw" 'mybigword-pronounce-word
    ";" 'avy-goto-char-timer
    "n" 'nov-scroll-up
    "p" 'nov-scroll-down
    "d" nil)
  (evil-set-initial-state 'nov-mode 'motion)
  (add-hook 'nov-post-html-render-hook
            (defun nov-post-html-render-setup ()
              (call-interactively 'my/fill-buffer)))
  
  (define-hook-setup 'nov-mode-hook
    ;; (face-remap-add-relative 'variable-pitch
    ;;                          :family "Libreation Serif"
    ;;                          :width 'semi-expanded)
    (face-remap-add-relative 'variable-pitch :family "Baskerville")
    (face-remap-add-relative 'fixed-pitch-serif :family "Baskerville")
    (face-remap-add-relative 'variable-pitch-serif :family "Baskerville")
    (setq-local line-spacing 0.0)
    ;; TODO: find a way to center text by only left fringe
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width 70
                ;; Additional margin needed for Unicode text width
                visual-fill-column-extra-text-width '(0 . 5))
    ;; (visual-fill-column-mode 1)
    (read-room-mode 1)
    ;; 1.5 scale of current font height
    ;; (set-face-attribute 'default (selected-frame) :height (truncate (* 1.5 (face-attribute 'default :height))))
    (setq-local simple-modeline-segments
                `((simple-modeline-segment-winum
                   simple-modeline-segment-evil-modal
                   simple-modeline-segment-modified
                   simple-modeline-segment-nov-info)
                  (simple-modeline-segment-position
                   simple-modeline-segment-major-mode)))))

(use-package wgrep :ensure t
  :disabled
  :defer t
  :config
  (define-key grep-mode-map [?\C-c ?\C-c] 'wgrep-finish-edit)
  ;; save the change after wgrep finishes the job

  (setq wgrep-auto-save-buffer t)
  (setq wgrep-too-many-file-length 2024))
;; }}

(use-package which-key :ensure t
  :defer t
  :init
  (setq which-key-allow-imprecise-window-fit t ; performance
        which-key-idle-delay 0.5
        which-key-allow-evil-operators t
        which-key-show-operator-state-maps t
        which-key-max-description-length 25
        which-key-side-window-max-height 0.25
        which-key-frame-max-height 25
        which-key-min-display-lines 2)
  (add-hook 'after-init-hook 'which-key-mode))

(use-package ligature
  :disabled
  :defer t
  :commands (global-ligature-mode)
  :config
  ;; (setq ligature-composition-table nil)
  (add-to-list/s 'ligature-ignored-major-modes '(c-mode c++-mode))
  (ligature-set-ligatures 'text-mode
                          '("::" "->" "=>" "==" "===" "!="
			                "++" "<-" "/=" ">=" "<=" "..." "&&" "||" "//"))
  (ligature-set-ligatures 'prog-mode
                          '("::" ":::" "->" "=>" "==" "===" "!="
			                "++" "<-" "/=" ">=" "<=" ".."
			                "..." "&&" "||" "//"))
  :init (add-hook 'after-init-hook 'global-ligature-mode))

(use-package flycheck :ensure t
  :defer t
  :config
  ;; Region selection and background get mixed up
  (custom-set-faces
    '(flycheck-warning ((t (:background nil)))))
  ;; (set-face-attribute 'flycheck-warning nil :background nil)
  :init
  ;; (add-hook 'after-init-hook (lambda () (global-flycheck-mode 0)))
  ;; (when (require-package 'flycheck-color-mode-line)
  ;;   (setq flycheck-color-mode-line-face-to-color 'mode-line)
  ;;   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
  (setq flycheck-display-errors-function
        'flycheck-display-error-messages-unless-error-list))

(use-package expand-region :ensure t
  :defer t
  :config
  ;; press "v" to expand region
  ;; then press "c" to contract
  (setq expand-region-contract-fast-key "c")
  (setq expand-region-subword-enabled t))

(autoload 'golden-ratio-mode "golden-ratio")
(with-eval-after-load 'golden-ratio
  (setq golden-ratio-max-width 120
        golden-ratio-adjust-factor 1.0
        golden-ratio-auto-scale t
        golden-ratio-exclude-modes '(ediff-mode
                                     xref--xref-buffer-mode
                                     speedbar-mode)))

;; {{ project
(defun project-try-npm (dir)
  "My project-try for JavaScript (Nodejs) projects.
By locating package.json around DIR."
  (when-let ((root (and (memq major-mode '(js-mode js2-mode rjsx-mode))
                        (locate-dominating-file dir "package.json"))))
    (cons 'npm root)))

(cl-defmethod project-root ((project (head npm)))
  "Method of getting `project-root' for npm PROJECT."
  (cdr project))

(defun project-try-lisp (dir)
  "My project-try for CommonLisp (asdf) projects.
By locating package.json around DIR."
  (when-let ((root (and (memq major-mode '(lisp-mode))
                        (locate-dominating-file
                         dir
                         (lambda (dir)
                           (ignore-error (directory-files dir nil "asd")))))))
    (cons 'lisp root)))

(cl-defmethod project-root ((project (head lisp)))
  "Method of getting `project-root' for Lisp PROJECT."
  (cdr project))

(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'project-try-npm)
  (add-to-list 'project-find-functions 'project-try-lisp))
;; }}

;; {{ cache files
(use-package no-littering :ensure t
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backups/")))
        auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))
;; }}

(defun clean-backup-dir ()
  "Delete the files in the backup dir that are not in the list of `recentf-list'."
  (cl-labels ((aux (x)
		   (let ((x (subst-char-in-string ?! ?/ x)))
		     (substring (subst-char-in-string ?! ?/ x)
				0 (- (length x) 5)))))
    (mapcar (lambda (dir-pair)
	      (let* ((dir (cdr dir-pair))
		     (files-to-delete
		      (cl-set-difference
		       (mapcar #'aux (butlast (directory-files dir) 2))
		       recentf-list
		       :test 'string=)))
		(dolist (f files-to-delete)
		  (dolist (f (directory-files
			      dir
			      t
			      (concat "^" (file-name-nondirectory f) ".*")))
		    (delete-file f)))
		(cons (car dir-pair)
		      (length files-to-delete))))
	    backup-directory-alist)))

(use-package mini-frame :ensure t
  :config
  (setq mini-frame-show-parameters
	'((top . 0.4)
	  (width . 0.8)
	  (left . 0.5))))

;; undo highlight, similar to evil-goggles
(use-package undo-hl
  :defer t
  :straight (undo-hl :type git :host github :repo "casouri/undo-hl"))
 
(use-package elfeed :ensure t
  :disabled
  :defer t
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  (setq elfeed-feeds
        '(;; programming
          ("https://news.ycombinator.com/rss" hacker news)
          ("https://buttondown.email/hillelwayne/rss" programming)

          ;; reddit
          ("https://www.reddit.com/r/programming.rss" programming news)
          ("https://www.reddit.com/r/ReverseEngineering.rss" hacker news)
          ("https://www.reddit.com/r/philosophy/top.rss?t=week" philosophy news)
          ("https://www.reddit.com/r/emacs/top.rss?t=week" emacs news)
          ("https://www.reddit.com/r/lisp/top.rss?t=week" lisp news)

          ("https://www.hltv.org/rss/news" csgo news)
          ;; how to get youtube channel rss 
          ;; window["ytInitialData"].metadata.channelMetadataRenderer.rssUrl
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCTkXRDQl0luXxVQrRQvWS6w" dream youtube)
          ;; blog
          ("https://mstmetent.blogspot.com/feeds/posts/default?alt=rss" sbcl)
          ("https://asahilinux.org/blog/index.xml" asahilinux)))

  ;; Entries older than 2 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 weeks ago"
                                :remove 'unread))

  ;; https://emacstil.com/til/2021/10/31/bookmark-star-feeds-in-elfeed/
  ;; face for starred articles
  (defface elfeed-search-star-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")

  (add-to-list 'elfeed-search-face-alist '(star elfeed-search-star-title-face))

  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'star))

  (define-keys elfeed-search-mode-map
     "m" 'elfeed-toggle-star
     "u" (elfeed-expose #'elfeed-search-toggle-all 'unread)
     "U" 'elfeed-update-feed)

  (add-hook 'elfeed-search-mode-hook
            (defun elfeed-search-mode-setup ()
              (setq-local line-spacing 5)))
  :init (global-set-key [?\C-x ?w] 'elfeed))

(use-package newsticker
  :config
  (setq newsticker-retrieval-interval -1)
  (setq newsticker-groups
	'("Feeds" "asahilinux" "sbcl"
	  ("News" "hltv" "hackernews")
	  ("reddit"
	   "reddit lisp"
	   "reddit emacs"
	   "reddit philosophy"
	   "reddit reverse-eng"
	   "reddit programming")
	  "programming blog"
	  "Emacs Wiki")
	newsticker-url-list
	'(;; programming
          ("hackernews" "https://news.ycombinator.com/rss")
          ("programming blog" "https://buttondown.email/hillelwayne/rss")

          ;; reddit
          ("reddit programming" "https://www.reddit.com/r/programming.rss")
          ("reddit reverse-eng" "https://www.reddit.com/r/ReverseEngineering.rss")
          ("reddit philosophy" "https://www.reddit.com/r/philosophy/top.rss?t=week")
          ("reddit emacs" "https://www.reddit.com/r/emacs/top.rss?t=week")
          ("reddit lisp" "https://www.reddit.com/r/lisp/top.rss?t=week")

          ("hltv" "https://www.hltv.org/rss/news")
          ;; how to get youtube channel rss 
          ;; window["ytInitialData"].metadata.channelMetadataRenderer.rssUrl
          ;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCTkXRDQl0luXxVQrRQvWS6w" dream youtube)
          ;; blog
          ("sbcl" "https://mstmetent.blogspot.com/feeds/posts/default?alt=rss")
          ("asahilinux" "https://asahilinux.org/blog/index.xml"))))

(use-package eaf
  :defer t
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  ;; :straight (eaf :type git :host github
  ;; 		 :repo "emacs-eaf/emacs-application-framework"
  ;; 		 :files ("*.el" "*.py" "core" "app")
  ;; 		 ;; :pre-build (("python3" "install-eaf.py" "--install" "pdf-viewer" "--ignore-sys-deps"))
  ;; 		 )
  :custom
  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  ;; (browse-url-browser-function 'eaf-open-browser)
  :config
  (defalias 'browse-web #'eaf-open-browser)
  ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (use-package eaf-terminal :defer t
	:commands (eaf-open-terminal)
	:config
	(eaf-bind-key yank_text "C-y" eaf-terminal-keybinding))
  (use-package eaf-music-player :defer t :commands (eaf-open-music-player))
  (define-key eaf-mode-map* "q" 'quit-window)

  (with-eval-after-load 'evil
    (evil-set-initial-state 'eaf-mode 'emacs)))

(provide 'init-misc)
;;; init-misc.el ends here
