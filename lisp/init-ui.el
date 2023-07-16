;;; init-ui.el --- Theme, modeline and window behavior -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

;;; Minibuffer

(use-package maple-minibuffer
  :disabled
  ;; unusable if `no-accept-focus' is non-nil, will prevent input
  ;; acceptance. And `no-accept-focus' is needed for showing initial
  ;; candidates
  :straight (maple-minibuffer :type git :host github
							  :repo "honmaple/emacs-maple-minibuffer"
							  :files ("*.el"))
  :config
  (defun maple-minibuffer:parameters ()
	"Maple minibuffer parameters."
	`((height . ,(or maple-minibuffer:height 10))
      (width . ,(or maple-minibuffer:width 1.0))
	  ;; (no-accept-focus . t)
      (left-fringe . 5)
      (right-fringe . 5)))

  (setq maple-minibuffer:position-type 'window-center
		maple-minibuffer:border-color "black"
		;; maple-minibuffer:height nil
		maple-minibuffer:width 0.8
		maple-minibuffer:cache t))

(use-package mini-frame :ensure t
  :config
  (setq mini-frame-ignore-commands
		'(eval-expression
		  edebug-eval-expression
		  debugger-eval-expression))
  (setq mini-frame-show-parameters
		'((top . 0.45)
		  (width . 0.85)
		  (vertical-scroll-bars . nil)
		  (no-accept-focus . t)			; show initial candidates
		  (left . 0.5))))

;;; tab-bar

(with-eval-after-load 'tab-bar
  (setq tab-bar-new-tab-choice
		(lambda () (find-file (project-prompt-project-dir)))
		tab-bar-tab-hints nil
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
		tab-bar-tab-name-format-function 'my/tab-bar-tab-name-format-default)

  (defun my/tab-bar-tab-name-format-default (tab i)
	"Customized `tab-bar-tab-name-format-default' with 35 char limit."
	(let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat (if tab-bar-tab-hints (format "%d " i) "")
               (let ((name (alist-get 'name tab)))
				 (if (> (length name) 35)
					 (concat (cl-subseq name 0 35) "...")
				   name))
               (or (and tab-bar-close-button-show
						(not (eq tab-bar-close-button-show
								 (if current-p 'non-selected 'selected)))
						tab-bar-close-button)
                   ""))
       'face (funcall tab-bar-tab-face-function tab)))))

(with-eval-after-load 'tab-line
  (setq tab-line-new-button-show nil
		tab-line-close-button-show nil))

;;; Window management

(setq switch-to-buffer-obey-display-actions t)

;; follow-mode, double window same buffer? like reading book
;; (windmove-install-defaults
;;  nil modifiers
;;  '((windmove-left left)
;;    (windmove-right right)
;;    (windmove-up up)
;;    (windmove-down down)))

;; Winum: move focus between sub-windows
(setq winum-keymap-quick-access-modifier "C")

(with-eval-after-load 'winum
  (setq winum-format " %s ")
  (setq winum-mode-line-position 0
        winum-auto-setup-mode-line nil)
  (set-face-attribute 'winum-face nil
					  :inherit '(mode-line-emphasis bold)
					  ;; :underline "DeepPink"
					  ;; :weight 'bold
					  ))

;; @see https://github.com/wasamasa/shackle
(use-package shackle :ensure t
  :defer t
  :init (add-hook 'emacs-startup-hook 'shackle-mode)
  :config
  (setq shackle-select-reused-windows nil ; default nil
		shackle-inhibit-window-quit-on-same-windows t
        shackle-default-alignment 'right  ; default below
        shackle-default-size 0.5
		shackle-default-rule '(:select nil))
  ;; :same (reuse window)
  ;; :popup (new window)
  ;; :other (use other window)
  ;; :select if non-nil select upon open
  ;; :inhibit-window-quit if non-nil do not close window
  ;; CONDITION(:regexp)
  (setq shackle-rules
        `(("*Shell Command Output*" :select nil)
          ("\\*Async Shell.*\\*" :regexp t :ignore t)
          ;; (racket-repl-mode :select nil)
          ;; (,(rx "*" (or "eww history" "Help") "*")
          ;;  :regexp t :select nil :inhibit-window-quit t)
		  ((elfeed-show-mode) :select t)
		  ;; Show on other buffer, quit do not close window
		  ((messages-buffer-mode eshell-mode Man-mode Woman-mode)
		   :other t :inhibit-window-quit t)
		  ;; Show on current buffer, quit do not close window
		  ((Info-mode) :same nil :inhibit-window-quit t)
		  ;;
		  ((magit-status-mode magit-log-mode)
		   :inhibit-window-quit t :same t)
		  ;; popup window
		  ((compilation-mode completion-list-mode
							 calendar-mode
							 occur-mode
							 flycheck-error-list-mode)
		   :size 0.3 :align below ;; :inhibit-window-quit nil
		   :select t)
          ;; (" \\*which-key\\*" :size 0.3 :align below)
          ("TAGS" :select t :other t))))

;;; Display line numbers

(defvar my/linum-inhibit-modes
  '(js-comint-mode
    profiler-report-mode
    dictionary-mode
    erc-mode
    fundamental-mode
    inferior-js-mode
    woman-mode
	man-mode
    Info-mode
    calendar-mode vundo-mode which-key-mode
    calc-mode calc-trail-mode
    comint-mode
    gnus-group-mode
    gud-mode
    vc-git-log-edit-mode
    log-edit-mode
    term-mode eshell-mode shell-mode vterm-mode
    eww-mode
    org-mode nov-mode doc-view-mode
	help-mode
	elfeed-show-mode
    gnus-summary-mode
    gnus-article-mode)
  "Major modes without line number.")

(add-hook 'after-init-hook #'global-display-line-numbers-mode)

(setq-default display-line-numbers t
			  display-line-numbers-type t)

(defun display-line-numbers-mode-hook-setup ()
  "Setup 'display-line-numbers-mode'.
Will disable line numbers if is temp buffer, or if `major-mode'
in `my/linum-inhibit-modes'."
  (when (not (derived-mode-p 'prog-mode))
	;; (memq major-mode my/linum-inhibit-modes)
	(setq-local display-line-numbers nil)))

(add-hook 'display-line-numbers-mode-hook #'display-line-numbers-mode-hook-setup)

(defun toggle-display-line-numbers-relative (arg)
  "Toggle relative line numbers or if ARG non-nil."
  (interactive "P")
  (if (or (eq display-line-numbers t) arg)
      (setq-local display-line-numbers 'relative
				  display-line-numbers-type 'relative)
    (setq-local display-line-numbers t
				display-line-numbers-type t)))

;;; Mode line

;; Emacs 29 uses a proportional font by default. Fix:
;; (set-face-attribute 'mode-line-active nil :inherit 'mode-line)
;; (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)

(defvar display-minor-mode-line-p nil)

(defun simple-modeline--buffer-status ()
  "Find buffer modified status."
  (cond ((and buffer-read-only (buffer-file-name))
		 '(read-only "禁" . error))
		((and buffer-file-name (buffer-modified-p))
         '(modified "写" warning bold))
		(t
		 '(normal "梁" . font-lock-type-face))))

(defun simple-modeline-segment-modified ()
  "Displays a color-coded buffer modification/read-only indicator in the mode-line."
  ;; (if (not (string-match-p "\\*.*\\*" (buffer-name))))
  (pcase-let* ((`(,status ,status-name . ,face)
				(simple-modeline--buffer-status)))
    (propertize
	 status-name
     'face `(;; :height 1.1
             :inherit ,face)
     'display '(raise 0.1)
     'help-echo (format
                 "Buffer is `%s'\nmouse-1: Toggle read-only status."
                 status)
     'local-map '(keymap
                  (mode-line
                   keymap
                   (mouse-1 . mode-line-toggle-read-only)))
     'mouse-face 'mode-line-highlight)))

(setq-default
 mode-line-format
 `("%e "								; error on full memory
   (:eval (winum-get-number-string)) " "
   ,(when (not (display-graphic-p))
	  "-")								; mode-line-front-space
   mode-line-mule-info " "				; IM, Coding, EOL description
   mode-line-client						; info on emacs client frame
   mode-line-remote						; remote buffer info
   mode-line-modified					; buffer modification info
   " "
   ;; ,(propertize
   ;; 	 "本"
   ;; 	 'face
   ;; 	 ;; (list :inherit :height 1.2)
   ;; 	 ;; 'display '(raise -0.1)
   ;; 	 ;; 'error
   ;; 	 '(mode-line-emphasis bold))
   ;; (:eval (simple-modeline-segment-modified)) " "
   mode-line-buffer-identification		; buffer name
   "  %4p (%c:%l) "						; customized `mode-line-position'
   ;; mode-line-frame-identification		; frame info
   "  "									; wide spacing
   (vc-mode vc-mode) " "

   ;; `mode-line-modes' without minor modes
   ((compilation-in-progress
	 #("[Compiling] " 0 12
	   (
		help-echo "Compiling; mouse-2: Goto Buffer"
		mouse-face mode-line-highlight
		local-map (keymap
				   (mode-line keymap
							  (mouse-2 . compilation-goto-in-progress-buffer))))))
	#("%[" 0 2
	  (help-echo "Recursive edit, type M-C-c to get out"))
	"("
	(:propertize
	 mode-name
	 face bold
	 help-echo "Major mode\nmouse-1: Show help for major mode"
	 mouse-face mode-line-highlight
	 local-map (keymap
				(mode-line keymap (mouse-1 . describe-mode))))
	mode-line-process
	;; (:propertize
	;; 	 minor-mode-alist
	;; 	 mouse-face mode-line-highlight help-echo "Minor mode
	;; mouse-1: Display minor mode menu
	;; mouse-2: Show help for minor mode
	;; mouse-3: Toggle minor modes" local-map
	;; 	 (keymap
	;; 	  ;; (header-line keymap
	;; 	  ;; 			   (down-mouse-3 . #4=(menu-item "Menu Bar" #3# :filter bindings--sort-menu-keymap)))
	;; 	  (mode-line keymap
	;; 				 ;; (down-mouse-3 . #4#)
	;; 				 (mouse-2 . mode-line-minor-mode-help)
	;; 				 (down-mouse-1 . mouse-minor-mode-menu))))
	#("%n" 0 2							; narrow
	  (
	   help-echo "mouse-2: Remove narrowing from buffer"
	   mouse-face mode-line-highlight
	   local-map (keymap
				  (mode-line keymap (mouse-2 . mode-line-widen)))))
	")"
	#("%]" 0 2							; recursive edit
	  (help-echo "Recursive edit, type M-C-c to get out")))
   ;; "%-" ;; fill with '-'
   ;; org-timer-set-timer in org-mode need this, %m
   mode-line-misc-info					; equal to %M, it seems
   ,(when (not (display-graphic-p))
	  'mode-line-end-spaces)))

;; simple-modeline
;; define some custom modeline segments

(defun simple-modeline-segment-nov-info ()
  "Simple-modeline-segment nov epub."
  (concat " "
          (propertize
           (or (cdr (assoc 'creator nov-metadata))
               "unknown")
           'face 'simple-modeline-unimportant)
          ;; The title is displayed in the `header-line-format' already
          ;; " "
          ;; (cdr (assoc 'title nov-metadata))
          (propertize
           (format " %d/%d"
                   (1+ nov-documents-index)
                   (length nov-documents))
           'face 'simple-modeline-status-info)))

(use-package simple-modeline
  :load-path "~/.emacs.d/site-lisp/simple-modeline/"
  :disabled
  :defer t
  :init
  (autoload 'simple-modeline-get-segments "simple-modeline")
  (let ((segments
		 (simple-modeline-get-segments
          `(("%e "
			 (winum-mode
			  (:propertize
			   (:eval (winum-get-number-string))
			   'face 'simple-modeline-important))
			 ;; winum
			 " "
			 ,(propertize "本"
						  'face 'simple-modeline-status-error
						  ;; 'simple-modeline-important
						  ;; (list :inherit :height 1.2)
						  ;; 'display '(raise -0.1)
						  )
			 ;; evil-modal
			 modified buffer-name mode-line-remote
			 position
			 mode-line-misc-info)
            (input-method " " vc " " major-mode eol encoding)))))
    (setq-default simple-modeline-segments segments))
  (setq simple-modeline-box-height 3
        ;; '(:height 130)
        simple-modeline-face-attributes nil)
  (simple-modeline-mode 1)
  (simple-modeline--update-modeline))

(use-package dashboard :ensure t
  :defer t
  :config
  ;; (dashboard-setup-startup-hook)
  (setq dashboard-center-content t
		dashboard-startup-banner 3 ;; using text banner to save space
		dashboard-projects-backend 'project-el
		dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          ;; (agenda . 5)
                          (registers . 5)))
  (defun dashboard-insert-footer ()
	"Insert footer of dashboard."
	(when-let ((footer
				(and dashboard-set-footer (dashboard-random-footer))))
      (insert "\n  ")
      ;; (dashboard-center-line footer)
      (insert dashboard-footer-icon)
      (insert " ")
      (insert
	   (propertize
		(replace-regexp-in-string "\n" "\n    " footer)
		'face 'dashboard-footer))
      (insert "\n")))

  ;; dashboard-footer-messages
  ;; TODO: increase fortune selection
  (when (executable-find "fortune.py")
	(setq dashboard-footer-messages
		  (split-string
		   (with-temp-buffer
			 (insert-file-contents "~/sources/git/Daocang/阴符经.md")
			 (buffer-string))
		   "\n\n"))))

;; (shell-command-to-string
;;  "fortune.py --name -S '\n\n' ~/sources/git/Daocang/*.md")

;;; Themes

(require-package 'doom-themes)
(require-package 'modus-themes)
(require-package 'ef-themes)

(setq ef-themes-mixed-fonts t)			; set before pkg load

(with-eval-after-load 'ef-themes
  ;; this is not working ??
  (defface my/ef-themes-org-heading-face
	'((t :family "Linux Biolinum"))
	"My custom face for org mode heading.")

  (setq ef-themes-headings
		;; This variable pitch does not scale, which works badly in NOV
		'((0 bold my/ef-themes-org-heading-face)
		  (1 bold my/ef-themes-org-heading-face 1.15)
		  (2 bold my/ef-themes-org-heading-face 1.1)
		  (3 bold my/ef-themes-org-heading-face)
		  (t regular my/ef-themes-org-heading-face)
		  )))

;; doom-one, doom-solarized-dark-high-contrast, doom-dracula
;; doom-gruvbox-light, doom-homage-white, solarized-light-high-contrast
;; using doom variant, because tab-bar support
(defvar theme/night 'ef-dark)
(defvar theme/day 'ef-day)

;; times

(defun my/faces-setup ()
  "My personal faces setup."
  ;; (when (x-list-fonts "Liberation Sans")
  ;;   (set-face-attribute 'fixed-pitch-serif nil :font "Liberation Sans"))
  ;; (set-face-attribute 'font-lock-doc-face nil :slant 'italic)
  (when (and (not (bound-and-true-p org-modern-global-mode))
             (facep 'org-done))
    (set-face-attribute 'org-done nil
                        ;; :underline nil
                        ;; :bold t
						))
  (when (bound-and-true-p simple-modeline-mode)
    (simple-modeline--update-modeline))
  ;; Make mode-line taller (thicker)
  (dolist (face '(mode-line mode-line-inactive))
	(set-face-attribute
	 face nil
	 :box `(:line-width 2 :color ,(face-background face)))))

(defun load-theme-only (theme)
  "Unload all other theme before loading `THEME'."
  (dolist (i custom-enabled-themes)
    (disable-theme i))
  (load-theme theme t)
  (my/faces-setup)
  ;; sometimes, switching to light theme, cause unwanted font switch
  )

(defun my/toggle-day/night ()
  "Toggle between day and night themes."
  (interactive)
  (if (memq theme/night custom-enabled-themes)
      (load-theme-only theme/day)
    (load-theme-only theme/night)))

(defvar theme/day-time "08:00")
(defvar theme/night-time "18:00")

(use-package emacs
  :defer 2
  :config
  (blink-cursor-mode -1)
  (show-paren-mode 1)
  (tab-bar-mode 1)
  (column-number-mode 1)

  (if after-init-time
	  (autoload 'winum-mode "winum")
	(local-require 'winum))
  (winum-mode 1)

  (setq show-paren-delay 0.05)

  ;; load day/night theme according to current time
  (cl-flet
	  ((time-from-string
        (hour-minute-str)
        (time-to-seconds
         (encode-time
		  (parse-time-string
           (format-time-string
			(concat "%+4Y-%m-%d " hour-minute-str)))))))
    (let ((current-time (time-from-string "%H:%M")))
      (if (and
		   ;; past day time
           (> current-time (time-from-string theme/day-time))
		   ;; before night
           (< current-time (time-from-string theme/night-time)))
          (load-theme-only theme/day)
        (load-theme-only theme/night)))))

(provide 'init-ui)
;;; init-ui.el ends here
