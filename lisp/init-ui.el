;;; init-ui.el --- Theme, modeline and window behavior -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

;;; tab-bar
(with-eval-after-load 'tab-bar
  (setq tab-bar-new-tab-choice (lambda () (call-interactively 'consult-buffer))
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-tab-hints t))

;;; Window management

;; move focus between sub-windows
(local-require 'winum)
(add-hook 'after-init-hook #'winum-mode)

(setq winum-keymap-quick-access-modifier "C")

(with-eval-after-load 'winum
  (setq winum-format " %s ")
  (setq winum-mode-line-position 0
        winum-auto-setup-mode-line nil)
  (set-face-attribute 'winum-face nil
		      :inherit '(mode-line-emphasis bold)
		      ;; :foreground "DeepPink"
		      ;; :underline "DeepPink"
		      ;; :weight 'bold
		      ))

;; @see https://github.com/wasamasa/shackle
(use-package shackle :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'shackle-mode)
  :config
  (setq shackle-select-reused-windows nil ; default nil
        shackle-default-alignment 'below  ; default below
        shackle-default-size 0.4
		shackle-default-rule '(:select t))
  ;; :same if non-nil open in current window
  ;; :select if non-nil select upon open
  ;; :inhibit-window-quit if non-nil prevent window quiting on q

  ;; CONDITION(:regexp) :select :inhibit-window-quit :size+:align|:other :same|:popup
  (setq shackle-rules
        `((compilation-mode :select nil :align below :size 0.3)
          ("*Shell Command Output*" :select nil)
          ("\\*Async Shell.*\\*" :regexp t :ignore t)
          (occur-mode :align t)
          ;; (,(rx "*" (or "eww history" "Help") "*")
          ;;  :regexp t :select nil :inhibit-window-quit t)
          ("*Completions*" :size 0.3 :align t)
          (eshell-mode :other t)
		  (messages-buffer-mode :other t :inhibit-window-quit t :frame t)
          ("\\*[Wo]*Man.*\\*" :regexp t :inhibit-window-quit t :other t)
          ("\\*poporg.*\\*" :regexp t :other t)
          ("*Calendar*" :size 0.3 :align below)
          (Info-mode :inhibit-window-quit t :same nil)
          ((magit-status-mode magit-log-mode) :inhibit-window-quit t :same t)
          ("*Flycheck errors*" :select nil :size 0.3 :align below)
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
    vundo-mode
    woman-mode
    Info-mode
    calc-mode
    calc-trail-mode
    comint-mode
    gnus-group-mode
    gud-mode
    vc-git-log-edit-mode
    log-edit-mode
    term-mode
    eshell-mode
    shell-mode
    vterm-mode
    eww-mode
    nov-mode
    which-key-mode
    doc-view-mode
    gnus-summary-mode
    gnus-article-mode
    calendar-mode)
  "Major modes without line number.")

(add-hook 'after-init-hook #'global-display-line-numbers-mode)

(setq-default display-line-numbers t
			  display-line-numbers-type t)

(defun display-line-numbers-mode-hook-setup ()
  "Setup 'display-line-numbers-mode'.
Will disable line numbers if is temp buffer, or if `major-mode'
in `my/linum-inhibit-modes'."
  (when (memq major-mode my/linum-inhibit-modes)
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

(add-hook 'after-init-hook 'column-number-mode)

(defvar display-minor-mode-line-p nil)

(setq-default
 mode-line-format
 '("%e" mode-line-front-space mode-line-mule-info mode-line-client
   ;; mode-line-modified
   ;; (:eval (winum-get-number-string))
   ;; " "
   ;; (:eval (simple-modeline-segment-modified))
   ;; (:eval (simple-modeline-segment-evil-modal))
   " "
   mode-line-buffer-identification
   mode-line-position

   mode-line-remote
   ;; mode-line-frame-identification
   " " (vc-mode vc-mode) "  "
   ;; mode-line-modes
   (:propertize "%m" 'face 'bold)
   (display-minor-mode-line-p
    minor-mode-alist)
   ;; (:eval (let ((sys (coding-system-plist buffer-file-coding-system)))
   ;;          (if (memq (plist-get sys :category)
   ;;                    '(coding-category-undecided coding-category-utf-8))
   ;;              "UTF-8"
   ;;            (upcase (symbol-name (plist-get sys :name))))))
   ;; global-mode-string,org-timer-set-timer in org-mode need this
   ;; "%M"
   ;; "%-" ;; fill with '-'
   mode-line-misc-info
   mode-line-end-spaces))

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

;; {{ simple-modeline setup
(with-eval-after-load 'evil
  (setq evil-no-display t
        evil-mode-line-format nil))

(use-package simple-modeline
  :load-path "~/.emacs.d/site-lisp/simple-modeline/"
  :init
  (autoload 'simple-modeline-get-segments "simple-modeline")
  (let ((segments (simple-modeline-get-segments
                   `(("%e " winum " "
					  ,(propertize "本"
								   'face 'simple-modeline-status-error
								   ;; 'simple-modeline-important
								   ;; (list :inherit :height 1.2)
								   ;; 'display '(raise -0.1)
								   )
					  ;; evil-modal
					  modified buffer-name mode-line-remote " " position)
                     (input-method " " vc " " major-mode eol encoding)))))
    (setq-default simple-modeline-segments segments))
  (setq simple-modeline-box-height 3
        ;; '(:height 130)
        simple-modeline-face-attributes nil)
  (simple-modeline-mode 1)
  (simple-modeline--update-modeline))
;; (add-hook 'after-init-hook
;;           (lambda () (run-with-idle-timer 1 nil ')))

;; }}

;;; Themes

(require-package 'color-theme-modern)
(require-package 'doom-themes)
(require-package 'solarized-theme)
(require-package 'modus-themes)

;; doom-one, doom-solarized-dark-high-contrast, doom-dracula
;; doom-gruvbox-light, doom-homage-white, solarized-light-high-contrast
;; using doom variant, because tab-bar support
(defvar theme/night 'modus-vivendi)
(defvar theme/day 'modus-operandi)

;; times

(defun my/faces-setup ()
  "My personal faces setup."
  ;; (when (x-list-fonts "Liberation Sans")
  ;;   (set-face-attribute 'fixed-pitch-serif nil :font "Liberation Sans"))
  ;; (set-face-attribute 'font-lock-doc-face nil :slant 'italic)
  (when (and (not (bound-and-true-p org-modern-global-mode))
             (facep 'org-done))
    (set-face-attribute 'org-done nil
                        :underline t
                        :bold t))
  (when (bound-and-true-p simple-modeline-mode)
    (simple-modeline--update-modeline)))

(defun load-theme-only (theme)
  "Unload all other theme before loading `THEME'."
  (dolist (i custom-enabled-themes)
    (disable-theme i))
  (load-theme theme t)
  (my/faces-setup)
  ;; sometimes, switching to light theme, cause unwanted font switch
  ;; however, this resizes the frame of emacs
  ;; (completing-read-fonts "Menlo")
  )

(defun my/toggle-day/night ()
  "Toggle between day and night themes."
  (interactive)
  (if (equal (car custom-enabled-themes) theme/night)
      (load-theme-only theme/day)
    (load-theme-only theme/night)))

(defvar theme/day-time "08:00")
(defvar theme/night-time "18:00")

(defun load-day-night-theme ()
  (cl-flet ((time-from-string
             (hour-minute-str)
             (time-to-seconds
              (encode-time
	       (parse-time-string
                (format-time-string (concat "%+4Y-%m-%d " hour-minute-str)))))))
    (let ((current-time (time-from-string "%H:%M")))
      (if (and
           (> current-time (time-from-string theme/day-time))	 ; past day time
           (< current-time (time-from-string theme/night-time))) ; before night
          (load-theme-only theme/day)
        (load-theme-only theme/night)))))

(add-hook 'after-init-hook 'load-day-night-theme)

(provide 'init-ui)
;;; init-ui.el ends here
