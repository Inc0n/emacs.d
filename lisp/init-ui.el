;;; init-ui.el --- Theme, modeline and window behavior -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

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
  ;; (set-face-attribute 'winum-face nil
  ;;                     :foreground "DeepPink"
  ;;                     :underline "DeepPink"
  ;;                     :weight 'bold)
  (set-face-attribute 'winum-face nil :inherit '(mode-line-emphasis bold)))

;; @see https://github.com/wasamasa/shackle
(use-package shackle :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'shackle-mode)
  :config
  (setq shackle-select-reused-windows nil ; default nil
        shackle-default-alignment 'below  ; default below
        shackle-default-size 0.4)
  ;; :same if non-nil open in current window
  ;; :select if non-nil select upon open
  ;; :inhibit-window-quit if non-nil prevent window quiting on q

  ;; CONDITION(:regexp) :select :inhibit-window-quit :size+:align|:other :same|:popup
  (setq shackle-rules
        `((compilation-mode :select nil)
          ("*undo-tree*" :size 0.25 :align right)
          ("*eshell*" :select t :other t)
          ("*Shell Command Output*" :select nil)
          ("\\*Async Shell.*\\*" :regexp t :ignore t)
          (occur-mode :select nil :align t)
          ;; (,(rx "*" (or "eww history" "Help") "*")
          ;;  :regexp t :select nil :inhibit-window-quit t)
          ("*Completions*" :size 0.3 :align t)
          ("*Messages*" :select nil :inhibit-window-quit t :other t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :inhibit-window-quit t :other t)
          ("\\*poporg.*\\*" :regexp t :select t :other t)
          ("*Calendar*" :select t :size 0.3 :align below)
          ("*info*" :select t :inhibit-window-quit t :same nil)
          (magit-status-mode :select t :inhibit-window-quit t :same t)
          (magit-log-mode :select t :inhibit-window-quit t :same t)
          ("*Flycheck errors*" :select nil :size 0.3 :align below)
          ;; (" \\*which-key\\*" :size 0.3 :align below)
          ("TAGS" :select t :other t))))

;;; Display line numbers

(defvar my/linum-inhibit-modes
  '(js-comint-mode
    profiler-report-mode
    ffip-diff-mode
    dictionary-mode
    erc-mode
    fundamental-mode
    jabber-roster-mode
    jabber-chat-mode
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
    w3m-mode
    eww-mode
    nov-mode
    which-key-mode
    doc-view-mode
    speedbar-mode
    gnus-summary-mode
    gnus-article-mode
    calendar-mode)
  "Major modes without line number.")

(setq-default display-line-numbers t
	      display-line-numbers-type t)

(defun display-line-numbers-mode-hook-setup ()
  "Setup 'display-line-numbers-mode'.
Will disable line numbers if is temp buffer, or if `major-mode'
in `my/linum-inhibit-modes'."
  (when (or (memq major-mode my/linum-inhibit-modes)
            ;; (buffer-file-temp-p)
	    )
    (setq-local display-line-numbers nil)))

(add-hook 'display-line-numbers-mode-hook #'display-line-numbers-mode-hook-setup)
(add-hook 'after-init-hook #'global-display-line-numbers-mode)

(defun toggle-display-line-numbers-relative (arg)
  "Toggle relative line numbers or if ARG non-nil."
  (interactive "P")
  (if (or (eq display-line-numbers t) arg)
      (setq-local display-line-numbers 'relative
		  display-line-numbers-type 'relative)
    (setq-local display-line-numbers t
		display-line-numbers-type t)))

;;; Mode line

;; {{ 'built-in' date time in mode-line
(setq display-time-24hr-format t
      ;; @see `format-time-string' and `display-time-format' to customize time
      ;; format
      ;; display-time-format "%a %b %e"
      display-time-day-and-date t)
;; (add-hook 'after-init-hook 'display-time-mode)
;; }}

;; Emacs 29 uses a proportional font by default. Fix:
;; (set-face-attribute 'mode-line-active nil :inherit 'mode-line)
;; (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)

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
                   '(("%e " winum " " evil-modal modified buffer-name
                      mode-line-remote " " position)
                     (input-method " " vc " " major-mode eol encoding)))))
    (setq-default simple-modeline-segments segments))
  (setq simple-modeline-box-height 3
        ;; '(:height 130)
        simple-modeline-face-attributes nil)
  (simple-modeline-mode 1)
  (simple-modeline--update-modeline))
;; (add-hook 'after-init-hook
;;           (lambda () (run-with-idle-timer 1 nil ')))
(add-hook 'after-init-hook 'column-number-mode)
;; }}

;;; Themes

(require-package 'color-theme-modern)
(require-package 'doom-themes)
(require-package 'solarized-theme)
(require-package 'stimmung-themes)

;; doom-one, doom-solarized-dark-high-contrast, doom-dracula
;; doom-gruvbox-light, doom-homage-white, solarized-light-high-contrast
;; using doom variant, because tab-bar support
(defvar theme/night 'doom-dark+)
(defvar theme/day 'doom-gruvbox-light)

;; times
;; require in %H:%M form see format-time-string
(defvar theme/day-time "08:00")
(defvar theme/night-time "18:00")
(defvar theme/auto-day-night-switch nil
  "On mac, the switch sometimes is not successful.
this may be related to theme switching while the laptop is on
 sleep??")
;; timers
;; used to keep track of timers for cancellation on update.
(defvar theme/day-timer nil)
(defvar theme/night-timer nil)

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

(defun theme/setup-day-night-theme-timers ()
  "Initialize the day night timer.  And load the theme of time."
  (when theme/day-timer (cancel-timer theme/day-timer))
  (when theme/night-timer (cancel-timer theme/night-timer))
  (when theme/auto-day-night-switch
    (let ((one-day-secs (* 24 60 60)))
      (setq theme/day-timer
            (run-with-timer theme/day-time one-day-secs #'load-day-theme)
            theme/night-timer
            (run-with-timer theme/night-time one-day-secs #'load-night-theme))))
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

(add-hook 'after-init-hook 'theme/setup-day-night-theme-timers)


(defun my/theme-packages (packages &optional max)
  "Filter themes from PACKAGES."
  (--> ;; filter themes from all packages
   (--filter (string-match-p "-themes?$" (symbol-name (car it)))
             packages)
   ;; sort it by download count, i.e. (name . count)
   (cl-sort it #'> :key #'cdr)
   ;; list just top 100 themes
   (if (and max (fixnump max))
       (cl-subseq it 0 (min (length it) max))
     it)))

(defun my/get-popular-theme-name ()
  "Insert names of popular theme."
  (interactive)
  (with-current-buffer
      (url-retrieve-synchronously "http://melpa.org/download_counts.json" t t 30)
    (goto-char (point-min))
    (search-forward "{")
    (backward-char)                     ; move cursor just before the "{"
    (when-let ((pkgs (json-read)))      ; now read the json at point
      (--> (my/theme-packages pkgs 100) ; just top 100
           (mapcar (lambda (theme)
                     (format "%s %d" (car theme) (cdr theme)))
                   it)
           (completing-read "Top themes: " it)))))

(provide 'init-ui)
;;; init-ui.el ends here
