;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

;; prevent package.el loading packages prior to init-file loading.
(setq package-enable-at-startup t)

(setq default-frame-alist
      `((tool-bar-lines . 0) 			; (tool-bar-mode -1)
        (menu-bar-lines . 1)			; (menu-bar-mode 1)
		(horizontal-scroll-bar-mode . nil)
		(vertical-bar-mode . nil)
        (undecorated-round . t)			; Emacs 29+: disable title bar
		))


(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; @see https://www.reddit.com/r/emacs/comments/988paa/emacs_on_windows_seems_lagging/
;; speed up font rendering for special characters
(setq inhibit-compacting-font-caches t)

;; transparency setup
;; (set-frame-parameter (selected-frame) 'alpha '95)
;; (add-to-list 'default-frame-alist '(alpha . (85 . 50)))

(setq initial-scratch-message
	  (let ((fortune-prog (executable-find "fortune")))
		(if fortune-prog
			k
		  (format
			 ";; %s\n\n"
			 (replace-regexp-in-string
			  "\n" "\n;; "                  ; comment each line
			  (replace-regexp-in-string
			   "\\(\n$\\|\\|\\[m *\\|\\[[0-9][0-9]m *\\)" "" ; remove trailing linebreak
			   (shell-command-to-string
				(concat fortune-prog " ~/arch/fortunes/data")))))
		  (concat ";; Happy hacking "
				  (or user-login-name "")
				  " - Emacs loves you!\n\n"))))
(setq initial-buffer-choice
	  (lambda ()
		(dashboard-setup-startup-hook)
		(or (get-buffer dashboard-buffer-name)
			(get-buffer "*scratch*"))))

(provide 'early-init)
;;; early-init.el ends here
