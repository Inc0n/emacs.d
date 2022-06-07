;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

;; prevent package.el loading packages prior to init-file loading.
(setq package-enable-at-startup t)

(setq default-frame-alist
      `((tool-bar-lines . 0)
        (menu-bar-lines . ,(if (display-graphic-p) 1 0))
        ,(or (and (find-font (font-spec :name "Fira Code"))
		  '(font . "Fira Code"))
	     '(font . "Menlo"))))

(set-face-attribute 'default t :height 130)

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; @see https://www.reddit.com/r/emacs/comments/988paa/emacs_on_windows_seems_lagging/
;; speed up font rendering for special characters
(setq inhibit-compacting-font-caches t)

;; Show a marker in the left fringe for lines not in the buffer
(setq indicate-unused-lines nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(menu-bar-mode 1)

;; transparency setup
;; (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 50)))

(defun show-scratch-buffer-message ()
  "Construct the scratch buffer message."
  (let ((fortune-prog (executable-find "fortune")))
    (if fortune-prog
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

(setq initial-scratch-message (show-scratch-buffer-message))
(setq initial-buffer-choice "~/sources/org/chinese/学.org")
;; (lambda ()
;;   (let ((org-agenda-window-setup
;; 		 'Only-window))
;; 	(org-agenda nil "n")
;;     (current-buffer)))

(provide 'early-init)
;;; early-init.el ends here
