;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require-package 'company)
(require-package 'native-complete)
(require-package 'company-native-complete)
(require-package 'company-c-headers)
(require-package 'company-statistics)

(add-hook 'after-init-hook
          (lambda () (run-with-idle-timer 2 nil 'global-company-mode)))

(with-eval-after-load 'company
  ;; @see https://github.com/company-mode/company-mode/issues/348
  (company-statistics-mode)
  ;; (setq company-format-margin-function #'company-detect-icons-margin)
  ;; #'company-dot-icons-margin
  (setq company-format-margin-function nil)

  ;; can't work with TRAMP
  (setq company-backends
        (mapcar 'my/company-backend-with-yas
                (delete 'company-ropemacs company-backends)))

  (customize-set-variable 'company-quick-access-modifier 'meta)

  ;; Press SPACE will accept the highlighted candidate and insert a space
  ;; "M-x describe-variable company-auto-complete-chars" for details.
  ;; So that's BAD idea.
  (setq company-auto-commit nil
	company-auto-commit-chars '())

  ;; ;; company-ctags is much faster out of box. No further optimiation needed
  ;; (unless (featurep 'company-ctags)
  ;;   (local-require 'company-ctags)
  ;;   ;; (autoload 'company-ctags-auto-setup "company-ctags")
  ;;   (company-ctags-auto-setup)
  ;;   (setq company-ctags-ignore-case t))

  (setq company-dabbrev-downcase nil
        ;; make previous/next selection in the popup cycles
        company-selection-wrap-around t
        ;; Some languages use camel case naming convention,
        ;; so company should be case sensitive.
        company-dabbrev-ignore-case nil
        ;; press M-number to choose candidate
        company-show-numbers t
        company-minimum-prefix-length 2
        company-idle-delay 0.1
        company-clang-insert-arguments nil
        company-require-match nil
        ;; @see https://github.com/company-mode/company-mode/issues/146
        company-tooltip-align-annotations t)

  ;; NOT to load company-mode for certain major modes.
  ;; https://github.com/company-mode/company-mode/issues/29
  (setq company-global-modes
        '(not eshell-mode
              comint-mode
              erc-mode
              gud-mode
              rcirc-mode
              minibuffer-inactive-mode)))

(with-eval-after-load 'company-ispell
  (when (boundp 'ispell-alternate-dictionary)
    (setq company-ispell-dictionary
	  ispell-alternate-dictionary)))

;; {{ setup company-ispell
(defun toggle-company-ispell ()
  "Toggle company-ispell."
  (interactive)
  (if (memq 'company-ispell company-backends)
      (progn
        (setq company-backends (delq 'company-ispell company-backends))
        (message "company-ispell disabled"))
    (add-to-list 'company-backends 'company-ispell)
    (message "company-ispell enabled!")))

(defun company-ispell-setup ()
  "My company Ispell setup."
  ;; @see https://github.com/company-mode/company-mode/issues/50
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-ispell)))

(add-hook 'org-mode-hook #'company-ispell-setup)
;; }}


(defun my/company-backend-with-yas (backends)
  "Add :with `company-yasnippet' to company BACKENDS.
Taken from https://github.com/syl20bnr/spacemacs/pull/179."
  (if (and (listp backends)
           (memq 'company-yasnippet backends))
      backends
    (append (if (consp backends)
		backends
	      (list backends))
	    '(:with company-yasnippet))))

;; (setq-local yas-keymap-disable-hook '(company--active-p))

(provide 'init-company)
;;; init-company.el ends here

