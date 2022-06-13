;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

;; {{ @see https://coredumped.dev/2020/01/04/native-shell-completion-in-emacs/
(with-eval-after-load 'shell
  (defun my/kill-process-buffer-when-exit (process event)
    "Kill buffer of PROCESS when it's terminated.
EVENT is ignored."
    (ignore event)
    (when (memq (process-status process) '(signal exit))
      (kill-buffer (process-buffer process))))
  ;; Enable auto-completion in `shell'.
  (native-complete-setup-bash))

(define-hook-setup 'shell-mode-hook
  "Set up `shell-mode'."
  ;; hook `completion-at-point', optional
  (add-hook 'completion-at-point-functions #'native-complete-at-point nil t)
  ;; `company-native-complete' is better than `completion-at-point'
  (setq-local company-backends '((company-files company-native-complete)))
  ;; try to kill buffer when exit shell
  (let* ((proc (get-buffer-process (current-buffer)))
	 (shell (file-name-nondirectory (car (process-command proc)))))
    ;; Don't waste time on dumb shell which `shell-write-history-on-exit' is binding to
    (unless (string-match shell-dumb-shell-regexp shell)
      (set-process-sentinel proc #'my/kill-process-buffer-when-exit))))
;; }}

;; {{ @see http://emacs-journey.blogspot.com.au/2012/06/improving-ansi-term.html
;; TODO - see if process buffer would exit without this advice
;; (advice-add 'term-sentinel :after #'my/kill-process-buffer-when-exit)

(with-eval-after-load 'term
  ;; utf8
  (defun my/term-use-utf8 ()
    ;; "Ensure the ansi-term has the utf8 encoding."
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
  (add-hook 'term-exec-hook #'my/term-use-utf8))
;; }}

;; {{ comint-mode
(with-eval-after-load 'comint
  ;; Don't echo passwords when communicating with interactive programs:
  ;; Github prompt is like "Password for 'https://user@github.com/':"
  (setq comint-password-prompt-regexp
        (format "%s\\|^ *Password for .*: *$" comint-password-prompt-regexp)
        comint-prompt-read-only t)
  (define-key comint-mode-map
    ;; look up shell command history
    ;; (kbd "M-n") #'counsel-shell-history
    ;; Don't show trailing whitespace in REPL.
    (kbd "M-;") #'comment-dwim)
  (add-hook 'comint-output-filter-functions #'comint-watch-for-password-prompt))
;; }}

(with-eval-after-load 'gdb-mi
  (define-key gdb-inferior-io-mode-map "q" 'quit-window))

(provide 'init-shell)
;;; init-shell.el ends here
