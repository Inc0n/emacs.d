;;; init-haskell --- -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;; 09/20/22 switch to language server (eglot) from tags

;;; Code:

(require-package 'haskell-mode)
(require-package 'elm-mode)

(defun haskell-interactive-toggle-print-mode ()
  (interactive)
  (setq haskell-interactive-mode-eval-mode
        (intern
         (completing-read "Eval result mode: "
                          '("fundamental-mode"
                            "haskell-mode"
                            "espresso-mode"
                            "ghc-core-mode"
                            "org-mode")))))

(define-hook-setup 'haskell-mode-hook
  "`haskell-mode' setup."
  ;; Haskell smarter completion
  ;; @see http://haskell.github.io/haskell-mode/manual/latest/Completion-support.html#Completion-support
  ;; (rainbow-delimiters-mode 1)

  ;; Haskell module auto insert template
  (haskell-auto-insert-module-template)
  ;; Haskell interactive mode
  (interactive-haskell-mode t)
  ;; Haskell declaration scanning like 'beginning-of-defun' 'end-of-defun'
  (haskell-decl-scan-mode t))

;; disable input method since it i would have to reset it back to pyim
;; manually after use
(put 'haskell-unicode-input-method-enable 'disabled t)

(with-eval-after-load 'haskell-mode
  ;; (my/haskell-space-leader-def
  ;;  "t" 'haskell-process-do-type
  ;;  "i" 'haskell-process-do-info
  ;;  "l" 'haskell-process-load-file)
  ;; (define-key haskell-mode-map (kbd "M-.") nil)
  (defun haskell-interactive-mode-eval-region (start end)
	(interactive "r")
	(haskell-process-send-string
	 (haskell-interactive-process)
     (buffer-substring start end))
	;; haskell-interactive-mode-run-expr
	)

  (setq haskell-interactive-mode-eval-mode 'haskell-mode
		haskell-interactive-mode-hide-multi-line-errors nil
		;; haskell-interactive-types-for-show-ambiguous nil
		haskell-process-log nil
		haskell-process-type 'auto ;; 'ghci
		haskell-process-suggest-remove-import-lines t
		haskell-svg-render-images t))

;; interactive Haskell
;; start the repl 'haskell-interactive-bring
;; load the file 'haskell-process-load-or-reload

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  ;; (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (add-hook 'haskell-mode-hook 'dante-mode))

;; @see http://haskell.github.io/haskell-mode/manual/latest/REPL.html#REPL
;; 'run-haskell
;; 'switch-to-haskell

;; @see http://haskell.github.io/haskell-mode/manual/latest/Aligning-code.html#Aligning-code

(with-eval-after-load 'elm-mode
  (setq elm-interactive-command '("npm-do" "elm" "repl"))
  (setq elm-indent-offset 2))

(provide 'init-haskell)
;;; init-haskell.el ends here
