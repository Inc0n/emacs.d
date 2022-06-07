;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(require-package 'haskell-mode)
(require-package 'elm-mode)

(custom-set-variables
 ;; '(haskell-interactive-types-for-show-ambiguous nil)
 '(haskell-interactive-mode-hide-multi-line-errors nil)
 '(haskell-process-log t)
 '(haskell-process-type 'cabal-repl) ;; 'ghci
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-tags-on-save t)
 '(haskell-svg-render-images t)
 '(haskell-hasktags-path (expand-file-name "~/.cabal/bin/hasktags")))

;; (setq haskell-interactive-mode-eval-mode 'haskell-mode)

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
  "My `haskell-mode' setup."
  ;; Haskell smarter completion
  ;; @see http://haskell.github.io/haskell-mode/manual/latest/Completion-support.html#Completion-support
  (add-to-list 'company-backends '(company-capf company-dabbrev-code))
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
  ;; (evil-define-key 'insert haskell-mode-map (kbd "M-.") nil)
  )

;; interactive Haskell
;; start the repl 'haskell-interactive-bring
;; load the file 'haskell-process-load-or-reload

;; @see http://haskell.github.io/haskell-mode/manual/latest/REPL.html#REPL
;; 'run-haskell
;; 'switch-to-haskell

;; @see http://haskell.github.io/haskell-mode/manual/latest/Aligning-code.html#Aligning-code

(with-eval-after-load 'elm-mode
  (setq elm-interactive-command '("npm-do" "elm" "repl"))
  (setq elm-indent-offset 2))

(provide 'init-haskell)