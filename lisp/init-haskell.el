;;; init-haskell --- -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;; 09/20/22 switch to language server (eglot) from tags

;;; Code:

(require-package 'haskell-mode)
(require-package 'elm-mode)

(use-package flymake-hlint :ensure t
  :commands (flymake-hlint-load)
  :init (add-hook 'haskell-mode-hook 'flymake-hlint-load))

(autoload 'org-babel-execute:haskell "~/.emacs.d/site-lisp/inf-haskell/ob-haskell.el")

(use-package inf-haskell
  :load-path "~/.emacs.d/site-lisp/inf-haskell/"
  :commands (inf-haskell-minor-mode run-haskell))

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

(util:define-hook-setup 'haskell-mode-hook
  "`haskell-mode' setup."
  ;; (rainbow-delimiters-mode 1)
  (tree-sitter-hl-mode -1)			   ; prefer haskell-mode font lock
  (haskell-auto-insert-module-template)	; auto insert module template
  (interactive-haskell-mode -1)			; Haskell interactive mode
  ;; Haskell declaration scanning like 'beginning-of-defun' 'end-of-defun'
  (inf-haskell-minor-mode 1)
  (haskell-decl-scan-mode t))

;; disable input method since it i would have to reset it back to pyim
;; manually after use
(put 'haskell-unicode-input-method-enable 'disabled t)

(with-eval-after-load 'haskell-mode
  ;; (my/haskell-space-leader-def
  ;;  "t" 'haskell-process-do-type
  ;;  "i" 'haskell-process-do-info
  ;;  "l" 'haskell-process-load-file)
  (util:define-keys haskell-mode-map "" 'run-haskell)

  (setq-default
   haskell-interactive-mode-eval-mode 'haskell-mode
   haskell-interactive-mode-hide-multi-line-errors nil
   ;; haskell-interactive-types-for-show-ambiguous nil
   haskell-process-log nil
   haskell-process-type 'cabal-repl
   haskell-process-suggest-remove-import-lines t
   haskell-svg-render-images t))

;; interactive Haskell
;; start the repl 'haskell-interactive-bring
;; load the file 'haskell-process-load-or-reload

;; @see http://haskell.github.io/haskell-mode/manual/latest/REPL.html#REPL
;; 'run-haskell
;; 'switch-to-haskell

;; @see http://haskell.github.io/haskell-mode/manual/latest/Aligning-code.html#Aligning-code

(with-eval-after-load 'elm-mode
  (setq-default elm-interactive-command '("npm-do" "elm" "repl")
				elm-indent-offset 2))

(provide 'init-haskell)
;;; init-haskell.el ends here
