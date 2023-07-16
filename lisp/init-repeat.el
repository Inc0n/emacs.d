;;; init-repeat --- setup for repeat mode -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:
;;

(eval-when-compile (require 'cl-macs)
				   (require 'use-package))

(use-package repeat
  :defer t
  :config (setq repeat-on-final-keystroke t)
  :init (add-hook 'emacs-startup-hook 'repeat-mode))

(defmacro define-repeat-mode-map (name &rest key-binds)
  "Define helper for repaet mode map using NAME and KEY-BINDS."
  (let ((map-name (intern (format "%s-repeat-map" name))))
    (when (boundp map-name)
      (warn "Keymap `%s' is already defined, overwriting previous value %s."
            map-name
            (symbol-value map-name)))
    `(progn
       (defvar ,map-name (make-sparse-keymap))
       (cl-loop for (key command) on (list ,@key-binds) by #'cddr
                collect (define-key ,map-name key command))
       (cl-loop for (_key . command) in (cdr ,map-name)
                collect (put command 'repeat-map ',map-name))
       ',map-name)))

;; undo is already in this repeat map
(when nil (define-key undo-repeat-map "r" 'undo-redo))

(define-repeat-mode-map winner
  "u" 'winner-undo
  "r" 'winner-redo)

;; (define-repeat-mode-map flycheck
;;   "n" 'flycheck-next-error
;;   "p" 'flycheck-previous-error
;;   "h" 'flycheck-display-error-at-point
;;   "e" 'flycheck-explain-error-at-point)

(when nil (define-repeat-mode-map flymake
  "n" 'flymake-goto-next-error
  "p" 'flymake-goto-prev-error
  "h" 'flymake-display-warning))

;; (define-repeat-mode-map flyspell
;;   "n" 'flyspell-goto-next-error
;;   "c" 'flyspell-auto-correct-word)

;; (define-repeat-mode-map comment
;;   "c" 'comment-or-uncomment-sexp-at-point)

(provide 'init-repeat)
;;; init-repeat.el ends here
