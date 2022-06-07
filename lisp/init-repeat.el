;;; init-repeat --- setup for repeat mode -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;

;;; Cpode:
;;

(use-package repeat
  :defer 1
  :config (setq repeat-on-final-keystroke t)
  :init (add-hook 'emacs-startup-hook 'repeat-mode))

(defmacro define-repeat-mode-map (name &rest key-binds)
  "Define helper for repaet mode map using NAME and KEY-BINDS."
  (let ((map-name (intern (format "%s-repeat-map" name))))
    (if (boundp map-name)
        (warn "Keymap `%s' is already defined, overwriting previous value %s."
              map-name
              (symbol-value map-name)))
    `(progn
       (defvar ,map-name (make-sparse-keymap))
       ,@(cl-loop for (key command) on key-binds by #'cddr
                  collect `(define-key ,map-name ,key ,command))
       ,@(cl-loop for (key command) on key-binds by #'cddr
                  collect `(put ,command 'repeat-map ',map-name))
       ',map-name)))

(define-keys undo-repeat-map
  ;; undo is already in this repeat map
  "r" 'undo-redo)

(define-repeat-mode-map winner
  "u" 'winner-undo
  "r" 'winner-redo)

(define-repeat-mode-map last-change
  ",")



;; (put 'selectrum-next-candidate 'repeat-map 'selectsel-quick-navigate-map)
;; (define-repeat-mode-map flycheck
;;   "n" 'flycheck-next-error
;;   "p" 'flycheck-previous-error
;;   "d" 'flycheck-display-error-at-point
;;   "e" 'flycheck-explain-error-at-point)

;; (define-repeat-mode-map flyspell
;;   "n" 'flyspell-goto-next-error
;;   "c" 'flyspell-auto-correct-word)

;; (define-repeat-mode-map comment
;;   "c" 'comment-or-uncomment-sexp-at-point)

(provide 'init-repeat)
;;; init-repeat.el ends here