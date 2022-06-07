;;; init-transient --- my transient setup for serveal packages

;;; Commentary:
;; should load after the packages making transient interface for

;;; Code:

(require-package 'transient)
(autoload 'transient-define-prefix "transient" "transient")
;; (require 'transient)

(fset 'my/transient-suffix 'transient--do-stay)
(fset 'my/transient-non-suffix 'transient--do-stay)

(defmacro my/define-transient-command (name arg-list &rest args)
  "My `define-transient-command' setup for a transient stay interface.
NAME and ARG-LIST and ARGS check `define-transient-command'."
  (declare (debug (&define name
                           [&optional arg-list]
                           [&optional lambda-doc]
                           [&rest def-body]))
           (indent defun)
           (doc-string 3))
  (let ((doc-str (and (stringp (car args)) (car args)))
        (rest-args (if (stringp (car args)) (cdr args) args)))
    `(progn
       (transient-define-prefix ,name ,arg-list
         ,doc-str
         :transient-suffix 'my/transient-suffix
         :transient-non-suffix 'my/transient-non-suffix
         ,@rest-args
         (transient-setup ',name))
       ',name)))

;;;###autoload
(my/define-transient-command my/transient-flyspell ()
  "Transient interface for `flyspell'"
  ["Flyspell"
   ("n" "next error" flyspell-goto-next-error)
   ("c" "correct error or next correction" flyspell-auto-correct-word)
   ("q" "quit" transient-quit-one)]
  (interactive)
  (flyspell-goto-next-error))

;;;###autoload
(my/define-transient-command my/transient-flycheck (&optional arg)
  "Transient interface for `flycheck'"
  ["Flycheck"
   ("n" "next error" flycheck-next-error)
   ("p" "previous error" flycheck-previous-error)
   ("d" "display error" flycheck-display-error-at-point)
   ("e" "explain error" flycheck-explain-error-at-point)
   ("q" "quit" transient-quit-one)]
  (interactive "P")
  (if arg (flycheck-previous-error)
    (flycheck-next-error)))

;;;###autoload
(my/define-transient-command my/transient-winner (&optional arg)
  "Transient interface for `winner-undo'."
  ["Winner do"
   ("u" "undo" winner-undo)
   ("r" "redo" winner-redo)
   ("q" "quit" transient-quit-one)]
  (interactive "P")
  (if arg (winner-redo)
    (winner-undo)))

(provide 'init-transient)