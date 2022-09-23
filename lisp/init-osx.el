;;; init-osx -- osx setup
;;; Commentary:
;; osx packages

;;; Code:

(require-package 'applescript-mode)
(require-package 'swift-mode)

(when (eq system-type 'darwin)
  (defun dictionary-lookup (word)
    "Lookup WORD from Dictionary.app."
    (interactive (list (if (region-active-p)
                           (util/thing-at-point/deselect)
                         (read-string "Dictionary: " (thing-at-point 'word)))))
    (shell-command (format "open dict://%s" word))))

(provide 'init-osx)
;;; init-osx.el ends here
