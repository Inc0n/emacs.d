;;; init-osx -- osx setup
;;; Commentary:
;; osx packages

;;; Code:

(require-package 'applescript-mode)
(require-package 'swift-mode)

(when (eq system-type 'darwin)

  (defun osx-dictionary-lookup (word)
    "Lookup WORD from Dictionary.app."
    (interactive (list (if (region-active-p)
                           (util/thing-at-point/deselect)
                         (read-string "Dictionary: " (thing-at-point 'word)))))
    (let ((cur-buffer (current-buffer))
          (tmpbuf "* dict-process *"))
      (pop-to-buffer (get-buffer-create tmpbuf))
      (erase-buffer)
      ;; (insert word "\n")
      (let ((coding-system-for-read 'utf-8-mac)
            (coding-system-for-write 'utf-8-mac))
        (call-process "~/.emacs.d/site-lisp/osx-dict/osx-dict" nil tmpbuf nil word) ;; specify full pass of dict.py
        (local-set-key "q" 'quit-window)
        ;; (pop-to-buffer cur-buffer)
        ;; (let ((str (buffer-string))) (popup-tip str :scroll-bar t))
        )))
  (defun dictionary-lookup (word)
    "Lookup WORD from Dictionary.app."
    (interactive (list (if (region-active-p)
                           (util/thing-at-point/deselect)
                         (read-string "Dictionary: " (thing-at-point 'word)))))
    (shell-command (format "open dict://%s" word)))

  (defun open-in-term ()
    (interactive)
    (shell-command (format "open -a kitty -- ." default-directory))))

(provide 'init-osx)
;;; init-osx ends here
