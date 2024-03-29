;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:

;; used to avoid spell-checking doublon in certain major modes
(defvar-local my/flyspell-check-doublon t
  "Check doublon (double word) when calling `flyspell-highlight-incorrect-region'.")

(setq flyspell-use-meta-tab nil)	; disable C-M-i or M-\t
(with-eval-after-load 'flyspell
  ;; uncomment to disable 'flyspell-correct-word
  ;; (define-key flyspell-mouse-map [mouse-2] nil)

  ;; Cursor and background gets mixed up
  ;; (set-face-attribute 'flyspell-incorrect nil :background nil)

  ;; better performance
  (setq flyspell-issue-message-flag nil)

  (defun my/flyspell-highlight-incorrect-region-pred (beg end poss)
    "Don't mark doublon (double words) as typo."
    (and (not my/flyspell-check-doublon)
         (eq 'doublon poss)))
  (add-to-list 'flyspell-incorrect-hook
               'my/flyspell-highlight-incorrect-region-pred))

;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(with-eval-after-load 'ispell
  (setq ispell-program-name (-find 'executable-find '("aspell" "hunspell"))
		ispell-extra-args
        '("--sug-mode=ultra" "--lang=en_UK" "--camel-case"
          ;; @see https://github.com/redguardtoo/emacs.d/issues/796
          ;; "--run-together" "--run-together-limit=16"
          )
		ispell-silently-savep t))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; You can also use "M-x ispell-word" or hotkey "M-$". It pop up a multiple choice
;; @see http://frequal.com/Perspectives/EmacsTip03-FlyspellAutoCorrectWord.html
;; (global-set-key [?\C-c ?s] #'flyspell-auto-correct-word)

(provide 'init-spelling)
;;; init-spelling.el ends here
