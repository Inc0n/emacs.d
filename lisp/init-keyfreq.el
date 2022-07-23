;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require-package 'keyfreq)

(defun turnon-keyfreq-mode ()
  (interactive)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(defun turnoff-keyfreq-mode ()
  (interactive)
  (keyfreq-mode -1)
  (keyfreq-autosave-mode -1))

(with-eval-after-load 'keyfreq
  (setq keyfreq-excluded-commands
		'(self-insert-command
          abort-recursive-edit
          ace-jump-done
          ace-jump-move
          ace-window
          avy-goto-line
          backward-char
          backward-kill-word
          backward-word
          clipboard-kill-ring-save
          comint-previous-input
          comint-send-input
          delete-backward-char
          describe-variable
          dired							; nothing to optimize in dired
          dired-do-async-shell-command
          dired-find-file
          diredp-next-line
          diredp-previous-line
          electric-pair-delete-pair
          eval-buffer
          exit-minibuffer
          forward-char
          forward-word
          gnus
          gnus-summary-exit
          gnus-summary-next-page
          gnus-summary-scroll-up
          gnus-topic-select-group
          goto-line
          hippie-expand
          ido-complete
          ido-delete-backward-updir
          ido-exit-minibuffer
          ido-switch-buffer
          indent-new-comment-line
          isearch-abort
          isearch-backward-regexp
          isearch-cancel
          isearch-delete-char
          isearch-exit
          isearch-forward-regexp
          isearch-other-control-char
          isearch-other-meta-char
          isearch-printing-char
          isearch-repeat-forward
          isearch-ring-retreat
          ispell-minor-check
		  ;; vertico-exit-input
		  ;; vertico-exit
          js-mode
          js2-line-break
          keyboard-escape-quit
          keyboard-quit
          keyfreq-mode
          keyfreq-save-now
          keyfreq-show
          kill-sentence
          left-char
          markdown-exdent-or-delete
          markdown-outdent-or-delete
          minibuffer-complete
          minibuffer-complete-and-exit
          minibuffer-keyboard-quit
          move-beginning-of-line
          move-end-of-line
          mwheel-scroll
          newline-and-indent
          next-history-element
          next-line
          org-beginning-of-line
          org-ctrl-c-ctrl-c
          org-cycle
          org-delete-backward-char
          org-end-of-line
          org-force-self-insert
          org-return
          org-self-insert-command
          org-todo
          orgtbl-self-insert-command
          package-menu-execute
          pcomplete
          previous-history-element
          previous-line
          push-button
          pwd
          quit-window
          right-char
          rjsx-electric-gt
          rjsx-electric-lt
          save-buffer
          save-buffers-kill-terminal
          scroll-down-command
          scroll-up-command
		  winum-select-nth-window
		  winum-select-window-0-or-10
          self-insert-command
          slime-autodoc-space
          smarter-move-beginning-of-line
          suspend-frame
          term-send-raw
          turnon-keyfreq-mode
          undefined ;; lambda function
          w3m-goto-url
          w3m-next-anchor
          w3m-view-this-url
          web-mode
          web-mode-complete
          web-mode-jshint
          web-mode-navigate
          web-mode-part-beginning
          web-mode-reload
          web-mode-reveal
          web-mode-surround
          web-mode-tag-beginning
          web-mode-test
          wgrep-finish-edit
          xterm-paste
          yank
          ;; mine
		  ))
  ;; (util/make-file keyfreq-file "()")
  )

;; And use keyfreq-show to see how many times you used a command.
;; It's recommended to use `keyfreq-mode' (could be in "~/.custom.el").
;; It's reported keyfreq is not compatible with `latex-mode'
;; @see https://github.com/redguardtoo/emacs.d/issues/767
;; (add-hook 'after-init-hook
;; 		  ;; Fire up keyfreq a few seconds later to start up Emacs faster
;; 		  (lambda ()
;; 			(run-with-idle-timer 2 nil 'turnon-keyfreq-mode)))

(provide 'init-keyfreq)
;;; init-keyfreq.el ends here
