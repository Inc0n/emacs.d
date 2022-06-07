;;; init-evil.el --- evil setup -*- coding: utf-8; -*-
;;; Commentary:
;;; Code:

(use-package evil :ensure t
  :defer t
  :commands (evil-define-key)
  :config
  ;; don't add replaced text to `kill-ring', prevent copy with visual
  (setq evil-kill-on-visual-paste nil)
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; @see https://bitbucket.org/lyro/evil/issue/360/possible-evil-search-symbol-forward
  ;; search word instead of symbol
  (setq evil-symbol-word-search t
        evil-want-minibuffer nil)
  (setq evil-goto-definition-functions
        '(evil-goto-definition-imenu
          evil-goto-definition-semantic
          evil-goto-definition-xref
          evil-goto-definition-search))

  ;; @see https://bitbucket.org/lyro/evil/issue/342/evil-default-cursor-setting-should-default
  (setq evil-default-cursor t ; Cursor is always black because of evil.
        evil-move-cursor-back t ; `backward-char' when exiting insert mode
        evil-move-beyond-eol nil
        evil-auto-indent t)
  :init
  (setq evil-respect-visual-line-mode t
        evil-want-fine-undo t
        evil-want-C-i-jump t)
  (evil-mode -1))

(use-package evil-matchit :ensure t
  :defer t
  :init
  (setq evilmi-shortcut "m"
	evilmi-may-jump-by-percentage nil)
  (add-hook 'after-init-hook 'global-evil-matchit-mode))

;; (local-require 'evil-mark-replace)

(use-package evil-exchange :ensure t
  :defer 1
  :config (evil-exchange-install)
  :init
  ;; press `evil-exchange-key' twice to exchange, gX to cancel
  (setq evil-exchange-key "zx"
        evil-exchange-cancel-key "zX"))

;; {{ @see https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org#replacing-text-with-iedit
;; same keybindings as spacemacs:
;;  - Start `iedit-mode' by `evil-iedit-state/iedit-mode'
;;  - "TAB" to toggle current occurrence
;;  - "n" next, "N" previous (obviously we use "p" for yank)
;;  - "gg" the first occurrence, "G" the last occurrence
;;  - Please note ";;" or `avy-goto-char-timer' is also useful
;; }}

;; Evil’s f/F/t/T command can search PinYin
(use-package evil-find-char-pinyin :ensure t
  :defer 2
  :config (evil-find-char-pinyin-mode))

(use-package evil-args :ensure t
  :after evil
  :defer t
  :init
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  (evil-define-key '(normal motion) 'global
    ;; Move the cursor out of the nearest enclosing matching pairs.
    ;; "K" 'evil-jump-out-args
    "L" 'evil-forward-arg
    "H" 'evil-backward-arg))

;; {{
(with-eval-after-load 'evil
  ;; evil initial states
  (cl-loop for (state . modes) in
	   '(;; (motion xref--xref-buffer-mode speedbar-mode esup-mode)
	     (emacs eshell-mode))
           do (dolist (mode modes)
		(evil-set-initial-state mode state)))
  ;; (setq evil-buffer-regexps '(("^\\*.*\\*" . motion)))
  (setq evil-buffer-regexps nil)

  (defun advice/evil-initial-state-for-buffer (orig-fn &optional buffer default)
    "Force motion state if BUFFER is not visiting any file.
Emacs state stay Emacs state.  Argument ORIG-FN."
    (let ((state (funcall orig-fn buffer default)))
      (if (and (eq state 'normal)
               buffer-read-only
               (eq (key-binding [?q] t)
                   'quit-window))
          'motion
	state)))
  (advice-add 'evil-initial-state-for-buffer
              :around 'advice/evil-initial-state-for-buffer)

  ;; Undo setup for evil
  (customize-set-variable 'evil-undo-system 'undo-redo)
  
  ;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
  (with-eval-after-load 'git-timemachine
    (evil-make-overriding-map 'git-timemachine-mode-map 'normal)
    ;; force update evil keymaps after git-timemachine-mode loaded
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

  (setq evil-ex-search-direction 'forward)

  (evil-define-key '(normal insert) 'global
    ;; [tab] nil
    ;; (lambda ()
    ;;   (interactive)
    ;;   (if (yas-active-snippets)
    ;;	   (yas-next-field-or-maybe-expand)
    ;;	 (when (null (yas-expand))
    ;;	   (indent-for-tab-command))))
    [?\C-y] 'yank)

  (evil-define-key 'insert 'global
    ;; [escape] #'evil-normal-state
    ;; restore Emacs key bind in insert
    [?\C-d] #'delete-char
    [?\C-n] #'next-line			; #'evil-complete-next
    [?\C-p] #'previous-line		; #'evil-complete-previous
    [?\C-a] #'move-beginning-of-line
    [?\C-e] #'move-end-of-line
    [?\C-v] nil
    [?\C-k] nil)

  (evil-define-key 'normal 'global
    "U" #'join-line
    "Q" "qq"		    ; #'kmacro-start-macro ; record kmacro in register q
    ;; g leader key edit
    "g0" #'goto-char
    "go" #'endless/capitalize
    "gl" #'endless/downcase
    "gu" #'endless/upcase
    "gc" #'comment-line			; same as doom-emacs
    "gy" #'comment-and-copy-line
    "gr" #'copy-and-paste		; [?Y ?p]

    [tab] #'indent-for-tab-command
    [return] #'newline-and-indent
    [backspace] #'delete-backward-char
    [?\M-.] 'nil
    "n" 'evil-search-next
    "N" 'evil-search-previous

    [?\C-v] 'scroll-up-command)

  (evil-define-key '(motion normal) 'global
    [C-i] #'evil-jump-forward
    ;; g leader key
    "gs" #'consult-recent-file
    "gS" #'nil
    "gj" #'avy-goto-line-below
    "gk" #'avy-goto-line-above
    "gf" #'find-file-at-point
    "gb" #'consult-buffer)

  (evil-define-key 'visual 'global
    "v" #'er/expand-region
    "/" #'visual-replace-regexp-at-point
    "S" #'wrap-region-evil-activate)

  (evil-define-key 'motion 'global
    ;; free up some keys
    [tab] nil
    [return] nil

    ;; now it will be prev-next-line instead of evil-next-prev-line
    [up] nil
    [down] nil
    ;;
    "n" nil
    "q" 'quit-window
    "N" 'evil-search-next
    "P" 'evil-search-previous)
  
  (with-eval-after-load 'org-agenda
    (evil-define-key '(normal motion) org-agenda-mode-map
      "+" 'org-agenda-quick-capture))

  (evil-define-key '(motion normal) org-mode-map
    ;; custom g leader key
    "gJ" 'org-move-subtree-down
    "gK" 'org-move-subtree-up

    "$" 'org-end-of-line		; smarter behaviour on headlines etc.
    "^" 'org-beginning-of-line		; ditto
    "<" 'org-do-promote
    ">" 'org-do-demote
    ;; [ret] #'newline-and-indent

    [tab] 'org-cycle
    [return] 'org-return)

  (evil-define-key 'normal markdown-mode-map
    "gh" #'outline-up-heading
    [tab] #'markdown-cycle)

  (evil-define-key nil evil-window-map
    [up] #'evil-window-up
    [down] #'evil-window-down
    [left] #'evil-window-left
    [right] #'evil-window-right)

  ;; ffip-diff-mode (read only) evil setup
  (define-hook-setup 'ffip-diff-mode-hook
    (evil-define-key 'normal 'local
      "q" (lambda () (interactive) (quit-window t))
      [ret] #'ffip-diff-find-file
      ;; "C-c C-a" is binding to `diff-apply-hunk' in `diff-mode'
      "a" #'ffip-diff-apply-hunk
      "o" #'ffip-diff-find-file)))
;; }}

(provide 'init-evil)
