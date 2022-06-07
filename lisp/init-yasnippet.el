;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require-package 'yasnippet)
;; (require-package 'yasnippet-snippets)

;; Use C-q instead tab to complete snippet
;; - aya-create at first, input ~ to mark the thing next
;; - aya-expand to expand snippet
;; - aya-open-line to finish
(require-package 'auto-yasnippet)

(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'text-mode-hook #'yas-minor-mode)

(with-eval-after-load 'org 
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'yas-keymap-disable-hook 'org-at-heading-p))))

(defun my/yas-reload-all ()
  "Compile and reload snippets.
Run the command after adding new snippets."
  (interactive)
  ;; (yas-compile-directory (my/emacs-d "snippets"))
  (if (not (-every 'file-exists-p (yas-snippet-dirs)))
      (warn "yas not reloaded (snippet dirs not found)")
    (yas-recompile-all)
    (yas-reload-all)))

(defun my/yas-escape-string (s)
  "Regex escape string S."
  (replace-regexp-in-string
   "\"" "\\\\\""
   (replace-regexp-in-string "'" "\\\\'" s)))

(with-eval-after-load 'yasnippet
  ;; http://stackoverflow.com/questions/7619640/emacs-latex-yasnippet-why-are-newlines-inserted-after-a-snippet
  (setq-default mode-require-final-newline nil)
  ;; Use `yas-dropdown-prompt' if possible. It requires `dropdown-list'.
  ;; (setq-default yas/prompt-functions (delete 'yas-dropdown-prompt yas/prompt-functions))
  ;; (setq yas-prompt-functions '(yas-dropdown-prompt
  ;;                              yas-completing-prompt
  ;;   						   yas-maybe-ido-prompt))
  ;; yas fallback when no expansion found
  (setq yas-fallback-behavior 'return-nil)

  ;; how to add custom yasnippet directory
  ;; (add-to-list 'yas-snippet-dirs my/yasnippets)
  (my/yas-reload-all))

(provide 'init-yasnippet)
;;; init-yasnippet ends here
