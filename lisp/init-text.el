;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

;; improves the word-wrapping for CJK text mixed with Latin text
(customize-set-variable 'word-wrap-by-category t)

(add-hook 'text-mode-hook 'my/text-mode-setup)
(defun my/text-mode-setup ()
  (pixel-scroll-mode 1)
  (toggle-word-wrap 1)
  (when (and (version<= "29.0" emacs-version)
	     (fboundp 'pixel-scroll-precision-mode))
    (pixel-scroll-precision-mode -1)))

(use-package markdown-mode :ensure t
  :defer t
  :mode "\\.\\(m[k]d\\|markdown\\)\\'"
  :config
  ;; `pandoc' is better than obsolete `markdown'
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -f markdown"))
  (custom-set-faces
   '(markdown-header-face-1
     ((t (:height 1.25 :weight extra-bold :inherit markdown-header-face))))
   '(markdown-header-face-2
     ((t (:height 1.15 :weight bold :inherit markdown-header-face))))
   '(markdown-header-face-3
     ((t (:height 1.08 :weight bold :inherit markdown-header-face))))
   '(markdown-header-face-4
     ((t (:height 1.0 :weight bold :inherit markdown-header-face))))
   '(markdown-header-face-5
     ((t (:height 0.9 :weight bold :inherit markdown-header-face))))
   '(markdown-header-face-6
     ((t (:height 0.75 :weight extra-bold :inherit markdown-header-face))))))

(define-hook-setup 'markdown-mode-hook
  "Make markdown tables saner via `orgtbl-mode'.
Insert org table and it will be automatically converted
to markdown table.
Check Stolen from http://stackoverflow.com/a/26297700"
  (add-hook 'after-save-hook
            (defun cleanup-org-tables ()
              (save-excursion
                (goto-char (point-min))
                (while (search-forward "-+-" nil t) (replace-match "-|-"))))
            nil 'make-it-local)
  (turn-on-auto-fill)
  (orgtbl-mode 1)                       ; enable key bindings
  ;; don't wrap lines because there is table in `markdown-mode'
  (setq truncate-lines nil)
  (setq imenu-generic-expression '((nil "^#\\([# ]*[^#\n\r]+\\)" 1))))

;;; Latex

(require-package 'auctex)
;; (local-require 'calctex)

;; (setq auto-insert-query nil)
;; (setq auto-insert-directory (my/emacs-d "auto-insert"))
;; (define-auto-insert "\\.tex$" "latex-notes-template.tex")

(defun my/yas-insert-template (name)
  "Insert template when empty buffer."
  (when (= (point-min) (point-max))
    (cl-flet ((dummy-prompt
	       (prompt choices &optional display-fn)
	       (declare (ignore prompt))
	       (or (find name choices :key display-fn :test #'string=)
		   (throw 'notfound nil))))
      (let ((yas-prompt-functions '(dummy-prompt)))
	(catch 'notfound
          (yas-insert-snippet t))))))

(with-eval-after-load 'tex
  (setq tex-command "latex")
  (setq TeX-parse-self t)			 ; Enable parse on load.
  (setq TeX-auto-save t)			 ; Enable parse on save.
  (setq TeX-PDF-mode t)				 ; PDF mode (rather than DVI-mode)
  (setq TeX-electric-math '("$" . "$")))

(with-eval-after-load 'preview
  (setq preview-scale-function 1.4)
  (setq preview-auto-cache-preamble t
		preview-preserve-counters t)
  ;; (add-to-list 'preview-default-option-list "showbox")
  (add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}")
  ;; (add-to-list 'preview-default-preamble "\\PreviewEnvironment{enumerate}")
  ;; "\n\\PreviewEnvironment{tabular}"
  )

;; (custom/reset-var 'preview-default-preamble)

(define-hook-setup 'LaTeX-mode-hook
  (flyspell-mode 1)
  (LaTeX-math-mode 1)
  ;; (my/yas-insert-template "template")
  (visual-line-mode 1) ; enable `word-wrap'
  (electric-pair-mode 1))

(flycheck-define-checker vale
  "A checker for prose"
  :command ("vale" "--output" "line" source)
  :standard-input nil
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
  :modes (markdown-mode org-mode text-mode))
(add-to-list 'flycheck-checkers 'vale 'append)

(with-eval-after-load 'package
  (define-key package-menu-mode-map "l" [return]))

(provide 'init-text)
;;; init-text ends here
