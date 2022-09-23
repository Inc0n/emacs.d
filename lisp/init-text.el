;;; init-text --- -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;;  Text mode setup
;;
;;; Code:

;; improves the word-wrapping for CJK text mixed with Latin text
(customize-set-variable 'word-wrap-by-category t)

(add-hook 'text-mode-hook 'my/text-mode-setup)
(defun my/text-mode-setup ()
  (pixel-scroll-mode 1)
  (toggle-word-wrap 1)
  ;; (when (and (version<= "29.0" emacs-version)
  ;; 			 (fboundp 'pixel-scroll-precision-mode))
  ;;   (pixel-scroll-precision-mode -1))

  ;; keep text mode: Emms tag edit mode Monospace
  (when (derived-mode-p 'org-mode)
	;; variable pitch for text mode buffers
	(setq buffer-face-mode-face 'variable-pitch)
	(buffer-face-mode 1)
	;; bold current line breaks out...
	(face-remap-add-relative 'line-number-current-line
							 :inherit 'default)
	))

(with-eval-after-load 'faces
  (set-face-attribute
   'variable-pitch nil
   :family
   ;; "DejaVu Sans"
   "Verdana"
   ))

(use-package markdown-mode :ensure t
  :defer t
  :mode "\\.\\(m[k]d\\|markdown\\)\\'"
  :config
  ;; `pandoc' is better than obsolete `markdown'
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -f markdown"))
  (custom-set-faces
   '(markdown-header-face-1
     ((t :height 1.25 :weight extra-bold :inherit markdown-header-face)))
   '(markdown-header-face-2
     ((t :height 1.15 :weight bold :inherit markdown-header-face)))
   '(markdown-header-face-3
     ((t :height 1.08 :weight bold :inherit markdown-header-face)))
   '(markdown-header-face-4
     ((t :height 1.0 :weight bold :inherit markdown-header-face)))
   '(markdown-header-face-5
     ((t :height 0.9 :weight bold :inherit markdown-header-face)))
   '(markdown-header-face-6
     ((t :height 0.75 :weight extra-bold :inherit markdown-header-face)))))

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
            nil :local)
  (turn-on-auto-fill)
  (orgtbl-mode 1)                       ; enable key bindings
  ;; don't wrap lines because there is table in `markdown-mode'
  (setq truncate-lines nil)
  (setq imenu-generic-expression '((nil "^#\\([# ]*[^#\n\r]+\\)" 1))))

;;; Latex

(require-package 'auctex)
;; (local-require 'calctex)

;; auto-insert-mode

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
  ;; (add-to-list 'preview-default-preamble "\\PreviewEnvironment{enumerate}")
  ;; "\n\\PreviewEnvironment{tabular}"
  (add-to-list 'preview-default-preamble
			   "\\PreviewEnvironment{tikzpicture}"))

;; (custom/reset-var 'preview-default-preamble)

(define-hook-setup 'LaTeX-mode-hook
  (flyspell-mode 1)
  (LaTeX-math-mode 1)
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
  (define-key package-menu-mode-map
	"l" [return]))

;; pdf-tools, should be removed
(with-eval-after-load 'pdf-view
  (define-keys pdf-view-mode-map
	"k" #'pdf-view-previous-line-or-previous-page
	"j" #'pdf-view-next-line-or-next-page))

(with-eval-after-load 'doc-view
  (define-keys doc-view-mode-map
	"k" #'previous-line
	"j" #'next-line)
  ;; 09/06/22 There is a bug in mac port implementation
  (defun doc-view-djvu->tiff-converter-ddjvu (djvu tiff page callback)
	"Convert PAGE of a DJVU file to bitmap(s) asynchronously.
Call CALLBACK with no arguments when done.
If PAGE is nil, convert the whole document."
	(doc-view-multiplex-conversion
	 (lambda (tiff callback &optional resolution)
       (doc-view-start-process
		"djvu->tiff" "ddjvu"
		`("-format=tiff"
          ;; ddjvu only accepts the range 1-999.
          ,(format "-scale=%d" (round (or resolution doc-view-resolution)))
          ;; -eachpage was only added after djvulibre-3.5.25.3!
          ,@(unless page '("-eachpage"))
          ,@(if page `(,(format "-page=%d" page)))
          ,djvu
          ,tiff)
		callback))
	 tiff callback)))

(provide 'init-text)
;;; init-text.el ends here
