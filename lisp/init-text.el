;;; init-text --- -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;
;;  Text mode setup
;;
;;; Code:

;; improves the word-wrapping for CJK text mixed with Latin text
(customize-set-variable 'word-wrap-by-category t)

(use-package flymake-proselint :straight t ; We need the latest changes
  :commands (flymake-proselint-setup)
  :config
  (setq flymake-proselint-disable '(typography.symbols.sentence_spacing)))

(use-package writegood-mode :ensure t :commands (writegood-mode))

(use-package flymake-languagetool :ensure t
  :defer t
  :config
  (setq flymake-languagetool-ignore-faces-alist
		'((org-mode org-code org-block org-block-begin-line)
		  (t ef-themes-fixed-pitch)))
  :init
  ;; Remote server config with LanguageTool's free API
  ;; (setq flymake-languagetool-url "https://api.languagetool.org")
  ;; (setq flymake-languagetool-server-port nil)

  ;; Local server config
  (setq flymake-languagetool-check-spelling nil
		flymake-languagetool-language "en-GB"
		flymake-languagetool-server-jar
		"/opt/homebrew/opt/languagetool/libexec/languagetool-server.jar"))

(setq text-scale-mode-step 1.05) ;; (text-scale-set 0)

(setq-default fill-column 95)			; my fonts are bigger

(with-eval-after-load 'visual-fill-column
  (setq-default visual-fill-column-center-text t
				;; Additional margin needed for Unicode text width
				visual-fill-column-extra-text-width '(0 . -6))
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))


(add-hook 'text-mode-hook 'my/text-mode-setup)
(defun my/text-mode-setup ()
  (toggle-word-wrap 1)
  ;; (when (and (version<= "29.0" emacs-version)
  ;; 			 (fboundp 'pixel-scroll-precision-mode))
  ;;   (pixel-scroll-precision-mode -1))

  ;; keep text mode: Emms tag edit mode Monospace
  (when (derived-mode-p 'org-mode)
	(make-local-variable 'flymake-languagetool-disabled-rules)
	(writegood-mode 1)
	(flymake-mode 1)
	(flymake-proselint-setup)
	(flymake-languagetool-load)
	(add-to-list 'flymake-languagetool-disabled-rules "WHITESPACE_RULE")
    (puni-mode 1)
	(setq-local fill-column 100)
	(visual-fill-column-mode 1)
	;; (text-scale-set 1.4)
	(buffer-face-set 'variable-pitch)	; fixed-pitch-serif
	;; (variable-pitch-mode 1)			; buffer-face-mode
	(face-remap-add-relative 'variable-pitch  :height 1.0)
	))

(with-eval-after-load 'faces
  (set-face-attribute 'default nil :height 140)
  (set-face-attribute 'default nil :font "JetBrains Mono-14")
  (set-face-attribute 'fixed-pitch nil :font "Dejavu Sans Mono" :height 1.0)
  ;; Curly is more easy to the eyes than ss02
  (set-face-attribute 'fixed-pitch-serif nil
					  :font "Iosevka Curly Slab" :height 1.0)
  ;; I stopped using serif fonts, because the spacing between words
  (set-face-attribute 'variable-pitch nil
					  :height 1.15
					  :family
					  ;; serif
					  ;; "Dejavu Serif"
					  ;; "Literata"
					  ;; "Bookerly"
					  "PT Serif"
					  ;; sans serif
					  ;; "Libre Baskerville"
					  ;; "Optima"
					  ;; "SF Mono"
					  ))
(with-eval-after-load 'org-indent
  ;; (set-face-attribute 'org-hide nil :inherit 'variable-pitch)
  (set-face-attribute 'org-indent nil :inherit '(fixed-pitch-serif
												 org-hide)))

(use-package markdown-mode :ensure t
  :defer t
  :mode "\\.\\(m[k]d\\|markdown\\)\\'"
  :config
  (util:define-hook-setup 'markdown-mode-hook
	"Make markdown tables saner via `orgtbl-mode'.
Insert org table and it will be automatically converted to
markdown table.  Taken from http://stackoverflow.com/a/26297700."
	(add-hook 'after-save-hook
              (defun cleanup-org-tables ()
				(save-excursion
                  (goto-char (point-min))
                  (while (search-forward "-+-" nil t) (replace-match "-|-"))))
              nil :local)
	(turn-on-auto-fill)
	(when (boundp 'orgtbl-mode)
	  (orgtbl-mode 1)) ; enable key bindings
	;; don't wrap lines because there is table in `markdown-mode'
	(setq truncate-lines nil)
	(setq imenu-generic-expression '((nil "^#\\([# ]*[^#\n\r]+\\)" 1))))
  ;; `pandoc' is better than `markdown'
  (when (executable-find "pandoc")
    (setq-default markdown-command "pandoc -f markdown"))
  ;; deprecate in favor of ef-themes-heading
  ;; (custom-set-faces
  ;;  '(markdown-header-face-1
  ;;    ((t :height 1.25 :weight extra-bold :inherit markdown-header-face)))
  ;;  '(markdown-header-face-2
  ;;    ((t :height 1.15 :weight bold :inherit markdown-header-face)))
  ;;  '(markdown-header-face-3
  ;;    ((t :height 1.08 :weight bold :inherit markdown-header-face)))
  ;;  '(markdown-header-face-4
  ;;    ((t :height 1.0 :weight bold :inherit markdown-header-face)))
  ;;  '(markdown-header-face-5
  ;;    ((t :height 0.9 :weight bold :inherit markdown-header-face)))
  ;;  '(markdown-header-face-6
  ;;    ((t :height 0.75 :weight extra-bold :inherit markdown-header-face))))
  )

;;; Latex

(require-package 'auctex)
;; 09/26/22 I want to control fill-column to respect custom layouts.
;; With '\\', I do not know how to make it happen though.
;; (setq fill-nobreak-predicate nil)

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

(util:define-hook-setup 'LaTeX-mode-hook
  (flyspell-mode 1)
  (LaTeX-math-mode 1)
  (visual-line-mode 1)					; enable `word-wrap'
  (electric-pair-mode 1))

(with-eval-after-load 'flycheck
  (flycheck-define-checker vale
	"A checker for prose"
	:command ("vale" "--output" "line" source)
	:standard-input nil
	:error-patterns
	((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
	:modes (markdown-mode org-mode text-mode))
  (add-to-list 'flycheck-checkers 'vale 'append))

(with-eval-after-load 'package
  (define-key package-menu-mode-map "l" [return]))

;; pdf-tools, should be removed
(with-eval-after-load 'pdf-view
  (util:define-keys pdf-view-mode-map
	"k" #'pdf-view-previous-line-or-previous-page
	"j" #'pdf-view-next-line-or-next-page))

(with-eval-after-load 'doc-view
  (util:define-keys doc-view-mode-map
	"k" #'previous-line
	"j" #'next-line))

;; epub setup
(use-package nov :ensure t
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-variable-pitch nil
		nov-text-width t)
  (util:define-keys nov-mode-map
	"j" 'next-line
	"k" 'previous-line)
  (add-hook 'nov-post-html-render-hook
			(defun nov-post-html-render-setup ()
			  ;; Put code here to execute everytime
			  ;; `nov-render-document'
			  (variable-pitch-mode 1)
			  (visual-line-mode 1)
			  (face-remap-add-relative
			   'variable-pitch :family "Baskerville")))

  (util:define-hook-setup 'nov-mode-hook
	;; (face-remap-add-relative 'variable-pitch
	;;							:family "Libreation Serif"
	;;							:width 'semi-expanded)
	;; (dolist (face '(fixed-pitch-serif
	;; 				variable-pitch-serif))
	;;   (face-remap-add-relative face :family "Baskerville"))
	(setq-local line-spacing 0.0)
	(setq-local visual-fill-column-center-text t
				visual-fill-column-extra-text-width '(0 . 0))
	(visual-fill-column-mode 1)
	(visual-line-mode 1)
	;; 1.5 scale of current font height
	;; (set-face-attribute 'default (selected-frame) :height (truncate (* 1.5 (face-attribute 'default :height))))
	;; (setq-local simple-modeline-segments
	;; 			`((simple-modeline-segment-winum
	;; 			   simple-modeline-segment-evil-modal
	;; 			   simple-modeline-segment-modified
	;; 			   simple-modeline-segment-nov-info)
	;; 			  (simple-modeline-segment-position
	;; 			   simple-modeline-segment-major-mode)))
	))

(provide 'init-text)
;;; init-text.el ends here

