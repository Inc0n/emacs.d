;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;; use pdf-tools to view pdf
;; run "M-x pdf-tool-install" at debian and open pdf in GUI Emacs

;;; Code:

(use-package pdf-tools
  :ensure t
  :defer 1
  :if (display-graphic-p)
  :config (pdf-loader-install))

(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map "k" #'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map "j" #'pdf-view-next-line-or-next-page))

(with-eval-after-load 'doc-view
  (define-key doc-view-mode-map "k" #'previous-line)
  (define-key doc-view-mode-map "j" #'next-line))

(provide 'init-pdf)
