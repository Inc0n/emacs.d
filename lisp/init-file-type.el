;; -*- coding: utf-8; lexical-binding: t; -*-


(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'csv-mode)
(require-package 'yaml-mode)

(require-package 'vimrc-mode)
(add-auto-mode 'vimrc-mode "\\.?vim\\(rc\\)?$")

(require-package 'cmake-mode)

;;; {{ shell and conf
(add-auto-mode 'conf-mode
               ;; "\\.[^b][^a][a-zA-Z]*rc$"
               "\\.aspell\\.en\\.pws$"
	       "\\(rc\\)$" ;; any file that ends with `rc'
               )
;; }}

(autoload 'verilog-mode "verilog-mode" "Verilog mode" t)
(add-auto-mode 'verilog-mode "\\.[ds]?va?h?\\'")

(add-auto-mode 'text-mode "TAGS\\'" "\\.ctags\\'")

(add-auto-mode 'java-mode
               ;; java
               "\\.aj\\'"
               ;; makefile
               "\\.ninja$")

(add-auto-mode 'sh-mode "\\.z?sh$")

(add-auto-mode 'cmake-mode
               "CMakeLists\\.txt\\'"
               "\\.cmake\\'")

;; pyim
;; (add-auto-mode 'text-mode "\\.pyim\\'")

;; arduino setup
;; (add-auto-mode 'c-mode "\\.\\(pde\\|ino\\)$")

;; objc-mode
;; (rx (or ".xm" ".x"))
(add-auto-mode 'objc-mode (regexp-opt '(".xm" ".x") t))

(provide 'init-file-type)
