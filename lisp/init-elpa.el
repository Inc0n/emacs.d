;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

(require 'package)

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
;; (setq package-user-dir
;;       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
;;                         user-emacs-directory))
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

;; Some links to the Emacs China elpa mirror
;; '(("melpa" . "https://mirrors.163.com/elpa/melpa/")
;;   ("melpa-stable" . "https://mirrors.163.com/elpa/melpa-stable/")
;;   ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;   ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/"))

(when (and (not noninteractive)         ; no popup in batch mode
           (not (file-exists-p (file-truename package-user-dir)))
           (yes-or-no-p "Switch to faster package repositories in  temporarily?
You still need modify `package-archives' in \"init-elpa.el\" to PERMANENTLY use this ELPA mirror."))
  (setq package-archives
        '(("melpa" . "https://mirrors.163.com/elpa/melpa/")
          ("melpa-stable" . "https://mirrors.163.com/elpa/melpa-stable/"))))

;;;

;; (defun package-delete-by-name (name)
;;   (let ((pkg (assoc name package-alist)))
;;     (when pkg
;;       (package-delete (cadr pkg)))))

(defun local-require (pkg &optional force)
  "Require PKG in site-lisp directory if not load, or by FORCE."
  (or (and (not force)
           (featurep pkg))
      (let* ((pkg (symbol-name pkg))
             (path (expand-file-name pkg my/site-lisp-dir))
             (load-path (cons path load-path)))
        (if (file-exists-p path)
            (load (expand-file-name pkg path) t nil)
	      (load (file-truename path) t nil)))
      (message "cannot find package %s" pkg)))

;;; Bootstrap straight
(defvar bootstrap-version)
(defvar straight-use-package-by-default nil)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE for MIN-VERSION, NO-REFRESH."
  (or (featurep package)
      (package-installed-p package min-version)
      (if (or (assoc package package-archive-contents)
              no-refresh)
          (package-install package)
        ;; We don't want to refresh contents because it takes forever
        ;; (package-refresh-contents)
        ;; (require-package package min-version t)
        ;; Instead let's just throw a warning
        (warn (format "Package not in archive in `require-package': %s" package)))))

;; List of visible packages from melpa-unstable (http://melpa.org).
;; Please add the package name into `my/melpa-include-packages'
;; if it's not visible after `list-packages'.
(defvar my/melpa-include-packages
  '(ace-window               ; lastest stable is released on year 2014
    ace-pinyin
    
    nov
    nov-xwidget
    bbdb
    native-complete
    company-native-complete
    js2-mode           ; need new features
    git-timemachine    ; stable version is broken when git rename file
    
    command-log-mode
    ;; lsp-mode ; stable version has performance issue, but unstable version sends too many warnings
    edit-server ; use Emacs to edit textarea in browser, need browser addon
    package-lint                        ; for melpa pull request only
    typescript-mode ; the stable version lacks important feature (highlight function names)

    rjsx-mode                          ; fixed the indent issue in jsx
    julia-mode

    cdlatex
    auctex

    ibuffer-vc
    which-key

    js-doc
    wgrep

    company                  ; I won't wait another 2 years for stable
    simple-httpd
    ;; findr
    mwe-log-commands
    db
    web
    legalese
    htmlize
    pyim-basedict
    pyim-wbdict
    scratch
    session
    inflections
    lua-mode
    pomodoro
    packed
    keyfreq
    gitconfig-mode
    w3m
    zoutline
    company-c-headers
    company-statistics
    ;; toggle latex fragment
    org-fragtog
    use-package
    vterm
    maple-minibuffer
    ;; osx
    haxe-mode
    applescript-mode
    kotlin-mode
    gradle-mode
    lsp-grammarly
    grip-mode
    ;;
    jupyter
    cal-china-x
    face-up ;; for racket mode
    racket-mode
    undo-hl
    stimmung-themes)
  "Packages to install from melpa-unstable.")

;; (defvar my/melpa-stable-banned-packages nil "Banned packages from melpa-stable.")

;;; Packages

(require-package 'use-package)

(require-package 'async)

(require-package 'popup) ; some old package need it
;; (require-package 'fringe-helper)
(require-package 'unfill)

(require-package 'request)
;; (require-package 'jump)
;; (require-package 'findr)
;; (require-package 'nvm)
(require-package 'htmlize)
(require-package 'scratch)
(require-package 'git-timemachine)

(require-package 'command-log-mode)

(require-package 'cpputils-cmake)
(require-package 'bbdb)

;; rvm-open-gem to get gem's code
(require-package 'rvm)
;; C-x r l to list bookmarks
(require-package 'git-link)

(require-package 'legalese)
;; (require-package 'git-gutter) ; use my patched version

;; {{ Fixed expiring GNU ELPA keys
;; GNU ELPA GPG key will expire on Sep-2019. So we need install this package to
;; update key or else users can't install packages from GNU ELPA.
;; @see https://www.reddit.com/r/emacs/comments/bn6k1y/updating_gnu_elpa_keys/
;; BTW, this setup uses MELPA only. So GNU ELPA GPG key is not used.
(require-package 'gnu-elpa-keyring-update)
;; }}

(provide 'init-elpa)
;;; init-elpa.el ends here
