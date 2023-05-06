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

(when t
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
    (load bootstrap-file nil 'nomessage)))

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

;;; Packages

(require-package 'use-package)

(require-package 'async)

(require-package 'unfill)

;; (require-package 'jump)
;; (require-package 'findr)
;; (require-package 'nvm)
(require-package 'htmlize)
(require-package 'scratch)
(require-package 'git-timemachine)

(require-package 'command-log-mode)
(setq-default command-log-mode-key-binding-open-log nil)

(require-package 'cpputils-cmake)
(require-package 'bbdb)

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
