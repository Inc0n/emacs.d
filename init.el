;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur
(setq debug-on-error t)

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------

(setq garbage-collection-messages t) ; for debug
(setq gc-cons-percentage 0.5)
;; setting the initial gc-cons-threshold to a large value to prevent lots of GC
(setq gc-cons-threshold (* 128 1024 1024)) ;; 128mb

(defvar my/normal-gc-cons-threshold (* 24 1024 1024))

(defvar emacs-29? (version<= "29.0" emacs-version))

(add-hook 'emacs-startup-hook
          (lambda ()
			(set-face-attribute 'default nil :height 135)
            ;; reset the gc-cons-threshold back to a smaller value
            (setq gc-cons-threshold my/normal-gc-cons-threshold)
            (setq gc-cons-percentage 0.3)
            (message "startup time: %s, gcs-done=%d"
                     (emacs-init-time) gcs-done)
			(setq custom-file (my/emacs-d "custom.el"))
			(when (file-exists-p custom-file)
			  (load custom-file))))

;;----------------------------------------------------------------------------
;; Load other lisp configs
;;----------------------------------------------------------------------------
(defsubst my/emacs-d (path)
  "Get the expanded PATH under .emacs.d."
  (expand-file-name path user-emacs-directory))

(defvar my/site-lisp-dir (my/emacs-d "site-lisp")
  "My site directory.")

;; font setting after window-system is loaded
;; (when (x-list-fonts "Anonymous Pro-13")
;;   (set-frame-font "Anonymous Pro-13" nil t))

;; @see https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; Normally file-name-handler-alist is set to
;; '(("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;;   ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;;   ("\\`/:" . file-name-non-special))
;; Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.

(add-to-list 'load-path (my/emacs-d "lisp"))

(let ((file-name-handler-alist nil))
  ;; `package-initialize' takes 35% of startup time
  ;; need check https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast for solution
  (require 'init-elpa)
  (require 'init-utils)
  (require 'init-essential)
  (require 'init-file-type)
  ;; Any file use flyspell should be initialized after
  ;; init-spelling.el
  (require 'init-spelling)
  (require 'init-ibuffer)
  (require 'init-completing)

  (require 'init-chinese)
  (require 'init-keyfreq)

  (require 'init-ui)
  (require 'init-writting)
  (require 'init-misc)
  (require 'init-dired)
  (require 'init-bindings)
  (require 'init-repeat)
  ;; ediff configuration should be last so it can override
  ;; the key bindings in previous configuration
  ;; (require 'init-ediff)

  ;; language
  (require 'init-org)
  (require 'init-text)
  (require 'init-prog-modes)
  ;; (require 'init-osx)
  (require 'init-lisp)
  ;; (require 'init-haskell)
  ;;
  (require 'init-git)
  (require 'init-shell)
  ;; (require 'init-browse)

  ;; @see https://github.com/hlissner/doom-emacs/wiki/FAQ
  ;; Adding directories under "site-lisp/" to `load-path' slows
  ;; down all `require' statement. So we do this at the end of startup
  ;; NO ELPA package is dependent on "site-lisp/".
  (let ((default-directory "~/.emacs.d/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path)))

(add-to-list 'load-path my/site-lisp-dir)

;;; Local Variables:
;;; no-byte-compile: t
;;; End:
(put 'erase-buffer 'disabled t)
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)
