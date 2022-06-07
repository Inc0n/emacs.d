;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:

(require-package 'diredfl) ; font lock for `dired-mode'
(require-package 'find-by-pinyin-dired)

(defun diredext-exec-git-command-in-shell (command &optional arg file-list)
  "Run a shell command `git COMMAND`' on the marked files.
If no files marked, always operate on current line in dired-mode."
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "git command on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (unless (string-match "[*?][ \t]*\\'" command)
    (setq command (concat command " *")))
  (setq command (concat "git " command))
  (dired-do-shell-command command arg file-list)
  (message command))


(defun my/ediff-files ()
  "@see https://oremacs.com/2017/03/18/dired-ediff/.
Now use dired-diff under the hood."
  (interactive)
  (cl-letf (((symbol-function 'diff) 'ediff))
    (call-interactively #'dired-diff)))

(with-eval-after-load 'dired-aux
  (add-to-list 'dired-compress-file-suffixes
               '("\\.rar\\'" "" "7z x -aoa -o%o %i")))

(with-eval-after-load 'dired-x
  ;; (add-to-list 'dired-compress-files-alist '("\\.gz\\'" . "gzip -c9 %i > %o"))
  (setq dired-guess-shell-alist-user
        (mapcar (lambda (lst)
                  (list (concat "\\." (regexp-opt (cdr lst) t) "$")
                        (car lst)))
                `(("firefox --private-window"
                   "pdf" "dvi" "pdf.gz" "ps" "eps" "xhtml" "html" "htm" "mht")
                  ("7z x" "rar" "zip" "7z") ; "e" to extract, "x" to extract with full path
                  ("open"
                   "ogm"
                   "avi"
                   "mpg"
                   "rmvb"
                   "rm"
                   "flv"
                   "wmv"
                   "mkv" "mp4" "m4v"
                   "wav"
                   "webm"
                   "part"
                   "mov"
                   "3gp"
                   "crdownload"
                   "mp3")
                  ("open" "list" "pls")
                  ("qiv" "gif" "jpeg" "jpg" "tif" "png")
                  ("libreoffice" "doc" "docx" "xls" "xlsx" "odt")
                  ("djview" "djvu")))))

(defun compress-pdf (file-name)
  "Compress FILE-NAME, which should be a pdf."
  (interactive (list (thing-at-point 'filename)))
  (if (string= "pdf" (file-name-extension file-name))
      (let ((new-name (concat "compressed" file-name)))
        (when (file-exists-p new-name)
          (setq new-name (read-file-name "Enter name: ")))
        (async-shell-command
         "gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile=%s %s"
         new-name file-name))
    (user-error "File must be a pdf, %s" file-name)))

(with-eval-after-load 'files
  (when (eq system-type 'darwin)
    (unless (setq insert-directory-program (executable-find "gls"))
      (warn "Run `brew install coreutils' to enable functional dired"))))

;; https://www.emacswiki.org/emacs/EmacsSession which is easier to use
;; See `session-globals-regexp'
(with-eval-after-load 'dired
  (with-eval-after-load 'evil
    (evil-set-initial-state 'dired-mode 'emacs))
  ;; keep single dired buffer
  (setq dired-kill-when-opening-new-dired-buffer t)
  ;; search file name only when focus is over file
  (setq dired-isearch-filenames 'dwim)

  ;; @see http://blog.twonegatives.com/post/19292622546/dired-dwim-target-is-j00-j00-magic
  ;; when there is two dired buffer, Emacs will select another buffer
  ;; as target buffer (target for copying files, for example).
  ;; It's similar to windows commander.
  (setq dired-dwim-target t)

  (require 'dired-x)
  (require 'dired-aux)                  ; for `dired-dwim-target-directory'

  ;; @see https://emacs.stackexchange.com/questions/5649/sort-file-names-numbered-in-dired/5650#5650
  (setq dired-listing-switches "-laGh1vs")
  (setq dired-recursive-deletes 'top)

  ;; (diredfl-global-mode 1)
  (add-hook 'dired-mode-hook 'diredfl-mode)
  ;; avy, jump between texts, like easymotion in vim @see
  ;; http://emacsredux.com/blog/2015/07/19/ace-jump-mode-is-dead-long-live-avy/
  ;; for more tips
  (define-keys dired-mode-map
    ";" 'avy-goto-subword-1
    "e" 'my/ediff-files
    "h" [?^]                            ; was describe-mode
    "l" [return]                        ; was dired-do-redisplay
    "\\" 'diredext-exec-git-command-in-shell
    "/" 'dired-isearch-filenames)

  (add-hook 'dired-mode-hook
            (defun dired-mode-hook-setup ()
              "Set up dired."
              (dired-hide-details-mode 1))))

(provide 'init-dired)
;;; init-dired ends here
