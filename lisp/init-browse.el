;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'browse-url
  (setq browse-url-browser-function #'browse-url-generic)

  (if (eq system-type 'darwin)
      (setq browse-url-handlers '(("\\`file:" . browse-url-generic))
            browse-url-generic-program "open"
            browse-url-generic-args '())
    (setq browse-url-handlers '(("\\`file:" . browse-url-firefox))
          browse-url-generic-program "firefox"
          browse-url-generic-args '("--private-window")))
  (setq browse-url-firefox-arguments '("--private-window")))

(defun my/browse-file (filename)
  "Open FILENAME and open it in browser.
Prefix ARG will prompt for a file name in current directory."
  (interactive (list (read-file-name
  		      "Find file literally: " nil default-directory)))
  (browse-url-generic
   ;; using file-truename, to make osx happy
   (file-truename filename))
  (when (eq window-system 'x)
    (shell-command "swaymsg [app_id=firefox] focus" nil)))

;;; eww

(with-eval-after-load 'evil
  (evil-set-initial-state 'eww-mode 'emacs)
  (evil-set-initial-state 'eww-buffer-mode 'emacs))

(define-hook-setup 'eww-after-render-hook
  "My eww hook setup."
  (let* ((title (plist-get eww-data :title))
         (title (if (string-empty-p title)
                    "untitled"
                  (if (> (length title) 45)
                      (concat (substring title 0 45) "...")
                    title))))
    (rename-buffer (format "*eww: %s*" title)
                   t)))

(define-hook-setup 'eww-mode-hook
  (setq-local truncate-lines t)
  (setq-local shr-width 90)
  (setq-local fill-column 90
              visual-fill-column-center-text nil)
  (visual-line-mode 1)
  (visual-fill-column-mode 1))

(defun eww-edit-url-and-goto ()
  (interactive)
  (eww (read-from-minibuffer "URL: " (plist-get eww-data :url))))

(defvar eww-search-engine-alist
  '(("stackoverflow" "https://www.stackoverflow.com/search?q=%s" utf-8)
    ("duckduckgo"    "https://lite.duckduckgo.com/lite?q=%s")
    ("financial"     "https://financial-dictionary.thefreedictionary.com/%s" utf-8)
    ("dictionary"    "https://dictionary.reference.com/search?q=%s" utf-8))
  "My eww search engine alist similar to w3m.")

(defun my/eww-search (&optional arg)
  "My eww that can search with search engines.
Non-nil ARG will allow search engine selection."
  (interactive "P")
  (require 'eww)
  (let ((engine (if arg
                    (completing-read "Engine: " (mapcar 'car eww-search-engine-alist))
                  eww-search-default-engine)))
    (if-let* ((pair (assoc engine eww-search-engine-alist))
              (url (cadr pair)))
        (eww
         (format url
                 (read-from-minibuffer
                  (format "Search with %s: " engine))))
      (message "cannot find engine %s" engine))))

(defun my/eww-readable ()
  (interactive)
  (eww-readable)
  (setq-local visual-fill-column-center-text t))

(with-eval-after-load 'shr
  (setq shr-max-image-proportion 0.3))

(with-eval-after-load 'eww
  (define-keys eww-mode-map
    ;; "C-c b" 'w3m-external-view-this-url
    ;; "f" 'my/eww-search
    "j" 'next-line
    "h" 'eww-list-histories
    "R" 'my/eww-readable
    "H" 'eww-back-url
    "L" 'eww-forward-url
    "l" 'my/eww-search
    "k" 'previous-line
    "J" 'scroll-up-command
    "K" 'scroll-down-command
    "U" 'eww)
  (defvar eww-home-page "https://lite.duckduckgo.com/")
  (defvar eww-search-default-engine "duckduckgo"))

(provide 'init-browse)
;;; init-browse.el ends here