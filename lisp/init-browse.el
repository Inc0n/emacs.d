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
  ;; (setq-local shr-width 90)
  (setq-local ;; fill-column 90
              visual-fill-column-center-text nil)
  (text-scale-set 1.5)
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
  (interactive nil eww-mode)
  (eww-readable)
  (setq-local visual-fill-column-center-text t))

(with-eval-after-load 'shr
  (setq shr-max-image-proportion 0.3))

(with-eval-after-load 'eww
  (advice-add 'eww-download-callback :around 'my/eww-download-using-title)
  (defun my/eww-download-using-title (orig-func status url dir)
	"Use a better filename, and selection of directory."
	(unless (plist-get status :error)
	
	  (let* ((dir eww-download-directory)
			 (name (or (with-current-buffer
						   (--find (with-current-buffer it
									 (and (derived-mode-p 'eww-mode)
										  (string= url
												   (plist-get eww-data :url))))
								   (buffer-list))
						 (concat (plist-get eww-data :title)
								 ".html"))
					   (user-error "Cannot current active eww buff")))
			 (file (eww-make-unique-file-name name
											  ;; (eww-decode-url-file-name)
											  dir)))

		(goto-char (point-min))
		(re-search-forward "\r?\n\r?\n")
		(let ((coding-system-for-write 'no-conversion))
		  (write-region (point) (point-max) file))
		(message "Saved %s" file))))

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
	;; "D" 'eww-toggle-paragraph-direction
    "U" 'eww)

  (defvar eww-home-page "https://lite.duckduckgo.com/")
  (defvar eww-search-default-engine "duckduckgo")
  (setq eww-download-directory "~/sources/browse-session/"))

(provide 'init-browse)
;;; init-browse.el ends here
