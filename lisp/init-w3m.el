
;;; w3m

(require-package 'w3m)

;; (defvar w3m-display-callback nil
;;   "a function with no arguements to be called in `w3m-display-hook-setup'")
(defvar w3m-global-keyword nil
  "`w3m-display-hook' must search current buffer with this keyword twice if not nil.")

(defun w3m-guess-keyword (&optional encode-space-with-plus)
  (let* ((keyword (util/use-selected-string-or-ask "Enter keyword"))
         (encoded-keyword (w3m-url-encode-string (setq w3m-global-keyword keyword))))
    ;; some search requires plus sign to replace space
    (if encode-space-with-plus
        (replace-regexp-in-string "%20" " " encoded-keyword)
      encoded-keyword)))

(autoload 'w3m-search "w3m")
(defun w3m-search-api (engine)
  (interactive (list (completing-read "Search: " w3m-search-engine-alist)))
  (w3m-search engine (w3m-guess-keyword)))

(defun w3mext-open-link-or-image-or-url ()
  "Opens the current link or image or current page's uri or any url-like text under cursor in firefox."
  (interactive)
  (when (derived-mode-p 'w3m-mode 'gnus-article-mode)
    (let ((url (or (w3m-anchor)
		   (and (string= url "buffer://")
			(or (w3m-image) w3m-current-url))
                   (car (browse-url-interactive-arg "URL: ")))))
      (browse-url-generic url))))

(defun w3mext-open-with-mplayer ()
  (interactive)
  (when (derived-mode-p 'w3m-mode 'gnus-article-mode)
    ;; weird, `w3m-anchor' fail to extract url while `w3m-image' can
    (when-let*
        ((url (or (w3m-anchor)
                  (w3m-image)
		  (save-excursion
		    (goto-char (point-min))
                    (when (re-search-forward "^Archived-at: <?\\([^ <>]*\\)>?")
                      (buffer-substring-no-properties
                       (match-beginning 1)
                       (match-end 1))))))
	 (url (url-encode-url url)))
      (shell-command
       (if (string= url "buffer://")
	   ;; cache 2M data and don't block UI
	   (format "curl -L %s | qiv -F - &" url)
	 (format "mplayer -quiet -stop-xscreensaver -cache 2000 %s &"
                 url))))))

(defun w3mext-subject-to-target-filename ()
  (--> (buffer-string)
       (and (string-match "^Subject: \\(.+\\)" it)
	    (match-string 1 it))
       ;; clean the timestamp at the end of subject
       (replace-regexp-in-string "[ 0-9_.'/-]+$" "" it)
       (replace-regexp-in-string "'s " " " it)
       (replace-regexp-in-string "[ ,_'/-]+" "-" it)))

(defun w3mext-download-rss-stream ()
  (interactive)
  (when (derived-mode-p 'w3m-mode 'gnus-article-mode)
    (let ((url (w3m-anchor)))
      (if (or (null url)
	      (string= url "buffer://"))
          (message "This link is not video/audio stream.")
        (shell-command
         (format "curl -L %s > %s.%s"
		 url
		 (w3mext-subject-to-target-filename)
		 (file-name-extension url)))
        (message "downloaded stream")))))

(defun w3m-display-hook-setup (url)
  (let ((title
         (or w3m-current-title url)))
    (when w3m-global-keyword
      ;; search keyword twice, first is url, second is your input,
      ;; third is actual result
      (goto-char (point-min))
      (search-forward-regexp
       (replace-regexp-in-string " " ".*" w3m-global-keyword)
       (point-max) t 3)
      ;; move the cursor to the beginning of word
      (backward-char (length w3m-global-keyword))
      ;; cleanup for next search
      (setq w3m-global-keyword nil))
    ;; rename w3m buffer
    (rename-buffer
     (format "*w3m: %s*"
             (substring title 0 (min 50 (length title))))
     t)))
(add-hook 'w3m-display-hook 'w3m-display-hook-setup)

(util:define-hook-setup 'w3m-mode-hook
  (w3m-lnum-mode 1)
  (local-set-key (kbd "RET") 'w3m-view-this-url)
  (setq-local truncate-lines nil
              display-line-numbers nil))

(defun w3m-edit-url-and-goto ()
  (interactive)
  (w3m-goto-url (read-from-minibuffer "URL: " w3m-current-url)))

;; (setq w3m-search-engine-alist nil)

(with-eval-after-load 'w3m
  (util:define-keys 'w3m-mode-map
    (kbd "C-c b") 'w3m-external-view-this-url
    "J" 'w3m-scroll-up-or-next-url
    "K" 'w3m-scroll-down-or-previous-url)
  (setq w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        ;; emacs-w3m will test the ImageMagick support for png32
        ;; and create files named "png32:-" everywhere
        w3m-imagick-convert-program (executable-find "convert")
        w3m-terminal-coding-system 'utf-8
        w3m-use-cookies nil
        ;; w3m-cookie-accept-bad-cookies t
        w3m-home-page "https://lite.duckduckgo.com/"
        ;; w3m-command-arguments       '("-F" "-cookie")
        ;; w3m-mailto-url-function     'compose-mail
        ;; use shr to view html mail which is dependent on libxml
        ;; I prefer w3m. That's emacs 24.3+ default setup.
        ;; If you prefer colored mail body and other advanced features,
        ;; you can either comment out below line and let Emacs decide the
        ;; best html mail rendering engine, or "(setq mm-text-html-renderer 'shr)"
        ;; in "~/.gnus.el"
        ;; mm-text-html-renderer 'w3m ; I prefer w3m
        w3m-use-toolbar nil
        ;; show images in the browser
        ;; setq w3m-default-display-inline-images t
        w3m-display-mode 'dedicated-frames
        ;; w3m-use-tab nil
        ;; w3m-use-tab-line nil
        ;; w3m-confirm-leaving-secure-page nil
        w3m-search-default-engine "duckduckgo"
        w3m-key-binding 'info
        w3m-favicon-cache-expire-wait 86400 ;; 1 day cache expiration
        w3m-use-favicon t)

  ;; C-u S g RET <search term> RET in w3m
  (add-to-list/s
   'w3m-search-engine-alist
   '(("stackoverflow" "https://www.stackoverflow.com/search?q=%s" utf-8)
     ("duckduckgo"    "https://www.duckduckgo.com/?q=%s")
     ("dictionary"    "https://dictionary.reference.com/search?q=%s" utf-8)
     ("financial"     "https://financial-dictionary.thefreedictionary.com/%s" utf-8))))

(provide 'init-w3m)
;;; init-w3m.el ends here
