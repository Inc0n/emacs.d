
;; my buffer style popup window

(defvar my-key--buffer nil)
(defvar my-key-buffer-name "*my-key*")

(defun my-key--init-buffer ()
  (unless (buffer-live-p my-key--buffer)
    (setq my-key--buffer (get-buffer-create my-key-buffer-name))
    (with-current-buffer my-key--buffer
      ;; suppress confusing minibuffer message
      (let (message-log-max)
        (toggle-truncate-lines 1)
        (message ""))
      (display-line-numbers-mode -1)
      (setq-local cursor-type nil
                  cursor-in-non-selected-windows nil
                  mode-line-format nil
                  word-wrap nil
                  show-trailing-whitespace nil)
      ;; (run-hooks 'my-key-init-buffer-hook)
      )))

(defun my-key--show-popup (height width)
  "Using dimension of HEIGHT and WIDTH to show the popup."
  (when (and (> height 0) (> width 0))
    (let ((alist
           `((window-width .  ,(which-key--text-width-to-total width))
             (window-height . ,height)
             (side . bottom)
             (slot . 0))))
      ;; Previously used `display-buffer-in-major-side-window' here, but
      ;; apparently that is meant to be an internal function. See emacs bug #24828
      ;; and advice given there.
      (cond ((get-buffer-window my-key--buffer)
             (display-buffer-reuse-window my-key--buffer alist))
            (t
             (display-buffer-in-side-window my-key--buffer alist))))))

(defun my-key--show-page (msg)
  "Show current page filled with MSG."
  (my-key--init-buffer) ;; in case it was killed
  (let (;; (page-echo (which-key--process-page which-key--pages-obj))
        (height 10)
        (width (window-size (frame-root-window) t))
        ;; disable golden-ratio for messing with the size configuration
        (golden-ratio-mode nil))
    (with-current-buffer my-key--buffer
      (erase-buffer)
      (insert msg)
      (goto-char (point-min)))
      ;; (when (cdr page-echo) (funcall (cdr page-echo)))
      ;; which-key--show-popup
    (my-key--show-popup height width)))

(defun my-key--hide-popup ()
  "Hide which-key buffer when side-window popup is used."
  (when (buffer-live-p my-key--buffer)
    ;; in case which-key buffer was shown in an existing window, `quit-window'
    ;; will re-show the previous buffer, instead of closing the window
    (quit-windows-on my-key--buffer)))

(cl-defun ask-action-on (prompt actions-list &key target on-exit)
  "`ask-action-on' takes ACTIONS-LIST a list of (char PROMPT action), that could optionally act on TARGET with the corresponding action."
  (let ((echo-keystrokes nil)
        (menu-prompt
         (format " Actions for: %s\n\n%s\n" prompt
                 (mapconcat (lambda (p)
		    		          (cl-destructuring-bind (key prompt action) p
                                (unless action
                                  (error "No action specified for %s" p))
                                (unless (characterp key)
                                  (error "Key is not a character %s" p))
		    			        (format "  [%c] %s" key prompt)))
		    		        actions-list "\n")))
        (on-exit (if (and on-exit (functionp on-exit))
                     (lambda ()
                       (funcall on-exit)
                       (my-key--hide-popup))
                   'my-key--hide-popup)))
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map [?\C-g] 'abort-recursive-edit)
       (dolist (actions actions-list)
         (define-key
           map
           (vector (car actions))
           (caddr actions)))
       map)
     t on-exit)
    (my-key--show-page menu-prompt)))

;; test
(lambda () (interactive)
  (ask-action-on "Prompt"
                 '((?d "delete file" delete-file)
				   (?r "rename file" rename-file)
				   (?f "find file"  find-file)
                   (?q "quit"       quit))))