
;; nyxt setup

(defun nyxt/make-qr-code-of-current-url ()
  "Open QR code of current url."
  (interactive)
  (if (not (file-exists-p "~/.quicklisp/setup.lisp"))
      (error "You cannot use this until you have Quicklisp installed! Check how to do that at: https://www.quicklisp.org/beta/#installation")
    (unless (slime-connected-p)
      (nyxt/start-and-connect))
    (nyxt/slime-repl-send-sexps
     '(ql:quickload "cl-qrencode")
     '(cl-qrencode:encode-png (quri:render-uri (url (current-buffer))) :fpath "/tmp/qrcode.png"))
    (find-file "/tmp/qrcode.png")
    (auto-revert-mode)))

(defun nyxt/slime-repl-send-sexps (&rest sexps)
  "Send SEXPS into the slime repl which assumed is connected to the nyxt repl."
  (unless (nyxt/connected-p)
    (error "You cannot use this until you have started NYXT and connected to its repl! Check out function: nyxt/start-and-connect"))
  (let ((slime-default-connection  nyxt/connection))
    (mapc (lambda (sexp) ;; (slime-net-send sexp)
            (slime-repl-send-string (format "%S" sexp)))
          sexps)
    'done))

(defvar nyxt/connection nil)
(defvar nyxt/connection-delay 1.0)

(defun nyxt/connected-p ()
  (and nyxt/connection (slime-connected-p)))

(defun nyxt/running-p ()
  (not (string-empty-p (shell-command-to-string "pgrep -fl nyxt"))))

(defun nyxt/start-and-connect (&optional maximize)
  "Start Nyxt with swank capabilities.
Optional argument MAXIMIZE would start nyxt in fullscreen."
  (interactive "P")
  (if (nyxt/connected-p)
      (message "Nyxt is connected!")
    ;; start nyxt and swank
    (unless (nyxt/running-p)
      (async-shell-command "nyxt --eval '(nyxt-user::start-swank)'"
                           "*nyxt*"))
    ;; make connection
    (cl-loop until (nyxt/connected-p)
             for i from 1
             do
             (sleep-for nyxt/connection-delay)
             (unless (setq nyxt/connection
                           (ignore-errors (slime-connect "localhost" "4006")))
               (message "%d connections failed" i)))
    ;; make sure GUI is running
    (nyxt/slime-repl-send-sexps '(nyxt-user::start))
    (when maximize
      (nyxt/slime-repl-send-sexps '(nyxt:toggle-fullscreen)))))

(defun my/browse-url-nyxt (url &optional new-window)
  (interactive "sURL: ")
  (unless (nyxt/connected-p) (nyxt/start-and-connect))
  (nyxt/slime-repl-send-sexps
   `(make-buffer-focus :url (quri.uri:uri ,url))))