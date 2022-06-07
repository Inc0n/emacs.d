;;; init-completing --- completing-read -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; My Emacs completing framework setup
;; - vertico + orderless + consult
;; - cape

;;; Code:

(use-package consult
  :ensure t
  :defer t
  :config
  (setq consult-line-start-from-top nil)
  (consult-customize
   consult-theme
   consult-buffer
   consult-grep
   :preview-key '(:debounce 0.3 any)
   consult-ripgrep consult-git-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key (kbd "M-."))
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

;; provide annotations in minibuffer
(use-package marginalia
  :ensure t
  :defer 1
  :hook (after-init . marginalia-mode)
  :config
  ;; add notation for variables values
  (defun marginalia-annotate-command-maybe-mode (cand)
    "Annotate command CAND with its documentation string.
Similar to `marginalia-annotate-symbol', but does not show symbol class."
    (when-let (sym (intern-soft cand))
      (concat
       (marginalia--field
        (cond ((not (string-match "-mode$" cand))
               (marginalia-annotate-binding cand))
              ((boundp sym)
               (concat " " (marginalia--variable-value sym)))
              (:else (if (eq major-mode sym)
                         " on" " off")))
        :face 'marginalia-type)
       (marginalia--documentation (marginalia--function-doc sym)))))

  (add-to-list 'marginalia-annotator-registry
               '(command marginalia-annotate-command-maybe-mode)))

;; Vertico seems to lag when dealing with a very long list
;; such as while in describe-functions
(use-package vertico :ensure t
  :defer t
  :config
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if (or vertico-mode icomplete-mode)
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle nil)
  ;; Disable dynamic resize, this is too jumpy
  (setq vertico-resize nil)			   ; 'grow-only
  ;; C-prefix is better since C-n and C-p, better ergonomics
  (define-key vertico-map (kbd "C-o") 'embark-act) ; embark intergration
  (define-key vertico-map [C-return] #'vertico-exit-input)
  :init (vertico-mode 1))

(use-package embark :ensure t
  :config (define-key embark-file-map "f" 'my/browse-file))

(use-package icomplete
  ;; downside, narrow Emacs, would not resize minibuffer appropriately.
  ;; un-configurable keymap
  :disabled
  :config
  (setq icomplete-scroll t              ; scroll instead rotate
        icomplete-max-delay-chars 0
        icomplete-show-matches-on-no-input t)
  ;; unable to find the keymap where icomplete-ret is set, so replace it instead
  (fset 'icomplete-ret 'icomplete-force-complete-and-exit)
  ;; (define-keys icomplete-vertical-mode-minibuffer-map
  ;;   #'icomplete-ret
  ;;   [return] #'minibuffer-complete-and-exit)
  :init
  (icomplete-mode 1)
  (icomplete-vertical-mode 1))

;; more at
;; https://kristofferbalintona.me/posts/cape/
(use-package cape :ensure t
  :defer t
  :init
  (define-keys global-map
    (kbd "C-c p p") 'completion-at-point ;; capf
    (kbd "C-c p t") 'complete-tag        ;; etags
    (kbd "C-c p d") 'cape-dabbrev        ;; or dabbrev-completion
    (kbd "C-c p f") 'cape-file
    (kbd "C-c p k") 'cape-keyword
    (kbd "C-c p s") 'cape-symbol
    (kbd "C-c p a") 'cape-abbrev
    (kbd "C-c p i") 'cape-ispell
    (kbd "C-c p l") 'cape-line
    (kbd "C-c p w") 'cape-dict
    (kbd "C-c p \\") 'cape-tex
    (kbd "C-c p _") 'cape-tex
    (kbd "C-c p ^") 'cape-tex
    (kbd "C-c p &") 'cape-sgml
    (kbd "C-c p r") 'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
  ;; (cape-super-capf #'cape-ispell #'cape-dabbrev)
  
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package corfu :ensure t
  :disabled
  :init
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.1)
  (corfu-global-mode 1))

(setq completion-cycle-threshold 3)

(autoload 'pyim-cregexp-build "pyim")

(defvar my/enable-pinyin-in-completing-read nil)

(defun toggle-pinyin-in-completing-read ()
  "Toggle variable `my/enable-pinyin-in-completing-read'."
  (interactive)
  (message "pinyin in completing read: %s"
           (setq my/enable-pinyin-in-completing-read
                 (not my/enable-pinyin-in-completing-read))))

(use-package orderless :ensure t
  :defer t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  :config
  (setq orderless-matching-styles
        '(orderless-literal
          orderless-regexp
          match-flex-or-pinyin))
  (defun match-flex-or-pinyin (str)
    "Add pinyin search support to orderless, builds STR.
Otherwise, uses orderless-flex. Avoiding computation lag."
    ;; orderless-regexp
    ;; Needs to be careful for the regexp to get too long.
    (if my/enable-pinyin-in-completing-read
        (let ((pinyin-regexp (pyim-cregexp-build str)))
          (if (<= (length pinyin-regexp) 2000)
              pinyin-regexp
            (message "%s is too long for pinyin" (length pinyin-regexp))
            str))
      (orderless-flex str))))

(use-package dash-docs :ensure t)

;; (lookup-key vertico-map (kbd "M-q"))
(defun completing--replace-search (search)
  "Replace the SEARCH with string from `read-string'."
  (if (string-empty-p search)
      (user-error "Enter some text first!")
    (let ((to-string (read-string "To: "))
          (buf (seq-find (lambda (x) (not (minibufferp x)))
			             (buffer-list))))
      (with-selected-window (get-buffer-window buf)
        (with-current-buffer buf
	      (query-replace-regexp search to-string)
	      (exit-minibuffer))))))

(defun completing-swiper ()
  "My swiper, which also can record macro at end of search."
  (interactive)
  (consult-line (util/thing-at-point/deselect))
  (completing--yank-search (car consult--line-history))
  (evil-record-macro ?0))

(defun completing-ripgrep (init-input arg)
  "My version of ripgrep.
ARG can be used to control the behaviour of `consult-ripgrep'
A single `universal-argument' can disable preview.
Two `universal-argument' to change read a different directory to ripgrep."
  (interactive (list (util/thing-at-point/deselect) current-prefix-arg))
  (require 'consult)
  ;; (let ((default-directory default-directory))
  ;;   (when (consp arg)
  ;;     (setq default-directory
  ;;           (read-file-name
  ;;            "Directory: "
  ;;            default-directory nil nil nil #'file-directory-p))))
  (consult-ripgrep nil init-input)
  (completing--yank-search (car consult--grep-history)))

(defun completing-ripgrep-backups ()
  "Ripgrep search the backups of this current buffer file."
  (interactive)
  (if (cl-every 'file-exists-p (find-backup-file-name buffer-file-name))
      (let* ((backup-file (make-backup-file-name-1 buffer-file-name))
             (initial "")
             (consult-ripgrep-args
              (format "%s -g \"%s*\""
                      consult-ripgrep-args
                      (replace-regexp-in-string
                       "\!" "\\\\!"
                       (file-name-nondirectory backup-file)))))
        (message consult-ripgrep-args)
        (consult-ripgrep (file-name-directory backup-file)
                         initial))
    (user-error "No backup files for %s to search" buffer-file-name)))
;;

(defun my/git-recent-files ()
  "Get files in my recent git commits."
  (let* ((default-directory (my/git-root-dir))
         ;; two weeks is a sprint, minus weekend and days for sprint review and test
         (cmd (format "git --no-pager log --name-only --since=\"30 days ago\" --pretty=format: | sort | uniq | awk NF"))
         (lines (util/shell-command-to-lines cmd)))
    (mapcar #'abbreviate-file-name lines)))

(defun completing-imenu-comments ()
  "Imenu comments."
  ;; TODO: 2022-01-04 this imenu command shows old imenu entries.
  (interactive)
  (let* ((imenu-auto-rescan t)
         (imenu-generic-expression
          `(("Comments"
             ,(rx line-start
                  (= 3 (eval comment-start))
                  (or (optional " ")
                      ;; (regexp (format "[^%s]+" comment-start))
                      (not (syntax comment-start)))
                  (group (* anything))
                  line-end)
             ;; (format "^;\\{3\\}\\([^%s]+.*\\)$" comment-start)
             1))))
    (consult-imenu)))

(defun completing-packages (arg &optional filter)
 "List packages, or package archive if ARG non-nil.
use FILTER predicate to filter desired packages to see."
  (interactive (list current-prefix-arg))
  (let* ((packages
          (if arg ; use archive instead
              (mapcar #'car package-archive-contents)
            (append (mapcar #'car package-alist)
                    (mapcar #'car package--builtins))))
         (packages (if (and filter (functionp filter))
                       (-filter filter packages)
                     packages))
         (package (completing-read "Package: " packages)))
    (describe-package (intern package))))

(defun list-installed-themes ()
  "List all installed themes."
  (interactive)
  (selectsel-list-packages
   nil (lambda (pkg)
         (string-match-p "-themes?$" (symbol-name pkg)))))

(defun completing--yank-search (regex-str)
  "Set search item as str.
Argument REGEX-STR the regex str to find in buffer."
  (when regex-str
    (re-search-forward regex-str (line-end-position) t)
    (if (bound-and-true-p evil-mode)
        (when (save-excursion
                (evil-search regex-str t t (line-beginning-position)))
          ;; this would make cursor at beginning of selection
          (evil-search-previous))
      (isearch-mode t)
      (isearch-yank-string regex-str))))

;;; Experiments

(defun keymap-completion-candidates (keymap)
  (cl-loop for (key . bind) in (cdr keymap)
           for key = (cond ((numberp key) (key-description (vector key)))
                           ;; take care of 'remap
                           ((symbolp key) (if (eq key 'remap)
                                              (format "%s %s" key bind)
                                            (format "%s" key)))
                           (:else key))
           for bind = (if (and (listp bind) (symbolp (car bind)))
                          ;; take care of 'keymap
                          (if (eq (car bind) 'keymap)
                              ;; (mapcar (lambda (str) ))
                              (keymap-completion-candidates (cdr bind))
                            (user-error "Unexpected %s" bind))
                        bind)
           collect (cons key bind)))

;; (format "%s" (keymap-completion-candidates evil-insert-state-map))

;; selectrum evil mark
;; @see https://github.com/raxod502/selectrum/wiki/Useful-Commands#evil-marks

(provide 'init-completing)
;;; init-completing ends here
