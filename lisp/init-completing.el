;;; init-completing --- completions -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; My Emacs completing framework setup
;; - vertico + orderless + consult
;; - cape

;;; Code:

(use-package consult :ensure t
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
(use-package marginalia :ensure t
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
  (setq-default completion-in-region-function #'consult-completion-in-region)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle nil)
  ;; Disable dynamic resize, this is too jumpy
  (setq vertico-resize nil)		; 'grow-only
  ;; C-prefix is better since C-n and C-p, better ergonomics
  (define-keys vertico-map
    (kbd "C-o") 'embark-act		; embark intergration
    [C-return] #'vertico-exit-input)
  :init (vertico-mode 1))

(use-package embark :ensure t
  :config (define-key embark-file-map "f" 'my/browse-file))

;; more at
;; https://kristofferbalintona.me/posts/cape/
(use-package cape :ensure t
  :defer t
  :init
  (define-keys global-map
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
  ;; (cape-super-capf #'cape-ispell #'cape-dabbrev)
  
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package corfu :ensure t
  :config
  ;; reset this back to desired configuration
  (setq-default completion-in-region-function 'consult-completion-in-region)
  (add-hook 'corfu-mode-hook
	    (lambda () (kill-local-variable 'completion-in-region-function)))
  ;; No need for corfu-complete
  (define-keys corfu-map
	[remap completion-at-point] nil
	[?\M-d] #'corfu-doc-toggle
	[?\M-p] #'corfu-doc-scroll-down
	[?\M-n] #'corfu-doc-scroll-up)
  :init
  (use-package corfu-doc :defer t :ensure t)
  
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.1)
  (setq corfu-on-exact-match 'insert)
  (global-corfu-mode 1))

(use-package tempel :ensure t
  :config
  (define-keys tempel-map
    [?\C-n] 'tempel-next
    [?\C-p] 'tempel-previous
    [tab] 'tempel-next
    [shift-tab] 'tempel-previous)
  :init
  (defun tempel-create-new-template ()
	"Potentially create a new snippet quickly and save to templates file."
	(interactive)
	;; create a new temporary buffer
	;; get major mode
	;; prompt for template name
	;; define template on save
	;; MAYBE save template to config file
	'TODO)
  
  (setq tempel-path (my/emacs-d "tempel-templates.el"))
  (defun tempel-define-template (modes name exp)
    (pcase-let* ((modes (if (listp modes) modes
						  (list modes)))
				 (`(,mode-name ,plist . ,templates)
				  (--find (equal modes (car it)) tempel--path-templates))
				 (template
				  (assoc name templates)))
      (if template
		  (setf (cdr template) exp)
		(push (list modes nil (list name exp))
			  (cdr tempel--path-templates)))))

  (defun tempel-parse-template (string)
    (cl-labels
		((aux (string acc)
			  (if (string-empty-p string)
				  (nreverse acc)
				(if-let* ((index (cl-position ?$ string)))
					(cond ((= (aref string (1+ index)) ?\()
						   (pcase-let ((`(,obj . ,idx)
										(read-from-string string (1+ index))))
							 (message "%s %s" obj idx)
							 (aux (cl-subseq string idx)
								  (-cons* obj
										  (cl-subseq string 0 index)
										  acc))))
						  ((string-match "[rn]>\\|[prn>&%oq]"
										 string (1+ index))
						   (pcase-let ((`(,start ,end)
										(match-data 0)))
							 (aux (cl-subseq string end)
								  (if (= index 0)
									  (cons (intern
											 (cl-subseq string start end))
											acc)
									(-cons* (intern
											 (cl-subseq string start end))
											(cl-subseq string 0 index)
											acc)))))
						  (:else
						   (user-error "Invalid $ exp at %s %d %s"
									   string index
									   (cl-subseq string index))))
				  (nreverse (cons string acc))))))
      (aux string '())))

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
				(cons #'tempel-expand completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(setq completion-cycle-threshold 3)

(autoload 'pyim-cregexp-build "pyim")

(defvar my/enable-pinyin-in-completing-read nil)

(setq completion-styles '(fussy)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(defun match-pinyin (str)
    "A completion style for pinyin that builds on STR.
To avoid lag, it does not match if length is more than 2000."
    ;; orderless-regexp
    ;; Needs to be careful for the regexp to get too long.
    (let ((pinyin-regexp (pyim-cregexp-build str)))
      (if (<= (length pinyin-regexp) 2000)
          pinyin-regexp
        (message "%s is too long for pinyin" (length pinyin-regexp))
        str)))

(use-package orderless :ensure t
  :defer t
  :config
  (setq orderless-matching-styles
        '(orderless-literal
          orderless-regexp
          orderless-flex)))

;; it has other scoring/sorting backends such as fzf
(use-package fussy :ensure t :defer t
  :commands (fussy-all-completions)
  :config
  ;; fussy-fzf-native-score
  (defun fussy-filter-orderless (string table pred _point)
	"Match STRING to the entries in TABLE.

Use `orderless' for filtering by passing STRING, TABLE and PRED to
`orderless-filter'.  _POINT is not used."
	(when-let ((completions (orderless-filter string table pred)))
	  (pcase-let ((`(,prefix . ,pattern)
				   (orderless--prefix+pattern string table pred)))
		(list (orderless-highlight-matches pattern completions)
			  pattern prefix))))
  (setq fussy-filter-fn 'fussy-filter-orderless
		fussy-score-fn 'flx-score))

(use-package fzf-native
  :straight
  (:repo "dangduc/fzf-native"
   :host github
   :files (:defaults "*.c" "*.h" "*.txt"))
  :init
  (setq fzf-native-always-compile-module t)
  (setq fzf-native-module-cmake-args "-DCMAKE_C_FLAGS='-O3'")
  :config
  (fzf-native-load-own-build-dyn))

(defun completing-swiper ()
  "My swiper, which also can record macro at end of search."
  (interactive)
  (consult-line (util/thing-at-point/deselect))
  (completing--yank-search (car consult--line-history)))

(defun completing-ripgrep (init-input)
  "My version of ripgrep.
ARG can be used to control the behaviour of `consult-ripgrep'
A single `universal-argument' can disable preview.
Two `universal-argument' to change read a different directory to ripgrep."
  (interactive (list (if current-prefix-arg (util/thing-at-point/deselect) "")))
  (require 'consult)
  ;; (let ((default-directory default-directory))
  ;;   (when (consp arg)
  ;;     (setq default-directory
  ;;           (read-file-name
  ;;            "Directory: "
  ;;            default-directory nil nil nil #'file-directory-p))))
  (consult-ripgrep nil init-input)
  (completing--yank-search
   (string-trim-left (car consult--grep-history) "#")))

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
  (completing-packages
   nil
   (lambda (pkg)
     (string-match-p "-themes?$" (symbol-name pkg)))))

(defun completing--yank-search (regex-str)
  "Set search item as REGEX-STR in buffer using `isearch-mode'."
  (unless (stringp regex-str)
	(user-error "No a string, %s" regex-str))
  (re-search-forward regex-str (line-end-position) t)
  (isearch-mode t)
  (isearch-yank-string regex-str))

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

(provide 'init-completing)
;;; init-completing.el ends here
