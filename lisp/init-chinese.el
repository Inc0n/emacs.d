;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:

(require-package 'chinese-conv)
(with-eval-after-load 'chinese-conv
  (unless (executable-find "opencc")
    (message "install opencc for chinese-conv or check out other packages."))
  (setq chinese-conv-opencc-data "/opt/homebrew/share/opencc/"))

(defun chinese/fix-font ()
  (let ((chinese
         ;; "AR PL New Kai"
         ;; "AR PL UKai CN"
         ;; "WenQuanYi Zen Hei"
         ;; "I.Ming"
         ;; "DFFangSongW3-A"
         ;; "AaFangSong (Non-Commercial Use)"
         "STFangSong"
		 ;; "KaiTi SC"
         ;; "Hiragino Sans GB"
		 ;; "Songti SC"
		 ))
    (dolist (charset '(kana han ;; symbol, keep lambda pure
							cjk-misc
							bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family chinese
                                   ;; :size 16
                                   )))))
;; Contamination
;; (when (display-graphic-p) (chinese/fix-font))

(setq face-font-rescale-alist '(("Hiragino Sans GB" . 1.2)))

;; visual alignment, chinese and variable pitch font.
;; Doesn't work too well
(use-package valign
  :disabled
  :ensure t
  :defer t
  :config (setq valign-signal-parse-error t
                valign-fancy-bar nil)
  :init (add-hook 'org-mode-hook #'valign-mode))

;; {{ make IME compatible with evil-mode
(setq default-input-method "chinese-py")

(defun my/toggle-input-method (arg)
  "When input method is on, goto `evil-insert-state'.
ARG will allow selection of input method."
  (interactive "P")
  ;; evil-mode must be in insert mode to change IM
  (toggle-input-method (and (null current-input-method) arg))
  (if current-input-method
	  (message "IME on!")
	(message "IME off!")))
;; }}

;; {{ cal-china-x setup
(with-eval-after-load 'cal-china
  (setq calendar-chinese-celestial-stem
        ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"]
        calendar-chinese-terrestrial-branch
        ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"]))

(defun chinese-calendar (&optional args)
  "Open Chinese Lunar calendar.
Optional argument ARGS ."
  (interactive "P")
  (require 'cal-china-x)
  (let* ((diary-date-forms
          chinese-date-diary-pattern)

         ;; if chinese font width equals to twice of ascii font
         (calendar-day-header-array
          ["日" "一" "二" "三" "四" "五" "六"])
         (mark-holidays-in-calendar t)
         (cal-china-x-important-holidays cal-china-x-chinese-holidays)
         (cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
         (calendar-holidays
          (append calendar-holidays
                  cal-china-x-important-holidays
                  cal-china-x-general-holidays)))
    ;; (advice-add 'calendar-mark-holidays :around #'cal-china-x-mark-holidays)
    (calendar args)))
;; }}


;; {{ pyim
(require-package 'pyim)
(defvar my/pyim-directory "~/.eim"
  "The directory containing pyim dictionaries.")

(with-eval-after-load 'pyim
  ;; use western punctuation
  ;; (setq pyim-punctuation-dict nil)
  (setq pyim-page-length 8)

  (setq pyim-fuzzy-pinyin-alist
        '(("en" "eng")
		  ("in" "ing")))

  ;; pyim-bigdict is recommended (20M). There are many useless words in pyim-greatdict which also slows
  ;; down pyim performance
  ;; `curl -L http://tumashu.github.io/pyim-bigdict/pyim-bigdict.pyim.gz | zcat > ~/.eim/pyim-bigdict.pyim`

  ;; don's use shortcode2word
  (setq pyim-enable-shortcode nil)

  ;; use memory efficient pyim engine for pinyin ime
  (setq pyim-dcache-backend 'pyim-dregcache)

  ;; automatically load pinyin dictionaries "*.pyim" under "~/.eim/"
  ;; `directory-files-recursively' requires Emacs 25
  (when (file-exists-p my/pyim-directory)
    (when-let ((files (directory-files-recursively my/pyim-directory "\.pyim$")))
      (setq pyim-dicts
            (mapcar (lambda (f)
					  (list :name (file-name-base f) :file f))
                    files))
      ;; disable "basedict" if bigdict or greatdict is used
      (when (cl-notany (lambda (f)
						 (or (string= "pyim-another-dict" (file-name-base f))
							 (string= "pyim-bigdict" (file-name-base f))
							 (string= "pyim-greatdict" (file-name-base f))))
					   files)
		(pyim-basedict-enable))))

  ;; don't use tooltip
  (setq pyim-page-tooltip 'popup))
;; }}

(provide 'init-chinese)
;;; init-chinese.el ends here
