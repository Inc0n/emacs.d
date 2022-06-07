;; -*- lexical-binding: t -*-

(require 'dom) ; dom manipulation
(require 'xml) ; parse html
(require 'request) ; url request

(defvar scrap-modules '()
  "Modules (site) that are implemented for scrapping.")

(defun scrap-request-html (url handler)
  (request
   url
   ;; parser implemented with reference to `json-read'
   :parser (lambda () (libxml-parse-html-region (point) (point-max)))
   :success handler))

(defun scrap-find-module (url)
  (-some--> (--find (string-match (car it) url)
                    scrap-modules)
    (cdr it)))

;;;###autoload
(defun scrap-interface (url)
  (interactive "sUrl: ")
  (if-let ((handler (scrap-find-module url)))
      (scrap-request-html url handler)
    (message "Url %s not supported" url)))

;; (defun scrap-zhonghuadiancang (url)
;;   "Scrap the book URL of 中华典藏.
;; And write it to file named '<title>.org'."
;;   (interactive "sUrl: ")
;;   (message "scrapping %s" url)
;;   (scrap-request-html
;;    url
;;    (cl-function
;;     (lambda (&key data &allow-other-keys)
;;       (message "html parsed ok")
;;       (let ((title
;;              (dom-texts (dom-by-class (car (dom-by-tag data 'ol)) "active")))
;;             (chapters
;;              (dom-by-tag (dom-by-id data "booklist") 'a)))
;;         (with-temp-file (concat title ".org")
;;           (insert "#+title: " title "\n"))
;;         (dolist (chapter chapters)
;;           (scrap-zhonghuadiancang-chapter
;;            (dom-attr chapter 'href)
;;            (concat title ".org")))
;;         (message "written to %s" (concat title ".org"))
;;         )))))

;; (scrap-zhonghuadiancang
;;  "https://www.zhonghuadiancang.com/xuanxuewushu/taiyijinhuazongzhi/")

;; (scrap-zhonghuadiancang-chapter
;;  "https://www.zhonghuadiancang.com/xuanxuewushu/taiyijinhuazongzhi/41583.html"
;;  'message)


(cl-defun scrap-unknown-yet (&key data &allow-other-keys)
  "Scrap the book URL of  And write it to file named '<title>.org'.
Optional argument DATA ."
  ;; Some text have line break and spacing in front
  ;; this isn't perfect after scrapped
  (let ((title
         (--> (car (dom-by-id data "name"))
              (dom-text it)))
        (chapters
         (dom-by-class data "rd-content")))
    (with-temp-file (concat title ".org")
      (insert "#+title: " title "\n")
      (dolist (chapter chapters)
        (insert "* " (dom-text (dom-by-tag chapter 'h1)))
        (insert ?\n)
        (insert
         (s-join "\n\n"
                 (mapcar 'dom-texts/sanitise
                         (dom-by-tag chapter 'p))))
        (insert ?\n?\n)))
    (message "written to %s" (concat title ".org"))
    ))

(defun dom-texts/sanitise (node &optional separator)
  "Return all textual data under NODE concatenated with SEPARATOR in-between."
  (if (eq (dom-tag node) 'script)
      ""
    (mapconcat
     (lambda (elem)
       (string-trim
        (cond
         ((stringp elem) elem)
         ((eq (dom-tag elem) 'script) "")
         (t (dom-texts/sanitise elem separator)))))
     (dom-children node)
     (or separator ""))))

;; (scrap-unknown-yet :data temp)

(defun scrap-zhonghuadiancang-chapter (url filename)
  "Scrap the chapter URL of 中华典藏 to FILENAME."
  (scrap-request-html
   url
   (cl-function
    (lambda (&key data &allow-other-keys)
      (message "html parsed ok")
      (let ((chapter (dom-text (car (dom-by-tag data 'h1))))
            (content (dom-by-id data "content"))
            (gettext (lambda (elm)           ; remove \n
                       (string-trim
                        (if (stringp elm)
                            elm
                          (dom-texts/sanitise elm))))))
        
        (--> (s-join "\n" (mapcar gettext (dom-children content)))
             (format "\n* %s\n%s" chapter it)
             (with-temp-buffer
               (insert it)
               (append-to-file (point-min) (point-max) filename))))))))

(defun random-of-0.5 ()
  (+ (/ (random 5) 10.0)
     0.05))

(cl-defun scrap-zhonghuadiancang (&key data &allow-other-keys)
  "Scrap the book URL of 中华典藏.
And write it to file named '<title>.org'."
  ;; (scrap-request-html
  ;;  url
  ;;  (cl-function
  ;;   (lambda (&key data &allow-other-keys))))
  (message "html parsed ok")
  (let ((title
         (dom-texts (dom-by-class (car (dom-by-tag data 'ol)) "active")))
        (chapters
         (dom-by-tag (dom-by-id data "booklist") 'a)))
    (with-temp-file (concat title ".org")
      (insert "#+title: " title "\n"))
    (dolist (chapter chapters)
      (scrap-zhonghuadiancang-chapter
       (dom-attr chapter 'href)
       (concat title ".org"))
      (sleep-for (random-of-0.5)))
    (message "written to %s" (concat title ".org"))))

(cl-defun scrap-daode (&key data &allow-other-keys)
  "Scrap the book URL of 道德. And write to file named '<title>.org'."
  (message "html parsed ok")
  (let ((title
         (dom-texts (car (dom-by-class data "style18"))))
        (chapter-urls
         (--filter (and (stringp it)
                        (s-match "[0-9]+\\.htm" it))
                   (mapcar (lambda (node) (dom-attr node 'href))
                           (dom-by-tag data 'a)))))
    (let ((filename (concat title ".org")))
      (with-temp-file filename
        (insert "#+title: " title "\n"))
      (dolist (chapter-url chapter-urls)
        (scrap-request-html
         chapter-url
         (cl-function
          (lambda (&key data &allow-other-keys)
            (message "html parsed ok")
            (let ((chapter (dom-texts (car (dom-by-class data "style18"))))
                  (content (dom-by-class data "style15"))
                  ;; (gettext (lambda (elm)     ; remove \n
                  ;;            (string-trim
                  ;;             (if (stringp elm)
                  ;;                 elm
                  ;;               (dom-texts/sanitise elm)))))
                  )
              (--> (s-join "\n\n" (mapcar #'dom-texts (dom-children content)))
                   (format "\n* %s\n%s" chapter it)
                   (with-temp-buffer
                     (insert it)
                     (append-to-file (point-min) (point-max) filename)))))))
        ;;
        (sleep-for (random-of-0.5)))
      (message "written to %s" filename))))

;; (scrap-find-module "https://www.zhonghuadiancang.com/")
(add-to-list 'scrap-modules '("zhonghuadiancang" . scrap-zhonghuadiancang))
(add-to-list 'scrap-modules '("daode" . scrap-daode))


;; (defvar temp
;;   (with-current-buffer
;;                  (find-file-noselect
;;                   "/Users/xception/sources/org/chinese/格致余论.html")
;;                (libxml-parse-html-region (point) (point-max))))

(defvar temp nil)
;; (scrap-request-html
;;  "http://www.daode.in/rdbook/dhzgcs/index.html"
;;  (cl-function
;;   (lambda (&key data &allow-other-keys)
;;     (setq temp data))))

;; (scrap-interface
;;  "https://www.zhonghuadiancang.com/xuanxuewushu/taiyijinhuazongzhi/")

;; (scrap-interface "http://www.daode.in/rdbook/dhzgcs/index.html")

(provide 'scrap)
;; scrap.el ends here