

(require 'dom) ; dom manipulation
(require 'xml) ; parse html
(require 'request) ; url request

(defvar temp (with-current-buffer
                 (find-file-noselect
                  "./约瑟传说精灵大全_赛尔号2精灵大全图片-4399约瑟传说.html")
               (libxml-parse-html-region (point) (point-max))))


(defun 4399seer2-pokemon-li (html)
  (--> (car (dom-by-id html "genius_all"))
       (dom-non-text-children it)))

(defun 4399seer2-name-from-li (node)
  (--> (dom-by-class node "eimg")
       (car it)
       (dom-text it)
       (string-trim it)
       ))

(defun 4399seer2-href-from-li (node)
  (--> (dom-by-class node "eimg")
       (car it)
       (dom-attr it 'href)))

(4399seer2-name-from-li (car (4399seer2-pokemon-li temp)))

(defvar temp (with-current-buffer
                 (find-file-noselect
                  "./约瑟传说碧珞逍遥种族值 技能表_4399约瑟传说.html")
               (libxml-parse-html-region (point) (point-max))))

(defun 4399seer2-pokemon-info-page (html)
  (--> (dom-by-tag temp 'table)
       (dom-by-tag it 'tr)))

(destructuring-bind
    (page-title pokemon-image type-and-heraldry useles extra
                base-stat-names base-stats skills-title skills-image
                &rest ignored)
    (4399seer2-pokemon-info-page temp)
  (destructuring-bind (type heraldry)
      (dom-by-tag type-and-heraldry 'td)
    (list
     :type
     ;; (string-trim (dom-texts type))
     :heraldry
     ;; (string-trim (dom-texts heraldry))
     :stat
     (--map (string-trim (dom-texts it))
            (dom-by-tag base-stats 'td))
     :skill
     (--> (dom-by-tag skills-image 'img)
          (car it)
          (dom-attr it 'src)))))
"http://newsimg.5054399.com/uploads/userup/1703/2G52G63619.jpg"

(defun scrap-request-html (url handler)
  (request
    url
    ;; parser implemented with reference to `json-read'
    :parser (lambda () (libxml-parse-html-region (point) (point-max)))
    :success handler))

(defun scrap-4399seer2-pokemon-info-page (url)
  "Scrap the chapter URL of 中华典藏 to FILENAME."
  (scrap-request-html
   url
   (cl-function
    (lambda (&key data &allow-other-keys)
      (4399seer2-pokemon-info-page data)))))

;; (defun scrap-unknown-yet (data)
;;   "Scrap the book URL of  And write it to file named '<title>.org'.
;; Optional argument DATA."
;;   ;; Some text have line break and spacing in front
;;   ;; this isn't perfect after scrapped
;;   (let ((title
;;          (--> (car (dom-by-id data "genius_all"))
;;               (dom-children it)))
;;         (chapters
;;          (dom-by-class data "rd-content")))
;;     (with-temp-file (concat title ".org")
;;       (insert "#+title: " title "\n")
;;       (dolist (chapter chapters)
;;         (insert "* " (dom-text (dom-by-tag chapter 'h1)))
;;         (insert ?\n)
;;         (insert
;;          (s-join "\n\n"
;;                  (mapcar 'dom-texts/sanitise
;;                          (dom-by-tag chapter 'p))))
;;         (insert ?\n?\n)))
;;     (message "written to %s" (concat title ".org"))
;;     ))