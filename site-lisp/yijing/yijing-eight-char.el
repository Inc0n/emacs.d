;;; Commentary:
;; 
;;; Code:

(defvar yijing-wuxing-list
  '("火" "土" "金" "水" "木"))

(defun yijing-wuxing-relation (elm1 elm2)
  (cond ((not (member elm1 yijing-wuxing-list))
         (format "%s 非五行" elm1))
        ((equal elm1 elm2) "同")
        (t (let* ((elm1-rest (or (member elm1 yijing-wuxing-list)
                                 yijing-wuxing-list))
                  (elm2-rest (member elm2 elm1-rest)))
             (cond ((eq (cdr elm1-rest) elm2-rest) "生")     ; 1 生 2
                   ((equal (cddr elm1-rest) elm2-rest) "克") ; 1 克 2
                   (t (concat "反"                           ; 只能是 反生 反克
                              (yijing-wuxing-relation elm2 elm1))))))))

(defvar yijing-ec-ten-gods-alist
  '(("反生" "正印" "偏印")              ; 枭印
    ("同"   "比肩" "劫财")              ; 比劫
    ("生"   "伤官" "食神")              ; 食伤
    ("反克" "正官" "七杀")              ; 杀官
    ("克"   "正财" "偏财")              ; 才财
    )
  "关系 异 同.")

(defun yijing-ec-gans->ten-gods (day other)
  (let* ((pillar (list day other))
         (elms (mapcar #'yijing-ec-gan-to-elm pillar))
         (god-list (assoc (apply #'yijing-wuxing-relation elms)
                          yijing-ec-ten-gods-alist))
         (types (mapcar (lambda (x) (caddr (assoc x yijing-ec-天干-alist)))
                        pillar)))
    (list pillar elms)
    (if (equal (car types) (cadr types))
        (cl-third god-list)
      (cl-second god-list))))

(defvar yijing-ec-天干-alist
  '((?甲 "木" "阳")
    (?乙 "木" "阴")
    (?丙 "火" "阳")
    (?丁 "火" "阴")
    (?戊 "土" "阳")
    (?己 "土" "阴")
    (?庚 "金" "阳")
    (?辛 "金" "阴")
    (?壬 "水" "阳")
    (?癸 "水" "阴"))
  "天干 alist in (天干名 五行 阴阳).  Order is important.")

(defvar yijing-ec-地支-alist
  '((?子 "癸")
    (?丑 "乙" "辛" "癸")
    (?寅 "甲" "丙" "戊")
    (?卯 "乙")
    (?辰 "戊" "乙" "癸")
    (?巳 "丙" "戊" "庚")
    (?午 "丁" "己")
    (?未 "己" "丁" "乙")
    (?申 "庚" "壬" "戊")
    (?酉 "辛")
    (?戌 "戊" "辛" "丁")
    (?亥 "壬" "甲"))
  "藏天干，本气，中气，余气.  Order is important.")

(defun yijing-ec-cycle-num (pillar)
  "计算甲子数 from PILLAR.
公式来自:
怎么计算某一年的干支所表示的是一甲子中的第几年? - 黄亮anthony的回答 - 知乎
https://www.zhihu.com/question/513623565/answer/2327090033."
  (let* ((stem
          (yijing-ec-gan-num-from-pillar pillar))
         (branch
          (cl-position (string (aref pillar 1))
                       calendar-chinese-terrestrial-branch :test #'string=))
         (cycle
          (+ 60
             (- (* 6 stem)
                (% (* 5 branch) 60)))))
    cycle))

(defun yijing-ec-gan-num-from-pillar (pillar)
  ;; (cl-position (string (seq-elt pillar 0)) calendar-chinese-celestial-stem :test #'string=)
  (cl-position (seq-elt pillar 0) yijing-ec-天干-alist :key #'car))

(defun yijing-ec-gan-to-elm (stem)
  (cadr (assoc stem yijing-ec-天干-alist)))

;; 计算找到此旬之首，十二地支反推两位便是此旬空亡.
(defun yijing-ec-empty (pillar)
  "找此旬之空亡 from PILLAR.
某柱的天干代表的数字，减去10，所得余数；再从此柱地支顺数至所得余
数的地支为止，接下两位地支就属于空亡."
  (let* ((stem
          (cl-position (string (aref pillar 0))
                       calendar-chinese-celestial-stem :test #'string=))
         (branch
          (cl-position (string (aref pillar 1))
                       calendar-chinese-terrestrial-branch :test #'string=))
         (remain
          (abs (- stem 10)))
         (start
          ;; (- b (% cycle 10))
          (+ remain branch)))
    (if (>= start 12)
        (setq start (% start 12)))
    (s-join "" (subseq calendar-chinese-terrestrial-branch
                       start (+ start 2)))))

(defvar yijing-ec-纳音-alist
  '(("甲子乙丑" . "海中金") ("丙寅丁卯" . "炉中火") ("戊辰己巳" . "大林木")
    ("庚午辛未" . "路旁土") ("壬申癸酉" . "剑锋金")
    ("甲戌乙亥" . "山头火") ("丙子丁丑" . "漳下水") ("戊寅己卯" . "城头土")
    ("庚辰辛巳" . "白蜡金") ("壬午癸未" . "杨柳木")
    ("甲申乙酉" . "泉中水") ("丙戌丁亥" . "屋上土") ("戊子己丑" . "霹雳火")
    ("庚寅辛卯" . "松柏木") ("壬辰癸巳" . "长流水")
    ("甲午乙未" . "砂石金") ("丙申丁酉" . "山下火") ("戊戌己亥" . "平地木")
    ("庚子辛丑" . "壁上土") ("壬寅癸卯" . "金箔金")
    ("甲辰乙巳" . "覆灯火") ("丙午丁未" . "天河水") ("戊申己酉" . "大驿土")
    ("庚戌辛亥" . "钗钏金") ("壬子癸丑" . "桑柘木")
    ("甲寅乙卯" . "大溪水") ("丙辰丁巳" . "沙中土") ("戊午己未" . "天上火")
    ("庚申辛酉" . "石榴木") ("壬戌癸亥" . "大海水"))
  "纳音取象表.")

(defun yijing-ec-pillar->yin (pillar)
  (or (cdr (cl-assoc pillar yijing-ec-纳音-alist :test 's-contains?))
      (format "未知柱(%s)" pillar)))

(defun yijing-ec-排盘 (date hour min)
  "四柱 from DATE HOUR and MIN."
  (let* ((year (yijing-ec-year-pillar date))
         (month (yijing-ec-month-pillar date))
         (day (yijing-ec-day-pillar date))
         (hour (yijing-ec-hour-pillar date hour min))
         (four-pillars (list year month day hour))

         (天干 (mapcar (lambda (x) (aref x 0)) four-pillars)))
    `(hline
      ("四柱:" "年柱" "月柱" "日柱" "时柱")
      ("十神:" ,@(mapcar (lambda (x) (yijing-ec-gans->ten-gods (aref day 0) x))
                         天干))
      hline
      ("天干:" ,@(mapcar #'string 天干))
      ("地支:" ,@(mapcar (lambda (x) (string (aref x 1))) four-pillars))
      ("藏干:" ,@(mapcar (lambda (x) (cdr (assoc (aref x 1) yijing-ec-地支-alist)))
                         four-pillars))
      ("纳音:" ,@(mapcar #'yijing-ec-pillar->yin four-pillars))
      ("空亡:" ,@(mapcar #'yijing-ec-empty four-pillars))
      ("神煞:" "TODO")
      hline
      ("天干留意:" "TODO")
      ("地支留意:" "TODO")
      hline
      ("称骨重量:" "TODO")
      ("称骨评语:" "TODO")
      hline)))

(defun yijing-ec-meta (date hour min)
  (let* ((cn-date ;; (calendar-chinese-from-absolute
                  ;;  (calendar-absolute-from-gregorian ))
          date)
         (cn-month (car   cn-date))
         (cn-day  (cadr  cn-date))
         (cn-year (cl-caddr cn-date)))
    `(("公历:" ,(format "%s年%s月%s日 %s时%s分"
                        cn-year cn-month cn-day hour min))
      ("农历:" ,(cal-china-x-chinese-date-string date)))))

(defun yijing-ec-stem-branch-types-table ()
  "Table for stem."
  (-->
   `(("属性" "阳" "阴")
     ;; hline
     ,@(cl-loop for (stem elm _) in yijing-ec-天干-alist by #'cddr
                for (stem2 . _) in (cdr yijing-ec-天干-alist) by #'cddr
                collect (list elm (string stem) (string stem2))))
   ;; (apply 'seq-mapn 'list it)
   ;; (-insert-at 1 'hline it)
   ))

(defun yijing-ec-four-pillar (date hour min)
  "四柱 from DATE HOUR and MIN."
  (format "%s年 %s月 %s日 %s时"
          (yijing-ec-year-pillar date)
          (yijing-ec-month-pillar date)
          (yijing-ec-day-pillar date)
          (yijing-ec-hour-pillar date hour min)))

(defun yijing-from-gregorian (date)
  (let* ((cn-date (calendar-chinese-from-absolute
                   (calendar-absolute-from-gregorian date)))
         (cn-year  (cadr   cn-date))
         (cn-month (cl-caddr  cn-date))
         (cn-day   (cl-cadddr cn-date)))
    (list cn-month cn-day cn-year)))


;; 四柱算法

(defun yijing-ec-year-pillar (date)
  "年柱 from DATE."
  (let* ((cn-date (calendar-chinese-from-absolute
                   (calendar-absolute-from-gregorian date)))
         (cn-year  (cadr   cn-date)))
    (calendar-chinese-sexagesimal-name cn-year)))

(defun yijing-ec-month-pillar (date)
  "月柱 from DATE."
  (let* ((cn-date (calendar-chinese-from-absolute
                   (calendar-absolute-from-gregorian date)))
         (cn-year  (cadr cn-date))
         (stem-base (% (% (1- cn-year) 10)
                       5))
         (cn-month (cl-caddr  cn-date))
         ;; (cn-day   (cl-cadddr cn-date))
         )
    (calendar-chinese-sexagesimal-name
     (+ 2 cn-month (* stem-base 12)))))

(defun yijing-ec-day-pillar (date)
  "日柱，返回干支纪日 from DATE.
公式第三版来自: https://zhuanlan.zhihu.com/p/93508430."
  (let* ((cn-month (car      date))
         (cn-day   (cadr     date))
         (cn-year  (cl-caddr date))

         (s (% cn-year 100))
         (quarter-s (floor s 4))
         (u (% s 4))
         (m
          ;; 1,2 map to 13, 14, the rest remains the same
          ;; (+ 2 (caddr
          ;;       (calendar-chinese-from-absolute
          ;;        (calendar-absolute-from-gregorian date))))
          (if (<= cn-month 2)
              (+ cn-month 12)
            cn-month))
         (d cn-day)

         (C (1+ (floor cn-year 100)))
         (X (+ (* 44 (1- C))
               (floor (1- C) 4)
               9)))
    (calendar-chinese-sexagesimal-name
     (% (+ (* 6 quarter-s)
           (* 5 (+ u (* 3 quarter-s)))
           (+ (* 30 (/ (1+ (expt -1 m)) 2))
              (floor (- (* 3 m) 7) 5))
           d
           (% X 60))
        60))))

(defun yijing-ec-hour-pillar (date hour min)
  "时柱 from DATE HOUR and MIN."
  (let* ((cn-day (yijing-ec-gan-num-from-pillar
                  (yijing-ec-day-pillar date)))
         (day-stem-base
          (* (% cn-day 5) 2)))
    (calendar-chinese-sexagesimal-name
     (+ 1 ;; offset
        (* 6 day-stem-base)
        (/ hour 2)))))

;; (defun cal-china-x-chinese-date-string (date)
;;   (let* ((cn-date (calendar-chinese-from-absolute
;;                    (calendar-absolute-from-gregorian date)))
;;          (cn-year  (cadr   cn-date))
;;          (cn-month (cl-caddr  cn-date))
;;          (cn-day   (cl-cadddr cn-date)))
;;     (format "%s%s年%s%s%s%s(%s)"
;;             (calendar-chinese-sexagesimal-name cn-year)
;;             (aref cal-china-x-zodiac-name (% (1- cn-year) 12))
;;             (aref cal-china-x-month-name (1-  (floor cn-month)))
;;             (if (integerp cn-month) "" "(闰月)")
;;             (aref cal-china-x-day-name (1- cn-day))
;;             (cal-china-x-get-solar-term date)
;;             (cal-china-x-get-horoscope (car date) (cadr date)))))

;; test
(defun yijing-ec-test ()
  (cl-loop for (args expected) in
           '((((3 26 2018) 8 30)
              "戊戌年 乙卯月 丁巳日 甲辰时")
             (((3 16 2001) 0 00)
              "辛巳年 辛卯月 戊寅日 壬子时"))
           unless (string= (apply #'yijing-ec-four-pillar args)
                           expected)
           return (list args expected)
           finally (return 'no-error)))

(provide 'yijing-eight-char)
;;; yijing-eight-char.el ends here