
;;; Commentary:
;;
;;; Code:

(require 'calendar)
(require 'cal-china)
(require 'wuxing)

(defvar yijing-ec-ten-gods-alist
  [("克我" "正官" "七杀")              ; 杀官
   ("生我" "正印" "偏印")              ; 枭印
   ("同我" "比肩" "劫财")              ; 比劫
   ("我生" "伤官" "食神")              ; 食伤
   ("我克" "正财" "偏财")              ; 才财
   ]
  "十神 关系 异性 同性.")

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
  "天干 alist in (天干名 五行 阴阳).")

(defvar yijing-ec-地支-alist
  '((?子 . "癸")
    (?丑 . "乙辛癸")
    (?寅 . "甲丙戊")
    (?卯 . "乙")
    (?辰 . "戊乙癸")
    (?巳 . "丙戊庚")
    (?午 . "丁己")
    (?未 . "己丁乙")
    (?申 . "庚壬戊")
    (?酉 . "辛")
    (?戌 . "戊辛丁")
    (?亥 . "壬甲"))
  "藏天干，本气，中气，余气.")

(defvar yijing-ec-branch-cycle-list
  (wuxing-make-cycle-list
   (mapcar #'car yijing-ec-地支-alist)))

(defvar yijing-ec-长生-cycle
  (wuxing-make-cycle-list
   '("长生" "沐浴" "冠带" "临官" "旺"
     "衰" "病" "死" "墓" "绝"
     "胎" "养")))

(defvar yijing-ec-长生-starts
  [(?甲 ?亥  1)
   (?乙 ?午 -1)
   (?丙 ?寅  1)
   (?丁 ?酉 -1)
   (?戊 ?寅  1)
   (?己 ?酉 -1)
   (?庚 ?巳  1)
   (?辛 ?子 -1)
   (?壬 ?申  1)
   (?癸 ?卯 -1)])

(defun yijing-ec-长生 (stem branch)
  (pcase-let* ((stem-num (yijing-ec-stem-num stem))
               (`(,_ ,start-branch ,dir)
                (aref yijing-ec-长生-starts stem-num)))
    (cl-loop for b in (member start-branch yijing-ec-branch-cycle-list)
             for x in (if (< dir 0)
                          (reverse yijing-ec-长生-cycle)
                        yijing-ec-长生-cycle)
             when (equal b branch)
             return x)))

;; (yijing-ec-长生 ?戊 ?卯)

(defun yijing-ec-gans->ten-gods (day other)
  (let* ((pillar (list day other))
         (elms (mapcar #'yijing-ec-gan-to-5elm pillar))
         (god-list (aref yijing-ec-ten-gods-alist
                         (apply #'wuxing-relation elms)))
         (types (mapcar (lambda (x) (caddr (assoc x yijing-ec-天干-alist)))
                        pillar)))
    ;; (list pillar elms)
    (if (equal (car types)
               (cadr types))
        (cl-third god-list)
      (cl-second god-list))))

(defun yijing-ec-cycle-num (pillar)
  "计算甲子数 from PILLAR.
公式来自:
怎么计算某一年的干支所表示的是一甲子中的第几年? - 黄亮anthony的回答 - 知乎
https://www.zhihu.com/question/513623565/answer/2327090033."
  (let* ((stem
          (yijing-ec-stem-num (aref pillar 0)))
         (branch
          (cl-position (string (aref pillar 1))
                       calendar-chinese-terrestrial-branch :test #'string=))
         (cycle
          (+ 60
             (- (* 6 stem)
                (% (* 5 branch) 60)))))
    cycle))

(defun yijing-ec-stem-num (stem)
  (cl-position stem yijing-ec-天干-alist :key #'car))

(defun yijing-ec-gan-to-5elm (stem)
  (cadr (assoc stem yijing-ec-天干-alist)))

(defun yijing-ec-zhi->hidden-gan (branch)
  (cdr (assoc branch yijing-ec-地支-alist)))

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
    (s-join "" (cl-subseq calendar-chinese-terrestrial-branch
                          start (+ start 2)))))

(defvar yijing-ec-纳音-alist
  '(("甲子乙丑" . "海中金") ("丙寅丁卯" . "炉中火") ("戊辰己巳" . "大林木")
    ("庚午辛未" . "路旁土") ("壬申癸酉" . "剑锋金")
    ("甲戌乙亥" . "山头火") ("丙子丁丑" . "漳下水") ("戊寅己卯" . "城头土")
    ("庚辰辛巳" . "白蜡金") ("壬午癸未" . "杨柳木")
    ("甲申乙酉" . "泉中水") ("丙戌丁亥" . "屋上土") ("戊己子己丑" . "霹雳火")
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

(defun yijing-ec-wuxing-score (pillars)
  "Using PILLARS to compute 5elm score.
Formula found: https://www.131.com.tw/word/b3_2_14.htm"
  (let ((scores (make-hash-table :size 5
                                 :test #'equal
                                 ;; :data
                                 ;; '(("金" . 0)
                                 ;;         ("木" . 0)
                                 ;;         ("水" . 0)
                                 ;;         ("火" . 0)
                                 ;;         ("土" . 0))
                                 ))
        (藏干-score  '((1 . (8))
                       (2 . (3 5))
                       (3 . (1 2 5)))))
    (cl-flet ((plist-incf (plist prop x)
                          (cl-incf
                           (gethash prop plist 0)
                           x)))
      (cl-loop for pillar in pillars
               for gan = (aref pillar 0)
               for gan5 = (yijing-ec-gan-to-5elm gan)
               for zhi = (aref pillar 1)
               for gans = (yijing-ec-zhi->hidden-gan zhi)
               do
               (plist-incf scores gan5 5)
               (message "%s " gan5)
               (cl-loop for score in (cdr (assoc (length gans) 藏干-score))
                        for gan in (cl-coerce gans 'list) ; string -> list
                        for gan5 = (yijing-ec-gan-to-5elm gan)
                        do
                        (message "%c %s" gan gan5)
                        (plist-incf scores
                                    gan5
                                    score)))
      scores)))

(defun yijing-ec-wuxing-health (pillars)
  (cl-loop with ht = (yijing-ec-wuxing-score pillars)
           for elm being the hash-keys of ht
           using (hash-values score)
           ;; collect (format "%s = %s" elm score)
           append (list elm score)
           ))

(defun yijing-ec-排盘 (date hour min)
  "四柱 from DATE HOUR and MIN."
  (let* ((year (yijing-ec-year-pillar date))
         (month (yijing-ec-month-pillar date))
         (day (yijing-ec-day-pillar date))
         (hour (yijing-ec-hour-pillar date hour min))
         (four-pillars (list year month day hour))

         (天干 (mapcar (lambda (x) (aref x 0)) four-pillars))
         (地支 (mapcar (lambda (x) (aref x 1)) four-pillars)))
    `(hline
      ("四柱:" "年柱" "月柱" "日柱" "时柱")
      ("十神:" ,@(mapcar (lambda (x)
                           (yijing-ec-gans->ten-gods (aref day 0) x))
                         天干))
      hline
      ("天干:" ,@(mapcar #'string 天干))
      ("地支:" ,@(mapcar #'string 地支))
      ("藏干:" ,@(mapcar #'yijing-ec-zhi->hidden-gan
                         地支))
      ("纳音:" ,@(mapcar #'yijing-ec-pillar->yin four-pillars))
      ("长生" ,@(mapcar (lambda (branch)
                          (yijing-ec-长生 (aref day 0) branch))
                        地支))
      ("运" "TODO")
      ("空亡:" ,@(mapcar #'yijing-ec-empty four-pillars))
      ("神煞:" "TODO")
      hline
      ("天干留意:" "TODO")
      ("地支留意:" "TODO")
      ;; ("五行注意:" ,(yijing-ec-wuxing-health four-pillars))
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

(defun yijing-ec-get-enclosing-solar-terms (date)
  "Return the two enclosing solar terms for DATE."
  (let ((year (calendar-extract-year date))
        (absolute-date (calendar-absolute-from-gregorian date)))
    (cal-china-x-sync-solar-term year)
    (cl-loop
     with last = nil
     for i in cal-china-x-solar-term-alist
     until (>= absolute-date
              (calendar-absolute-from-gregorian (car i)))
     do (setq last i)
     finally return (list i last))))

(defun yijing-solar-terms-of-month (date)
  "Return the two solar terms for this DATE.
Only month and year is used, i.e. day is unused."
  (let ((year (calendar-extract-year date))
        (month (calendar-extract-month date)))
    (cal-china-x-sync-solar-term year)
    (cl-loop with last = nil
             for alist on cal-china-x-solar-term-alist by #'cddr
             for i = (car alist)
             for ((i-month _day _year) . _节气) = i
             until (= month i-month)
             do (setq last i)
             finally return (list i (cadr alist)))))

;; 四柱算法

(defun yijing-convert-date (gregorian-date)
  "把 `格里高利历' GREGORIAN-DATE 换算成 更符合天干地支的计算."
  (pcase-let ((`(,month ,day ,year) gregorian-date)
              (`(,end ,start)
               (yijing-solar-terms-of-month gregorian-date)))
    (when (< (calendar-absolute-from-gregorian gregorian-date)
             (calendar-absolute-from-gregorian (car start)))
      ;; when month is rolled back, the days a treated as
      ;; continuation of previous month
      (cl-incf day
               (- (calendar-absolute-from-gregorian
                   (list month 1 year))
                  (calendar-absolute-from-gregorian
                   (list (1- month) 1 year))))
      (cl-decf month 1))
    ;; when year is rolled back, month increased by 12.  this
    ;; means jan and some days of feb are treated as the 11th and
    ;; 12th month of last year.
    (cl-case month
      (1
       (cl-decf year)
       (cl-incf month 12))
      (2
       (when (and
              (string= (cdr start) "立春")
              (< (calendar-absolute-from-gregorian gregorian-date)
                 (calendar-absolute-from-gregorian (car start))))
         (cl-decf year)
         (cl-incf month 12))))
    (list month day year)))

(defun calendar-chinese-sexagesimal-name (n)
  "The N-th name of the Chinese sexagesimal cycle.
N congruent to 1 gives the first name, N congruent to 2 gives the second name,
..., N congruent to 60 gives the sixtieth name."
  ;; Having - in between Chinese characters is weird.
  (format "%s%s"
          (aref calendar-chinese-celestial-stem (% (1- n) 10))
          (aref calendar-chinese-terrestrial-branch (% (1- n) 12))))

(defun yijing-ec-year-pillar (date)
  "年柱 from DATE."
  (let* ((cn-date (calendar-chinese-from-absolute
                   (calendar-absolute-from-gregorian date)))
         (cn-year  (cadr   cn-date)))
    (calendar-chinese-sexagesimal-name cn-year)))

(defun yijing-ec-month-pillar (date)
  "月柱 from DATE.  年上起月法."
  (let ((cn-month (car date)))
    ;; (calendar-chinese-sexagesimal-name
    ;;  (+ 2 cn-month (* stem-base 12)))
    (calendar-chinese-sexagesimal-name
     (+
      (cdr
       (cl-assoc (aref (yijing-ec-year-pillar date) 0)
                 '(("甲己" . 3)        ; 丙寅
                   ("乙庚" . 15)       ; 戊寅
                   ("丙辛" . 27)       ; 庚寅
                   ("丁壬" . 39)       ; 壬寅
                   ("戊癸" . 51)       ; 甲寅
                   )
                 :test (lambda (x xs)
                         (cl-position x xs :test #'char-equal))))
      ;; offset ?
      (- cn-month 2)))))

(defun yijing-ec-day-pillar (date)
  "日柱，返回干支纪日 from DATE.
月基数修改自 高氏日柱公式第三版。
公式第三版来自: https://zhuanlan.zhihu.com/p/93508430."
  (let* ((cn-month (car      date))
         (cn-day   (cadr     date))
         (cn-year  (cl-caddr date))

         (s (% cn-year 100))
         (quarter-s (floor s 4))
         (m
          ;; 1,2 map to 13, 14, the rest remains the same
          ;; (+ 2 (caddr
          ;;       (calendar-chinese-from-absolute
          ;;        (calendar-absolute-from-gregorian date))))
          (if (<= cn-month 2)
              (+ cn-month 12)
            cn-month))
         (d cn-day))
    (calendar-chinese-sexagesimal-name
     (+ (* (1- cn-year) 5)
        (/ (1- cn-year) 4)
        (+ (* 30 (/ (1+ (expt -1 m))
                    2))
           (floor (- (* 3 m) 7) 5)
           ;; weird padder I found
           (if (<= cn-month 2)
               -6
             -1))
        d)
     ;; (% (+ (* 6 quarter-s)
     ;;       (* 5 (+ (* 3 quarter-s) (% s 4)))
     ;;       ;; 月基数运算
     ;;       (+ (* 30 (/ (1+ (expt -1 m))
     ;;                   2))
     ;;          (floor (- (* 3 m) 7) 5))
     ;;       d
     ;;       (% X 60))
     ;;    60)
     )))

(defun yijing-ec-hour-pillar (date hour min)
  "时柱 from DATE HOUR and MIN.
五鼠遁口诀:
甲己还加甲, 乙庚丙作初.
丙辛从戊起, 丁壬庚子居.
戊癸何方发, 壬子是真途."
  (calendar-chinese-sexagesimal-name
   (+
    (cdr
     (cl-assoc (aref (yijing-ec-day-pillar date)
                     0)
               '(("甲己" . 1)        ; 甲子
                 ("乙庚" . 13)       ; 丙子
                 ("丙辛" . 25)       ; 戊子
                 ("丁壬" . 37)       ; 庚子
                 ("戊癸" . 49)       ; 壬子
                 )
               :test (lambda (x xs)
                       (cl-position x xs :test #'char-equal))))
    ;; TODO 需要换算时刻.  TODO Nth day 23 hours is actually
    ;; the next day, this is not accounted yet. This need to be
    ;; fixed in day-pillar.
    (floor hour 2))))

;; test
(when nil
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
