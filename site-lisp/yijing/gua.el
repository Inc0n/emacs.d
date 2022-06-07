
(defgroup gua ()
  "卦."
  :group 'yijing
  :prefix "gua-")

(defvar gua-eight-phenomenon
  '("地" "山" "水" "风" "雷" "火" "泽" "天")
  "Ordered by their numerical binary value.")

(defvar gua-64-names
  '("坤" "剥" "比" "观" "豫" "晋" "萃" "否"
    "谦" "艮" "蹇" "渐" "小过" "旅" "咸" "遁"
    "师" "蒙" "坎" "涣" "解" "未济" "困" "讼"
    "升" "蛊" "井" "巽" "恒" "鼎" "大过" "姤"
    "复" "颐" "屯" "益" "震" "噬嗑" "随" "无妄"
    "明夷" "贲" "既济" "家人" "丰" "离" "革" "同人"
    "临" "损" "节" "中孚" "归妹" "睽" "兑" "履"
    "泰" "大畜" "需" "小畜" "大壮" "大有" "夬" "乾"))

(defvar gua-unicodes
  ;; 文王卦序
  '("乾" "坤" "屯" "蒙" "需" "讼" "师" "比" "小畜" "履" "泰" "否"
    "同人" "大有" "谦" "豫" "随" "蛊" "临" "观" "噬嗑" "贲"
    "剥" "复" "无妄" "大畜" "颐" "大过" "坎" "离"
    "咸" "恒" "遁" "大壮" "晋" "明夷" "家人" "睽"
    "蹇" "解" "损" "益" "夬" "姤" "萃" "升" "困" "井" "革" "鼎" "震"
    "艮" "渐" "归妹" "丰" "旅" "巽" "兑" "涣" "节" "中孚"
    "小过" "既济" "未济"))

(defun >> (x y)
  (ash x (- y)))

(defun gua-change (gua yao)
  (when-let ((i (--find-index (string= it gua) gua-64-names)))
    (nth (logxor i (>> 64 yao)) gua-64-names)))

(defun gua-overlap (gua)
  (--> (gua->binary gua)
       (concat (substring it 1 4) (substring it 2 5))
       (gua-from-bin it)))

(defvar gua-ordered
  (append (-difference (cl-subseq gua-64-names 32) '("乾" "离"))
          (reverse (-difference (cl-subseq gua-64-names 0 32) '("坤" "坎")))))

(defun gua-get-unicode (name)
  (or (cl-typecase name
        (string
         (when-let ((i (--find-index (string= it name) gua-unicodes)))
           (format "%c" (+ #x4DC0 i))))
        (symbol (gua-get-unicode (symbol-name name)))
        (fixnum (gua-get-unicode (nth name gua-64-names)))
        (t (error "Unexpected gua name")))
      "\u0000"))

(defun gua-full-name (gua)
  (let ((gua-bin (gua->binary gua))
        (to-prefix (lambda (x)
                     (--> (gua-binary->int x)
                          (nth it gua-eight-phenomenon)))))
    (concat (funcall to-prefix (substring gua-bin 3 6))
            (funcall to-prefix (substring gua-bin 0 3))
            (gua-from-bin gua-bin))))

(defun gua-transform (ss)
  (cond ((string= ss "乾") "姤")
        ((string= ss "坤") "复")
        ((string= ss "坎") "蒙")
        ((string= ss "离") "革")
        (:else ss)))

(defun gua-add (gua a)
  (let ((gua (gua-transform gua)))
    (or (when-let ((i (--find-index (string= it gua) gua-ordered)))
          (nth (% (+ i a) 60)
               gua-ordered))
        "")))

;;; Gua representation

;; (defun pad-string-left (str c len)
;;   (if (< (length str) len)
;;       (concat (s-repeat (- len (length str)) c)
;;               str)
;;     str))

(defun gua-int->binary-str (x &optional n)
  (s-pad-left (or n 6)
              "0"
              (let ((calc-number-radix 2))
                (math-format-radix x))))

(defun gua->binary (gua)
  (when-let ((i (--find-index (string= it gua) gua-64-names)))
    ;; convert to binary
    (gua-int->binary-str i 6)))

(defun gua-binary->int (b)
  (or (when-let ((res (condition-case nil
                          (read-from-string (concat "#b" b))
                        (invalid-read-syntax nil))))
        (cl-destructuring-bind (obj . i) res
          ;; (read-from-string (concat "#b" b))
          (when (= i (+ (length b) 2))
            obj)))
      -1))

(defun gua-from-bin (b)
  (nth (gua-binary->int b) gua-64-names))

(provide 'gua)
;;; gua.el ends here