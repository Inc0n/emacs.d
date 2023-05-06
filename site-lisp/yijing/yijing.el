
;;; Commentary:
;;; Code:

(defgroup yijing ()
  "易经 x Emacs."
  :group 'applications
  :prefix "yijing-")

(require 'gua)

(defcustom yijing-text-file (concat default-directory "/yijing.org")
  "Directory of yijing text."
  :group 'yijing)

(defvar yijing-buffer-name "*yijing*" "Buffer name to operate with.")

(defun yijing-with-buffer (insert-fn)
  (pop-to-buffer yijing-buffer-name)
  (read-only-mode -1)
  (erase-buffer)
  (funcall insert-fn)
  (goto-char (point-min))
  ;; (local-set-key "q" nil)
  ;; (read-only-mode 1)
  )

(defun yijing-calculate-yun (y)
  (if (>= y 1744)
      (+ (/ (- y 1744) 360) 192)
      (+ (/ (- y 1743) 360) 191)))

(defun max-rem (a b)
  (if (= (% a b ) 0)
      b
    (% a b)))

(defun yijing-calculate-year (year)
  (if (= year 0)
      "没有0年"
    (let* ((y (if (> year 0) year (1+ year)))
           (ayun (gua-calculate-yun y)))
      (if (or (> ayun 360) (< ayun 1))
          "超出本元"
        (let* ((会 (1+ (/ (1- ayun) 30.0)))
               (运 (max-rem ayun 30))
               (会卦 (nth (floor (1- ayun) 6) gua-ordered))
               (运卦 (gua-change 会卦 (max-rem ayun 6)))
               (ayear (- y (* (- ayun 192) 360) 1744)) ;; 0
               (世 (1+ (/ ayear 30.0)))
               (年 (1+ (% ayear 30)))
               (世卦 (gua-change 运卦 (1+ (floor (1- 世) 2))))
               (十年卦 (gua-change 世卦
                                   (+ (floor (1- 年) 10)
                                      (if (= (% (truncate 世) 2) 1)
                                          1
                                        4))))
               (年卦 (gua-add 世卦 (% ayear 60))))
          (cl-loop for str in '("会" "运" "世" "十年" "年")
                   for 卦 in (list 会卦 运卦 世卦 十年卦 年卦)
                   do
                   (insert (format "%s %s" str 卦))
                   (insert-image (gua-svg 卦))
                   (insert "\n"))
          ;; (org-table-align)
          ;; (org-table-transpose-table-at-point)
          (insert "\n")
          (concat
           (format "公元%d年\n"
                   (if (>= year 0)
                       year
                     (format "前%d" (- year))))
           (format "第%.2f会，第%d运，全元第%d运，第%.2f世，第%d年，全运第%d年\n"
                   会 运 ayun 世 年 (+ ayear 1))
           (apply 'format "%s%s值会，%s%s值运，%s%s值世，十年卦%s%s，%s%s值年\n"
                  (cl-loop for x in (list 会卦 运卦 世卦 十年卦 年卦)
                           collect x
                           collect (gua-get-unicode x)))
           (format "月卦依次为%s"
                   (s-join "、"
                           (cl-loop for i from 1 to 6
                                    for x = (gua-change 年卦 i)
                                    collect (concat x (gua-get-unicode x))))))
          )))))

(defun yijing-show-calculate (year)
  (interactive (list (read-from-minibuffer "Year: ")))
  (yijing-with-buffer
   (lambda ()
     (let ((num-year (string-to-number year)))
       (if (= num-year 0)
           (insert (format "Bad year: %s\n" year))
         (insert (yijing-calculate-year num-year)))))))

(defun yijing-get-gua-text (gua)
  (with-current-buffer (find-file-noselect yijing-text-file)
    (goto-char (org-find-olp (list gua) 'this-buffer))
    (org-narrow-to-subtree)
    (prog1 (buffer-substring-no-properties (point-min) (point-max))
      (widen))))

;;; plum blossom

(defun yijing-plum-blossom-nums (lst)
  (cl-case (length lst)
    (1 (let ((x (car lst)))
         `(,(/ x 2)
           ,(+ (/ x 2) (% x 2))
           ,x)))
    (2 `(,@lst ,(apply '+ lst)))
    (3 lst)
    (t (error "unexpected lst"))))

(defun yijing-plum-blossom (&rest args)
  "ARGS is parsed with `yijing-plum-blossom-nums' to a list of 3 numbers."
  (cl-destructuring-bind (a b c)
      (yijing-plum-blossom-nums args)
    (--> (mapcar (lambda (x)
                   (--> (% x 8)
                        (yijing-int-innate->extrinsic it)
                        (gua-int->binary-str it 3)))
                 (list b a))
         (gua-from-bin (apply 'concat it))
         (list
          it
          (gua-overlap it)
          (gua-change it (% c 6))))))

;;;

(defun yijing-sheshi-dinvate (total)
  (let* ((left (random total))          ; splitting total into two parts
         (right (- total left 1))     ; take this one out from right hand
         ;; 
         (left (max-rem left 4))
         (right (max-rem right 4)))
    (- total left right 1)))

(defun yijing-divination ()
  (cl-flet ((fn (total) ; short hand
                (yijing-sheshi-dinvate total))
            (unique-of-1000 (fn)
                            (let ((acc '()))
                              (dotimes (i 1000)
                                (setq acc (cl-adjoin (funcall fn)
                                                     acc)))
                              (sort acc '<))))
    (let* ((first-change (fn 49))
           (second-change (fn first-change))
           (third-change (fn second-change)))
      (unique-of-1000
       (lambda ()
         (/ (fn (fn (fn 49)))
            4))))))

;;; yijing innate

(defun yijing-innate-gua-from-int (i)
  (gua-from-bin
   (s-repeat 2
             (yijing-int-innate->extrinsic (- 8 i) 3))))

(defun yijing-int-innate->extrinsic (i)
  (- 8 i))

(defun yijing-innate-eight ()
  (cl-loop for i from 0 to 7
           for bin = (gua-int->binary-str i 3)
           collect (gua-from-bin (s-repeat 2 bin))))

(provide 'yijing)
;;; yijing.el ends here
