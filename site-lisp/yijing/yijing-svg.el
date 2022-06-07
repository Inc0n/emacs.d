;;; Code:

(require 'yijing)

(defvar gua-svg-yin-fill-factor 0.8
  "The ratio of filled to space in center.
The bigger this number is, the smaller the center gap.")

(defvar gua-svg-fill-factor 0.75)
(defvar gua-svg-yao-fill-factor 0.618)

(defvar gua-svg-color
  '(:yin "#111" :yang "#f10" :text "#edf")
  "Default color for the yao of gua svg.")

(defvar gua--current nil
  "Keep track of current gua to operate on.")

;;; Helpers

(defalias 'point-x 'car)
(defalias 'point-y 'cdr)
(defalias 'dim-w 'car)
(defalias 'dim-h 'cdr)

(defun gua-get-color (yao)
  (plist-get gua-svg-color yao))

;;; Gua, yao svg construct

(defun gua-percent (num)
  (format "%d%%" num))

(defun gua-make-rect (p dim)
  `(rect ((width . ,(dim-w dim))
          (height . ,(dim-h dim))
          (x . ,(point-x p))
          (y . ,(point-y p)))))

(defun gua-append-attris (sexp attri-pairs)
  "Modify the attribute of svg SEXP to with ATTRI-PAIRS."
  `(,(car sexp)
    (,@(--remove (assq (car it) attri-pairs) (cadr sexp))
     ,@attri-pairs)                      ; override the fill-color
    ,@(cddr sexp)))

;;; SVG yao function

(defun svg-yang (p dim)
  `(svg ((fill . ,(gua-get-color :yang)))
        ,(gua-make-rect p dim)))

(defun svg-yin (p dim)
  (let* ((w-ratio gua-svg-yin-fill-factor)
         ;; (dim (cons 100.0 (* 100.0 gua-svg-yao-fill-factor)))
	 (w (* (dim-w dim) (/ w-ratio 2)))
	 (rw (* (dim-w dim) (- 1 (/ w-ratio 2)))))
    (let ((p1 p)
	  (p2 (cons (+ (point-x p) rw)
		    (point-y p)))
	  (dim (cons w (dim-h dim))))
      ;; (mapcar (lambda (rect)))
      `(svg ((fill . ,(gua-get-color :yin)))
            ,(gua-make-rect p1 dim)
            ,(gua-make-rect p2 dim)))))

;;; SVG layout

(defun gua-wrap-svg-tag (attris elm)
  (if (eq (car elm) 'svg)
      (gua-append-attris elm attris)
    `(svg ,attris ,elm)))

(defun gua-layout (items fill-ratio handler)
  (let* ((len (float (length items)))
	 (h (/ 100.0 len))
	 (fill-h (* h (float fill-ratio)))
         ;; reduce gap for `len'-1 instead of `len'
         (h (+ h (* fill-ratio (/ fill-h len))))
         (h-str (gua-percent fill-h)))
    (cl-loop for y0 = 0 then (+ y0 h)
             for item in items
             collect (funcall handler item y0 h-str))))

(defun gua-vertical-layout (items fill-ratio)
  (gua-layout items fill-ratio
              (lambda (item y0 h-str)
                (gua-wrap-svg-tag
                 `((x . 0)
                   (y . ,(gua-percent y0))
                   (width . "100%")
                   (height . ,h-str))
                 item))))

(defun gua-horizontal-layout (items fill-ratio)
  (gua-layout items fill-ratio
              (lambda (item x0 w-str)
                (gua-wrap-svg-tag
                 `((x . ,(gua-percent x0))
                        (y . 0)
                        (width . ,w-str)
                        (height . "100%"))
                 item))))

;;; Gua yao SVG interface

(defun gua--svg-yaos (yaos width height)
  "Transform the list of svg yao functions (YAOS) into a list of
svg-image compatible data."
  (gua-vertical-layout
   (cl-loop with point = (cons 0 0)
            with dim = (cons width height)
            for y in yaos
            collect (funcall y point dim))
   gua-svg-yao-fill-factor))

;; '(svg-yang svg-yin svg-yin svg-yin svg-yang svg-yin)

(defun gua-svg-get-dim (&optional height)
  (let ((height (float (or height 100))))
    (cl-values (* 0.75 height)
               height)))

(defun gua--svg (gua width height)
  "Produce an image object for GUA of WIDTH and HEIGHT."
  `(svg ((width . ,width) (height . ,height)
         ;; (version . "1.1")
         ;; (xmlns . "http://www.w3.org/2000/svg")
         ;; (xmlns:xlink . "http://www.w3.org/1999/xlink")
         )
        ;; Gua takes top 80% of space
        (g ((transform
             . ,(format "scale(%f)\ntranslate(%f, %f)"
                        gua-svg-fill-factor
                        (* 0.5 width (- 1 gua-svg-fill-factor))
                        ;; y make space for text
                        (* 0.25 height (- 1 gua-svg-fill-factor)))))
           ,@(gua--svg-yaos
              (gua-binary->internal (gua->binary gua))
              width height))
        ;; Text take bottom 20% of space
        (text ((x . ,(* 0.5 width))
               (y . ,(* 0.95 height))
               (dominant-baseline . "middle")
               (text-anchor . "middle")
               (fill . ,(gua-get-color :text)))
              ,(gua-full-name gua))))

(defun gua--svg-multiple (guas width height)
  `(svg ((width . ,(* 3.0 width))
         (height . ,height)
         )
        ,@(cl-loop for x0 = 0 then (+ x0 width)
                   for gua in guas
                   collect (gua-wrap-svg-tag
                            `((x . ,x0)
                              (overflow . "visible"))
                            (gua--svg gua width height)))))

(defun gua-svg (gua &optional height)
  ;; (if (gua->binary gua)
  ;;   (message "Bad gua: %s\n" gua))
  (cl-multiple-value-bind (width height)
      (gua-svg-get-dim height)
    (svg-image
     (if (listp gua)
         (gua--svg-multiple gua width height)
       (gua--svg gua width height)))))

(defun gua-svg-test ()
  (let* ((height 100)
         (width (* 0.75 height)))
    (svg-gua '(svg-yang svg-yin svg-yin svg-yin svg-yang svg-yin)
	     :height (* (float height) gua-svg-fill-factor)
	     :aspect-ratio (/ (float width) (float height)))))

;;; Yao translation

(defun gua-binary->yao (yao)
  (cond ((characterp yao) (gua-binary->yao (string yao)))
        ((string= yao "0") 'svg-yin)
        ((string= yao "1") 'svg-yang)
        (:else (error "unexpected yao" yao))))

(cl-assert (eq (gua-binary->yao "0") 'svg-yin))
(cl-assert (eq (gua-binary->yao "1") 'svg-yang))

(defun gua-binary->internal (gua)
  (mapcar 'gua-binary->yao (reverse gua)))

(cl-assert (equal (gua-binary->internal "111000")
                  '(svg-yin svg-yin svg-yin svg-yang svg-yang svg-yang)))

;;; Gua UI

(defun yijing-display-gua (gua)
  (interactive (list (completing-read "Gua: " gua-64-names)))
  (yijing-with-buffer
   (lambda ()
     (insert-image (gua-svg gua))
     (insert "\n" (yijing-get-gua-text gua)))))

(defun yijing-display-natural-order-or-bin (bin)
  (interactive (list (read-from-minibuffer "Binary: ")))
  (if (not (string-empty-p bin))
      (yijing-display-gua (gua-from-bin bin))
    (yijing-with-buffer
     (lambda ()
       (dotimes (i 8)
         (cl-loop for j from 0 to 7
                  for idx = (+ (* i 8) j)
                  collect (nth idx gua-64-names) into guas
                  collect (gua-get-unicode (+ (* i 8) j))
                  into unicodes
                  ;; collect (gua-get-unicode (+ (* i 8) j)) into guas
                  finally
                  (progn
                    (insert "|" (s-join "|" guas) "|\n")
                    (insert "|" (s-join "|" unicodes) "|\n")
                    )))))))

(provide 'yijing-svg)
;;; yijing-svg.el ends here