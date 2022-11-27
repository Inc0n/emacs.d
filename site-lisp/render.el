
;;; Commentary:
;; Fractal Rendering in Emacs
;; nullprogram.com/blog/2012/09/14/
;;; Code:
(cl-labels ((polygon (o p1 p2 p3 &rest args)
		     `(polygon
		       ((points . ,(mapconcat ; put into
				    (lambda (pair) ; x1 y1, x2 y2 ...
				      (format "%s %s" (car pair) (cdr pair)))
				    (list o p1 p2 p3)
				    ", "))
			,@args)))
	    ;; it's just a trapezium
	    (yang-yao (x y width height ratio)
		      (let ((width (* (* width 0.5) 2.0)))
			(polygon
			 (cons x y)
			 (cons (+ x width) y)
			 (cons (+ x (* ratio width)) (+ y height))
			 (cons (+ x (* (- 1 ratio) width)) (+ y height))
			 '(fill . "red"))))
	    ;; it's two trapezium with spacing in between
	    (yin-yao (x y width height ratio)
		     (let* ((gap 2.0)
			    (width (- (* width 0.5) (* gap 0.5)))
			    (x1 (+ x width (* gap 0.5))))
		       `(g ((fill . "black"))
			   ,(polygon
			     (cons x y)
			     (cons (+ x width) y)
			     (cons (+ x width) (+ y height))
			     (cons (+ x (* (- 1 ratio) width)) (+ y height)))
			   ,(polygon
			     (cons x1 y)
			     (cons (+ x1 width) y)
			     (cons (- (+ x1 width) (* (- 1 ratio) width))
				   (+ y height))
			     (cons x1 (+ y height))))))
	    (gua (i num width height ratio padding)
		 (let* ((cx (* (/ (+ height padding)
				  (* (- 1.0 ratio)
				     width))
			       ;; dy/dx * CX
			       (* width 0.5)))
			(x (- cx (* width 0.5))))
		   `(g ((transform . ,(format "rotate(%d,%d,%d)"
					      (* -45 i)
					      cx
					      cx)))
		       ,@(cl-loop
			  with y = 0
			  repeat 3
			  for yao in (num-to-yaos num)
			  collect (funcall yao x y width height ratio)
			  do
			  (cl-incf x (* (- 1 ratio) width))
			  (cl-incf y (+ height padding))
			  (setq width (* ratio ratio width))))))
	    (num-to-yaos (i)
			 (mapcar
			  (lambda (c) (if (eq c ?0)
				     #'yin-yao
				   #'yang-yao))
			  (s-reverse	; the order is reverse of numeric
			   (s-pad-left	; pad to length 3 with 0 
			    3		; to base 2
			    "0"		
			    (let ((calc-number-radix 2))
			      (math-format-radix i))))))
	    (taiji (cx cy r &rest args)
		   (let ((c1 "black")
			 (c2 "white")
			 (half-r (* 0.5 r))
			 (inner-r (* (/ 1.0 3 2) r)))
		     `(g (,@args)
			 ;; two big half circle

			 (circle ((cx . ,cx)
				  (cy . ,cx)
				  (r . ,r)
				  (fill . ,c1)))
			 (path ((d . ,(format "M%s,%s A%s,%s %s 0,1 %s,%s"
					      cx (+ cy r)
					      r r
					      ;; rotate by 
					      90
					      cx (- cx r)))
				;; arc path calculation
				;; http://xahlee.info/js/svg_circle_arc.html
				;; "M0,50 a1,1 0 0 0,0 100,0"
				(fill . ,c2)))
			 ;; two smaller circles
			 (circle ((cx . ,cx)
				  (cy . ,(- cx (* 0.5 r)))
				  (r . ,half-r)
				  (fill . ,c2)))
			 (circle ((cx . ,cx)
				  (cy . ,(+ cx (* 0.5 r)))
				  (r . ,half-r)
				  (fill . ,c1)))
			 ;; circle within smaller circles
			 (circle ((cx . ,cx)
				  (cy . ,(- cx (* 0.5 r)))
				  (r . ,inner-r)
				  (fill . ,c1)))
			 (circle ((cx . ,cx)
				  (cy . ,(+ cx (* 0.5 r)))
				  (r . ,inner-r)
				  (fill . ,c2))))))
	    (svg (deg seq)
		 ))
  (let ((ratio 0.94)
	(width 40)			; width of yao
	(height 8)			; height of yao
	(padding 2))
    `(svg ((width . 240)
	   (height . 240)
	   ;; (viewBox . "0 0 120 120")
	   (xmlns . "http://www.w3.org/2000/svg"))
	  (g ()
	     ,@(cl-loop for i from 0
			;; 先天卦序
			for num in '(0 1 2 3 7 6 5 4)
			collect (gua i (- 7 num) width height ratio padding))
	     ,(let ((cx
		     (* (/ (+ height padding)
			   (* (- 1.0 ratio)
			      width))
			;; dy/dx * CX
			(* width 0.5))))
		;; testing circle
		`(circle ((cx . ,cx) (cy . ,cx) (r . 10)
			  (fill . "blue")))
		(taiji cx cx 50
		       `(transform . ,(format "rotate(%s,%s,%s)"
					      90
					      cx
					      cx))))))))

(defun lst-svg-p (lsvg)
  (pcase lsvg
    (`(,tag ,attr . ,nodes) (and (symbolp tag) (listp attr) (listp nodes)))
    (_ nil)))
(defun lst-svg-tag (lsvg) (car lsvg))
(defun lst-svg-attr (lsvg) (cadr lsvg))
(defun lst-svg-nodes (lsvg) (cddr lsvg))

(defun lsvg->svg-attri (plist)
  (mapconcat 'identity
             (cl-loop for (attr value) on plist by 'cddr
                      collect (format "%s=%S"
                                      (substring (symbol-name attr) 1)
                                      value))
             " "))

(defun lsvg->svg (data)
  (if (lst-svg-p data)
      (format "<%s %s>%s</%s>"
              (lst-svg-tag data)
              (lsvg->svg-attri (lst-svg-attr data))
              (mapconcat 'identity (mapcar #'lsvg->svg (lst-svg-nodes data)) "\n")
              (lst-svg-tag data))
    (error "invalid lsvg data type" data)))

(progn
  (pop-to-buffer (get-buffer-create "*sierpinksi*"))
  (fundamental-mode) (erase-buffer)
  (svg-insert-image
   `(svg ((xmlns . "http://www.w3.org/2000/svg"))
         (polygon ((points . "0,0 75,0 75,10.299999 0,10.299999 ")
		   (style . "fill:#cccccc;stroke-width:0;")))
         (polygon ((points . "0,16.666666 30.0,16.666666 30.0,26.966665 0,26.966665 ")
		   (style . "fill:#cccccc;stroke-width:0;")))
         (polygon ((points . "45.0,16.666666 75.0,16.666666 75.0,26.966665 45.0,26.966665 ") (style . "fill:#cccccc;stroke-width:0;")))
         (polygon ((points . "0,33.333332 30.0,33.333332 30.0,43.63333 0,43.63333 ") (style . "fill:#cccccc;stroke-width:0;")))
         (polygon ((points . "45.0,33.333332 75.0,33.333332 75.0,43.63333 45.0,43.63333 ") (style . "fill:#cccccc;stroke-width:0;")))
         (polygon ((points . "0,50.0 30.0,50.0 30.0,60.3 0,60.3 ") (style . "fill:#cccccc;stroke-width:0;")))
         (polygon ((points . "45.0,50.0 75.0,50.0 75.0,60.3 45.0,60.3 ") (style . "fill:#cccccc;stroke-width:0;")))
         (polygon ((points . "0,66.666664 30.0,66.666664 30.0,76.96666 0,76.96666 ") (style . "fill:#cccccc;stroke-width:0;")))
         (polygon ((points . "45.0,66.666664 75.0,66.666664 75.0,76.96666 45.0,76.96666 ") (style . "fill:#cccccc;stroke-width:0;")))
         (polygon ((points . "0,83.33333 30.0,83.33333 30.0,93.63333 0,93.63333 ") (style . "fill:#cccccc;stroke-width:0;")))
         (polygon ((points . "45.0,83.33333 75.0,83.33333 75.0,93.63333 45.0,93.63333 ") (style . "fill:#cccccc;stroke-width:0;")))))
  (image-mode))
(test-svg)

;; defun sierpinski
(let ((s (expt 3 5)))
  (pop-to-buffer (get-buffer-create "*sierpinksi*"))
  (fundamental-mode) (erase-buffer)
  (cl-labels ((fill-p (x y)
		      (cond ((or (zerop x)
				 (zerop y))
			     "0")
			    ((and (= 1 (mod x 3))
				  (= 1 (mod y 3)))
			     "1")
			    (t (fill-p (/ x 3) (/ y 3))))))
    (insert (format "P1\n%d %d\n" s s))
    (dotimes (y s)
      (dotimes (x s)
	(insert (fill-p x y) " "))))
  (image-mode))

(defun mandelbrot ()
  (pop-to-buffer (get-buffer-create "*mandelbrot*"))
  (let ((w 400) (h 300) (d 32))
    (fundamental-mode) (erase-buffer)
    (set-buffer-multibyte nil)
    (insert (format "P6\n%d %d\n255\n" w h))
    (dotimes (y h)
      (dotimes (x w)
	(let* ((cx (* 1.5 (/ (- x (/ w 1.45)) w 0.45)))
	       (cy (* 1.5 (/ (- y (/ h 1.45)) h 0.5)))
	       (zr 0)
	       (zi 0)
	       (v (dotimes (i d d)
		    (if (> (+ (* zr zr ) (* zi zi)) 4)
			(return i)
		      (psetq zr (+ (* zr zr) (- (* zi zi)) cx)
			     zi (+ (* zr zr 2) cy))))))
	  (insert-char (floor (* 256 (/ v 1.0 d))) 3))))
    (image-mode)))

;;; render.el ends here