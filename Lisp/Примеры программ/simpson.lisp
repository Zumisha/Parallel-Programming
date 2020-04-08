;(ql:quickload :lparallel)
;(defpackage :smps (:use :cl :lparallel))
;(in-package :smps)

(defun integrate (f a b eps p)
  (let ((h (/ (- b a) 2.0)) (odd-sum nil) (af (funcall f a)) (bf (funcall f b)))
    (setq odd-sum (funcall f (+ a h)))
    (integr f a b eps p (/ h 2.0) af bf odd-sum (* (/ h 3.0) (+ af bf (* 4.0 odd-sum))))))

(defun integr (f a b eps p h af bf last-sum I2h)
  (let ((odd-sum (odd-sum-calc f a b h p)) (Ih nil))
    (setq Ih (simpson-calc af bf h last-sum odd-sum))	
    (if (runge-calc eps Ih I2h)
      Ih
      (integr f a b eps p (/ h 2.0) af bf (+ last-sum odd-sum) Ih))))

(defun runge-calc (eps Ih I2h)
  (< (/ (abs (- Ih I2h)) 15.0) eps))

(defun simpson-calc (af bf h last-sum odd-sum)
  (* (/ h 3.0) (+ af bf (* last-sum 2.0) (* odd-sum 4.0))))

(defun odd-sum-calc (f a b h p)
  (sum f (+ a h) (- b h) (* 2.0 h) p))

(defun sum (f a b h p)
  (par-sum f a b h (* h p) p))

(defun par-sum (f a b h ph i)
  (if (and (> i 1) (<= a b))
      (let ((this-sum (seq-sum f a b ph)) 
             (next-sum (par-sum f (+ a h) b h ph (- i 1))))
            (+ this-sum next-sum))
    (seq-sum f a b ph)))

(defun seq-sum (f a b h)
  (do ((x a (+ x h))
       (s 0.0 (+ s (funcall f x))))
      ((> x b) s)))
  
(defun fun1 (x)
  (* (+ (* x x) 1.0) (cos (* 0.5 x))))

(defun fun2 (x)
  (/ (+ (* x x) (* 3.0 x) 11) (+ (* x x) (* 11.0 x) -7.0)))

(defun fun3 (x)
  (* (sin (* 0.5 x x x)) (sin (* 0.25 x x)) (sin (* 0.125 x))))

(defun test (p switch)
  (let ((start (/ (get-internal-real-time) 1000.0)))
	(print
    (cond ((= switch 1) (integrate #'fun1 0.0d0 4000.0d0 0.00001d0 p))
         ((= switch 2) (integrate #'fun2 1.0d0 10000.0d0 0.0000001d0 p))
         (t (integrate #'fun3 0.0d0 100.0d0 0.0000001d0 p))))
	(print (concatenate 'string "Time: " (write-to-string (- (/ (get-internal-real-time) 1000) start))))))