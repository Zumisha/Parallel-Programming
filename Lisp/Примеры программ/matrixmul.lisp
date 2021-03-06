(ql:quickload :lparallel)
(defpackage :qs (:use :cl :lparallel))
(in-package :qs)

(defun matrix-make-const (n initial)
  (make-array (list n n) :initial-element initial))

(defun matrix-make-id (n)
  (let ((res (make-array (list n n) :initial-element 0)))
    (dotimes (i n)
      (setf (aref res i i) 1))
    res))

(defun matrix-make-random (n limit)
  (let ((res (make-array (list n n))))
    (dotimes (i n)
      (dotimes (j n)
        (setf (aref res i j) (random limit))))
    res))

(defun multiply (a b p)
  (let ((res (make-array (list (car (array-dimensions a)) (cadr (array-dimensions b))) :initial-element 0)))
    (multiply-parallel a b p res 0 (car (array-dimensions a)))))

(defun multiply-parallel (a b p res start end)
  (cond ((= p 1) (multiply-sequential a b res start end))
        (t (let ((block-size (floor (/ (- end start) p))))
             (plet ((res1 (multiply-sequential a b res start (+ start block-size)))
                   (res2 (multiply-parallel a b (- p 1) res (+ start block-size) end)))
               res2)))))

(defun multiply-sequential (a b res start end)
  (do ((i start (+ 1 i)))
      ((= i end) res)
    (dotimes (j (cadr (array-dimensions b)))
      (get-result-matrix-element a b res i j))))

(defun get-result-matrix-element (a b res i j)
  (dotimes (k (cadr (array-dimensions a)))
    (setf (aref res i j) (+ (aref res i j) (* (aref a i k) (aref b k j))))))

(defun test1 ()
  (let ((a (matrix-make-const 5 10))
        (b (matrix-make-const 5 1))
        (res nil))
    (setf res (multiply a b 2))
    (print res)))

(defun matrix-from-file (name m n)
  (with-open-file (input-file name :direction :input)
    (let ((res (make-array (list m n))))
      (dotimes (i m)
        (dotimes (j n)
          (setf (aref res i j) (read))))
      res)))

(defun test (n p)
  ;(let ((a (matrix-make-const n 3))
  ;      (b (matrix-make-const n 2))
  ; Тут хардкодь размерности и имя файла
  (let ((a (matrix-from-file (concatenate 'string (princ-to-string n) ".txt") n n))
        (res nil))
    (setf res (multiply a a p))
    (cond ((<= n 10) (print-array-to-file res "out.txt"))
          (t (print (aref res 0 0))))))

(defun print-array-to-file (arr filename)
  (with-open-file (ot filename :direction :output :if-exists :supersede)
    (dotimes (i (car (array-dimensions arr)))
      (dotimes (j (cadr (array-dimensions arr)))
        (princ (aref arr i j))
        (princ (" ")))
      (terpri))))

(defun pow (x n)
  (cond ((= n 0) 1)
        (t (* x (pow x (- n 1))))))
  
(defun run-tests (n)
  (with-open-file (ot "stat2.txt" :direction :output :if-exists :supersede)	
    (let ((a (matrix-from-file (concatenate 'string (princ-to-string n) ".txt") n n)))
      (dotimes (i1 4 t) 
        (let ((thread (pow 2 i1)))
          (setf lparallel:*kernel* (lparallel:make-kernel thread))
          (prin1 (concatenate 'string "Число потоков: " (write-to-string thread)) ot)
          (dotimes (i2 6 t) 
            (let ((zern (pow 2 i2)) (tm 0.0))
;	        (print (concatenate 'string "Зернистость: " (write-to-string zern)) ot)
              (dotimes (i3 10 t) 
                (let ((start (/ (get-internal-real-time) 1000)))
                  (test a a zern)
                  (setq tm (+ tm (- (/ (get-internal-real-time) 1000) start)))))
              (setq tm (/ tm 10))
              (print tm ot)))
          (terpri ot)))
      (terpri ot))))

(test 10 8)
(sb-ext:exit) 