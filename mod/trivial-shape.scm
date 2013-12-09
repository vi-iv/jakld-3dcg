;; =============================================================================
;; The MIT License (MIT)
;;
;; Copyright (c) 2013, Koutarou FURUKAWA (<Furukawa.Koutarou@Gmail.com>)
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.
;; =============================================================================

;;;; Trivial shape

;;; cube

(define cube-001
  (painter:cube attribute-001 (list 10.0 10.0 10.0)))

(define cube-002
  (painter:cube attribute-001 (list 10.0 15.0 20.0)))

;;; sphere

(define sphere-001
  (painter:sphere attribute-001 10.0 0))

(define sphere-002
  (painter:sphere attribute-001 10.0 1))

(define sphere-003
  (painter:sphere attribute-001 10.0 2))

(define sphere-004
  (painter:sphere attribute-001 10.0 3))

;;; cylinder

(define cylinder-001
  (painter:cylinder attribute-001 10.0 5.0 5.0 2))

(define cylinder-002
  (painter:cylinder attribute-001 20.0 0.0 5.0 2))

;;; compounded

(define compounded-001
  (painter:translate
   '(-10.0 -10.0 -10.0)
   (painter:union cube-002
                  (painter:translate '(10.0  0.0  0.0) cube-002)
                  (painter:translate '( 0.0  0.0 10.0) cube-002)
                  (painter:translate '(10.0  0.0 10.0) cube-002)
                  (painter:translate '(20.0  0.0  0.0) cube-002)
                  (painter:translate '( 0.0  0.0 20.0) cube-002)
                  (painter:translate '(20.0  0.0 20.0) cube-002)
                  (painter:translate '(20.0  0.0 10.0) cube-002)
                  (painter:translate '(10.0  0.0 20.0) cube-002)
                  (painter:translate '( 0.0  0.75  0.0) cube-002)
                  (painter:translate '(10.0  0.75  0.0) cube-002)
                  (painter:translate '( 0.0  0.75 10.0) cube-002)
                  (painter:translate '(10.0  0.75 10.0) cube-002)
                  (painter:translate '(20.0  0.75  0.0) cube-002)
                  (painter:translate '( 0.0  0.75 20.0) cube-002)
                  (painter:translate '(20.0  0.75 20.0) cube-002)
                  (painter:translate '(20.0  0.75 10.0) cube-002)
                  (painter:translate '(10.0  0.75 20.0) cube-002))))

(define compounded-002
  (painter:union (painter:translate '(10.0 0.0 0.0) sphere-003)
                 (painter:translate '(0.0 0.0 10.0) sphere-003)
                 (painter:translate '(-10.0 0.0 0.0) sphere-003)
                 (painter:translate '(0.0 0.0 -10.0) sphere-003)))

(define compounded-003
  (painter:union (painter:translate '(10.0 0.0 0.0) cylinder-002)
                 (painter:translate '(0.0 0.0 10.0) cylinder-002)
                 (painter:translate '(-10.0 0.0 0.0) cylinder-002)
                 (painter:translate '(0.0 0.0 -10.0) cylinder-002)))

(define compounded-004
  (let ((cyl (painter:translate '(0.0 0.0 0.5) cylinder-002)))
    (painter:union cyl
                   (painter:rotate 90 '(1.0 1.0 1.0) cyl)
                   (painter:rotate 180 '(1.0 1.0 1.0) cyl)
                   (painter:rotate 270 '(1.0 1.0 1.0) cyl))))

(define compounded-005
  (painter:union
   (painter:rotate 90 '(1.0 0.0 0.0) compounded-003)
   (painter:rotate 90 '(1.0 0.0 0.0) (painter:translate '(0.0 20.0 0.0) compounded-003))
   (painter:rotate 90 '(1.0 0.0 0.0) (painter:translate '(0.0 -20.0 0.0) compounded-003))))

;;; revolution

(define glass-001
  (painter:translate
   '(-100 -40 -60)
   (painter:revolution
    attribute-001
    (map (lambda (p) (scl 1.5 (append1 p 0)))
         '(( 0  0) ( 0 16) ( 2 16) ( 3  5) ( 5  3)
           (10  2) (15  2) (20  3) (25 10) (28 15)
           (32 18) (35 19) (40 20) (50 20) (49 18)
           (36 17) (31 14) (27  9) (25  5) (24  0)))
    5)))

;;; scale

(define scale-001
  (let ((c (painter:cube attribute-001 '(10 10 10))))
    (painter:union
     (painter:translate '(0 -10  24) c)
     (painter:translate '(0 -10  12) (painter:scale '(2 1 1) c))
     (painter:translate '(0 -10   0) (painter:scale '(3 1 1) c))
     (painter:translate '(0 -10 -12) (painter:scale '(4 1 1) c)))))

;;; rotate

(define rotate-001
  (let ((c (painter:cube attribute-001 '(10 10 10))))
    (painter:union
     (painter:translate '(0 -10  24) c)
     (painter:translate '(0 -10  12) (painter:rotate 20 '(0 0 1) c))
     (painter:translate '(0 -10   0) (painter:rotate 40 '(0 0 1) c))
     (painter:translate '(0 -10 -12) (painter:rotate 60 '(0 0 1) c)))))

;;; translate

(define translate-001
  (let ((c (painter:cube attribute-001 '(10 10 10))))
    (painter:union
     (painter:translate '(0 -10  24) c)
     (painter:translate '(0 -10  12) c)
     (painter:translate '(0 -10   0) c)
     (painter:translate '(0 -10 -12) c))))

;;; transform

(define transform-001
  (let ((c (painter:cube attribute-001 '(10 10 10))))
    (painter:union
     (painter:translate '(-20 -10 0) c)
     (painter:translate '(0 -10 0)
                        (painter:transform
                         c
                         (make-3d-frame '(0 0 0)
                                        '(2 0 0)
                                        '(0.6 1.5 0)
                                        '(0 0.6 1.2)))))))
