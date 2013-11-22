;;;; Sample

;;; frame

(define *2d-frame-left-half*
  (make-2d-frame (list 0.0 0.0)
                 (list 0.5 0.0)
                 (list 0.0 1.0)))

(define *2d-frame-right-half*
  (make-2d-frame (list 0.5 0.0)
                 (list 1.0 0.0)
                 (list 0.5 1.0)))

(define *2d-frame-lower-half*
  (make-2d-frame (list 0.0 0.0)
                 (list 1.0 0.0)
                 (list 0.0 0.5)))

(define *2d-frame-upper-half*
  (make-2d-frame (list 0.0 0.5)
                 (list 1.0 0.5)
                 (list 0.0 1.0)))

;;; camera and lights

(define camera0 (make-camera '(10.0 10.0 10.0)
                             '(-1.0 -1.0 -1.0)
                             '(60.0 60.0 200.0)))

(define camera1 (make-camera '(0.5 1.0 0.5)
                             '(-1.0 -2.0 -1.0)
                             '(7.5 7.5 20.0)))

(define camera2 (make-camera '(1.0 1.0 1.0)
                             '(-1.0 -1.0 -1.0)
                             '(7.5 7.5 20.0)))

(define lights0 (list (make-parallel-light '(-1.0 -1.0 -1.0)
                                          (make-intensity 0.5 0.5 0.5))))

(set! *camera* camera0)
(set! *lights* lights0)

;;; color and attribute

(define attribute0
  (make-attribute (hex->color #x000000)
                  (hex->color #x4169E1)
                  (hex->color #x4169E1)
                  (hex->color #xffffff)
                  (make-color 0 0 0)
                  3))

(define attribute1
  (make-attribute (hex->color #x000000)
                  (hex->color #x222222)
                  (hex->color #x222222)
                  (hex->color #xffffff)
                  (make-color 0 0 0)
                  3))

(define attribute3 ;; for 2d-model
  (make-attribute (make-color 1.0 0.0 0.0)
                  nil
                  nil
                  nil
                  nil
                  nil))

;;; 2d model

(define letterlambda
  (polygon attribute3
           (list (list .45 .6)
                 (list .25 .2)
                 (list .2 .2)
                 (list .2 .1)
                 (list .3 .1)
                 (list .5 .5)
                 (list .7 .1)
                 (list .8 .1)
                 (list .8 .2)
                 (list .75 .2)
                 (list .4 .9)
                 (list .3 .9)
                 (list .3 .8)
                 (list .35 .8))))

;; (letterlambda *2d-frame-full*)
;; (letterlambda *2d-frame-left-half*)
;; (letterlambda *2d-frame-right-half*)

;;; 3d model

;; (define cube0
;;   (cube attribute0 (list 10.0 10.0 10.0)))

;; (define cube1
;;   (cube attribute0 (list 5.0 5.0 5.0)))

;; (show cube0)
;; (cube0 *2d-frame-left-half*)
;; (cube0 *2d-frame-rihgt-half*)

(define sphere0
  (sphere attribute0 10.0 2))

;; (define sphere1
;;   (sphere attribute0 10.0 4))

;; (show sphere0)
;; (sphere0 *2d-frame-left-half*)
;; (sphere0 *2d-frame-rihgt-half*)

;; (define cylinder0
;;   (cylinder attribute0 10.0 5.0 5.0 2))

;; (define cylinder1
;;   (cylinder attribute0 20.0 0.0 5.0 2))

;;; transform

;; (define cubes0
;;   (translate '(-10.0 -10.0 -10.0)
;;              (union cube1
;;                     (translate '(10.0  0.0  0.0) cube1)
;;                     (translate '( 0.0  0.0 10.0) cube1)
;;                     (translate '(10.0  0.0 10.0) cube1)
;;                     (translate '(20.0  0.0  0.0) cube1)
;;                     (translate '( 0.0  0.0 20.0) cube1)
;;                     (translate '(20.0  0.0 20.0) cube1)
;;                     (translate '(20.0  0.0 10.0) cube1)
;;                     (translate '(10.0  0.0 20.0) cube1)
;;
;;                     (translate '( 0.0  0.75  0.0) cube1)
;;                     (translate '(10.0  0.75  0.0) cube1)
;;                     (translate '( 0.0  0.75 10.0) cube1)
;;                     (translate '(10.0  0.75 10.0) cube1)
;;                     (translate '(20.0  0.75  0.0) cube1)
;;                     (translate '( 0.0  0.75 20.0) cube1)
;;                     (translate '(20.0  0.75 20.0) cube1)
;;                     (translate '(20.0  0.75 10.0) cube1)
;;                     (translate '(10.0  0.75 20.0) cube1))))

;; (define spheres0
;;   (union (translate '(10.0 0.0 0.0) sphere0)
;;          (translate '(0.0 0.0 10.0) sphere0)
;;          (translate '(-10.0 0.0 0.0) sphere0)
;;          (translate '(0.0 0.0 -10.0) sphere0)))

;; (define cylinders0
;;   (union (translate '(10.0 0.0 0.0) cylinder1)
;;          (translate '(0.0 0.0 10.0) cylinder1)
;;          (translate '(-10.0 0.0 0.0) cylinder1)
;;          (translate '(0.0 0.0 -10.0) cylinder1)))

;; (define cylinders1
;;   (let ((cyl (translate '(0.0 0.0 0.5) cylinder1)))
;;     (union cyl
;;            (rotate 90 '(1.0 1.0 1.0) cyl)
;;            (rotate 180 '(1.0 1.0 1.0) cyl)
;;            (rotate 270 '(1.0 1.0 1.0) cyl))))

;; (define cylinders2
;;   (union
;;    (rotate 90 '(1.0 0.0 0.0) cylinders0)
;;    (rotate 90 '(1.0 0.0 0.0) (translate '(0.0 20.0 0.0) cylinders0))
;;    (rotate 90 '(1.0 0.0 0.0) (translate '(0.0 -20.0 0.0) cylinders0))))

;;; Menger sponge

(define (menger-sponge1 attribute size max-count . height)
  (define (insert-y xz y)
    (list (car xz) y (cadr xz)))
  (define (menger-sponge1-iter origin size counter)
    (let ((s/3 (map (lambda (x) (/ x 3)) size)))
      (let ((x1 (x s/3)) (x2 (* 2 (x s/3)))
            (y1 (y s/3)) (y2 (* 2 (y s/3))))
        (if (= counter 0)
            (list origin)
            (append (menger-sponge1-iter origin s/3 (1- counter))
                    (menger-sponge1-iter (add origin (list x1 0.0)) s/3 (1- counter))
                    (menger-sponge1-iter (add origin (list x2 0.0)) s/3 (1- counter))
                    (menger-sponge1-iter (add origin (list 0.0 y1)) s/3 (1- counter))
                    ;;(menger-sponge1-iter (add origin (list x1  y1)) s/3 (1- counter))
                    (menger-sponge1-iter (add origin (list x2  y1)) s/3 (1- counter))
                    (menger-sponge1-iter (add origin (list 0.0 y2)) s/3 (1- counter))
                    (menger-sponge1-iter (add origin (list x1  y2)) s/3 (1- counter))
                    (menger-sponge1-iter (add origin (list x2  y2)) s/3 (1- counter)))))))
  (let* ((cube-size% (map (lambda (x) (/ x (expt 3 max-count))) size))
         (height (if (null? height) (car cube-size%) (car height)))
         (cube-size (insert-y cube-size% height)))
    (apply union
           (map (lambda (orig)
                  (translate (insert-y orig 0.0) (cube attribute cube-size)))
                (menger-sponge1-iter (list 0.0 0.0) size max-count)))))

;; (define sponge0
;;   (translate '(-10.0 -15.0 -10.0) (menger-sponge1 attribute0 '(30.0 30.0) 1)))
;; (define sponge1
;;   (translate '(-10.0 -15.0 -10.0) (menger-sponge1 attribute0 '(30.0 30.0) 2)))
;; (define sponge2
;;   (translate '(-10.0 -15.0 -10.0) (menger-sponge1 attribute0 '(30.0 30.0) 3)))

;; (set! *camera* camera1)

;; (define sponge3
;;   (translate '(-30.0 -16.0 -30.0) (menger-sponge1 attribute0 '(50.0 50.0) 1 1)))
;; (define sponge4
;;   (translate '(-30.0 -16.0 -30.0) (menger-sponge1 attribute0 '(50.0 50.0) 2 1)))
;; (define sponge5
;;   (translate '(-30.0 -16.0 -30.0) (menger-sponge1 attribute0 '(50.0 50.0) 3 1)))

(define (menger-sponge attribute size max-count)
  (define (menger-sponge% origin size counter)
    (if (<= counter 0)
        (translate origin (cube attribute (list size size size)))
        (let* ((s/3 (/ size 3.0))
               (2s/3 (* 2 (/ size 3.0)))
               (unit (list 0.0 s/3 2s/3))
               (void (list 4 10 12 13 14 16 22))
               (next (lambda (o) (menger-sponge% (add o origin) s/3 (1- counter)))))
          (let iter ((x 3) (y 3) (z 3) (number 0) (result nil))
            (cond ((= z 0) (apply union (map next result)))
                  ((= y 0) (iter x 3 (1- z) number result))
                  ((= x 0) (iter 3 (1- y) z number result))
                  (else (iter (1- x) y z (1+ number)
                              (if (member number void)
                                  result
                                  (cons (map (lambda (n) (list-ref unit (1- n)))
                                             (list x y z) )
                                        result)))))))))
  (menger-sponge% (list 0.0 0.0 0.0) size max-count))

;; Sierpinski tetrahedron

(define (tetrahedron attribute origin size)
  (let ((s0 (/ 1.0 2.0))
        (s1 (/ 1.0 (* 2.0 (sqrt 2.0)))))
    (let ((ps (list (list     s0 0.0 s1)
                    (list (- s0) 0.0 s1)
                    (list 0.0     s0 (- s1))
                    (list 0.0 (- s0) (- s1))))
          (ts (list (list 0 1 3)
                    (list 0 2 1)
                    (list 0 3 2)
                    (list 1 2 3))))
      (polyhedron attribute
                  (map (lambda (p) (add (scl size p) origin)) ps)
                  ts))))

(define (sierpinski-tetrahedron attribute size max-count)
  (define (sierpinski-tetrahedron% origin size counter)
    (if (<= counter 0)
        (tetrahedron attribute origin (* 1.3 size)) ; need overlap
        (let ((s0 (/ 1.0 4.0))
              (s1 (/ 1.0 (* 4.0 (sqrt 2.0)))))
          (let ((o0 (add (scl size (list     s0 0.0 s1)) origin))
                (o1 (add (scl size (list (- s0) 0.0 s1)) origin))
                (o2 (add (scl size (list 0.0     s0 (- s1))) origin))
                (o3 (add (scl size (list 0.0 (- s0) (- s1))) origin))
                (s/2 (/ size 2)))
            (union (sierpinski-tetrahedron% o0 s/2 (1- counter))
                   (sierpinski-tetrahedron% o1 s/2 (1- counter))
                   (sierpinski-tetrahedron% o2 s/2 (1- counter))
                   (sierpinski-tetrahedron% o3 s/2 (1- counter)))))))
  (sierpinski-tetrahedron% (list 0.0 0.0 0.0) size max-count))

(define (sierpinski-tetrahedron-sphere attribute size max-count)
  (define (sierpinski-tetrahedron% origin size counter)
    (if (<= counter 0)
        (translate origin (sphere attribute (/ size 2) 2))
        (let ((s0 (/ 1.0 4.0))
              (s1 (/ 1.0 (* 4.0 (sqrt 2.0)))))
          (let ((o0 (add (scl size (list     s0 0.0 s1)) origin))
                (o1 (add (scl size (list (- s0) 0.0 s1)) origin))
                (o2 (add (scl size (list 0.0     s0 (- s1))) origin))
                (o3 (add (scl size (list 0.0 (- s0) (- s1))) origin))
                (s/2 (/ size 2)))
            (union (sierpinski-tetrahedron% o0 s/2 (1- counter))
                   (sierpinski-tetrahedron% o1 s/2 (1- counter))
                   (sierpinski-tetrahedron% o2 s/2 (1- counter))
                   (sierpinski-tetrahedron% o3 s/2 (1- counter)))))))
  (sierpinski-tetrahedron% (list 0.0 0.0 0.0) size max-count))
