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

(define camera (make-camera '(1.0 1.0 1.0)
                            '(-1.0 -1.0 -1.0)
                            '(10.0 10.0 20.0)))

(define lights (list (make-parallel-light '(-1.0 -1.0 -1.0)
                                          (make-intensity 0.5 0.5 0.5))))

(set! *camera* camera)
(set! *lights* lights)

;;; color and attribute

(define *black* (make-color 0.0 0.0 0.0))
(define *white* (make-color 1.0 1.0 1.0))

(define *red*   (make-color 1.0 0.0 0.0))
(define *green* (make-color 0.0 1.0 0.0))
(define *blue*  (make-color 0.0 0.0 1.0))

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

;;; 2d model

(define letterlambda
  (polygon *black*
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

(define cube0
  (cube attribute0 (list 1.0 1.0 1.0)))

(define cube1
  (cube attribute0 (list 0.5 0.5 0.5)))

;; (show cube0)
;; (cube0 *2d-frame-left-half*)
;; (cube0 *2d-frame-rihgt-half*)

(define sphere0
  (sphere attribute0 1.0 20))

(define sphere1
  (sphere attribute0 1.0 20))

;; (show sphere0)
;; (sphere0 *2d-frame-left-half*)
;; (sphere0 *2d-frame-rihgt-half*)

;;; transform

