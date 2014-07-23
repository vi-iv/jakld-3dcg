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

;;;; Trivial Art

;;; Asagao

(define asagao:color:flower
  (make-attribute (hex->color #x000000)
                  (hex->color #x4169E1)
                  (hex->color #x4169E1)
                  (hex->color #xffffff)
                  (hex->color #x000000)
                  3))

(define asagao:color:sepal
  (make-attribute (hex->color #x000000)
                  (hex->color #x036b27)
                  (hex->color #x036b27)
                  (hex->color #xffffff)
                  (hex->color #x000000)
                  3))

(define asagao:color:leaf
  (make-attribute (hex->color #x000000)
                  (hex->color #x143d20)
                  (hex->color #x143d20)
                  (hex->color #x036b27)
                  (hex->color #x000000)
                  3))

(define (enumerate2 begin end difference-function)
  (do ((n begin (+ n (difference-function)))
       (result nil (append1 result n)))
      ((>= n end) result)))

(define asagao
  (painter:rotate
   180 '(-0.5 1 -0.15)
   (painter:translate
    '(-10 0 0)
    (painter:union
     ;; Hana
     (let ((from 0.02) (to 20) (width 0.2) (diff 0.01) (ratio 1.1))
       (let ((diff-func (lambda () (set! diff (* ratio diff)))))
         (let* ((x (enumerate2 from to diff-func))
                (y (map (lambda (x) (* 2 (log x))) x))
                (z (repeat 0 (length x))))
           (painter:revolution
            asagao:color:flower
            (let ((verts (map list x y z)))
              (map (lambda (p) (sub p (list 0 (last1 y) 0)))
                   (append verts
                           (reverse (map (lambda (p)
                                           (add p (list 0 width 0)))
                                         verts)))))
            3))))
     ;; Gaku
     (let ((from 14) (to 24) (width 0.1) (diff 0.01) (ratio 1.2))
       (let ((diff-func (lambda () (set! diff (* ratio diff)))))
         (let* ((x (enumerate2 from to diff-func))
                (y (map (lambda (x) (* 2.2 (log x))) x))
                (z (repeat 0 (length x))))
           (painter:revolution
            asagao:color:sepal
            (let ((verts (map list x y z)))
              (map (lambda (p) (sub p (list 0 (last1 y) 0)))
                   (append verts
                           (reverse (map (lambda (p)
                                           (add p (list 0 width 0)))
                                         verts)))))
            1))))
     ;; Ha
     (painter:translate
      '(0 -6 -2)
      (painter:rotate
       -30 '(-1 -0.2 -1)
       (painter:union
        (painter:translate
         '(24 0 -6)
         (painter:scale '(0.01 0.5 1.2)
                        (painter:sphere asagao:color:leaf 8 2)))
        (painter:rotate
         30 '(1 0 0)
         (painter:translate
          '(24 0 -4)
          (painter:scale '(0.01 0.5 1)
                         (painter:sphere asagao:color:leaf 8 2))))
        (painter:rotate
         -30 '(1 0 0)
         (painter:translate
          '(24 0 -4)
          (painter:scale '(0.01 0.5 1)
                         (painter:sphere asagao:color:leaf 8 2)))))))
     ;; Turu
     (painter:translate
      '(20 0 0)
      (painter:rotate
       90 '(1 1 0)
       (painter:helix
        14 2 0 0.02 (* 3 +pi+)
        (repeat (painter:cube asagao:color:leaf (list 0.4 0.4 0.4)) 1000))))
     ))))
