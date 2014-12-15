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

;;; Hirbert Curve

(define (painter:cube-center attribute origin size)
  (let ((s/2 (/ size 2)))
    (painter:translate
     origin
     (painter:translate (map - (list s/2 s/2 s/2))
                        (painter:cube attribute
                                      (list size size size))))))

(define (painter:hilbert-curve attribute size max-count)
  (define (cube% size)
    (painter:cube-center attribute '(0 0 0) size))
  (define (s% p)
    (painter:scale (list 0.5 0.5 0.5) p))
  (define (r% a i p)
    (painter:rotate a (list-ref '((1 0 0) (0 1 0) (0 0 1)) i) p))
  (define (t% v s p)
    (painter:translate (scl (/ s 2) v) p))
  (define (hilbert-curve% counter)
    (if (<= counter 1)
        (cube% size)
        (let ((c (1- counter))
              (s (* 2 size (expt 0.5 counter)))
              (l (* (- 1 (expt 0.5 (1- counter))) size)))
          (painter:union
           (t% '( 1  1  1) size (s% (r%  90 1 (hilbert-curve% c))))
           (t% '( 1  1 -1) size (s% (r% 270 1 (hilbert-curve% c))))
           (t% '( 1 -1  1) size (s% (r% 180 0 (r%  90 2 (hilbert-curve% c)))))
           (t% '(-1  1  1) size (s% (r%  90 1 (hilbert-curve% c))))
           (t% '( 1 -1 -1) size (s% (r% 180 1 (r% 270 2 (hilbert-curve% c)))))
           (t% '(-1  1 -1) size (s% (r% 270 1 (hilbert-curve% c))))
           (t% '(-1 -1  1) size (s% (r%  90 0 (r% 270 2 (hilbert-curve% c)))))
           (t% '(-1 -1 -1) size (s% (r% 270 0 (r% 270 1 (hilbert-curve% c)))))
           (painter:translate (list 0 s l) (cube% s))
           (painter:translate (list 0 s (- l)) (cube% s))
           (painter:translate (list l 0 l) (cube% s))
           (painter:translate (list l 0 (- l)) (cube% s))
           (painter:translate (list l (- s) 0) (cube% s))
           (painter:translate (list (- l) 0 l) (cube% s))
           (painter:translate (list (- l) 0 (- l)) (cube% s))))))
  (hilbert-curve% (1+ max-count)))

(define hilbert-curve-000
  (painter:hilbert-curve attribute-001 20 0))

(define hilbert-curve-001
  (painter:hilbert-curve attribute-001 20 1))

(define hilbert-curve-002
  (painter:hilbert-curve attribute-001 20 2))

(define hilbert-curve-003
  (painter:hilbert-curve attribute-001 20 3))

;;(define hilbert-curve-004
;;  (painter:hilbert-curve attribute-001 5 4))
