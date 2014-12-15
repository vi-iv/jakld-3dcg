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

;;; Sierpinski tetrahedron

(define (painter:tetrahedron attribute origin size)
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
      (painter:polyhedron attribute
                          (map (lambda (p) (add (scl size p) origin)) ps)
                          ts))))

(define (painter:sierpinski-tetrahedron attribute size max-count)
  (define (sierpinski-tetrahedron% origin size counter)
    (if (<= counter 0)
        (painter:tetrahedron attribute origin (* 1.3 size)) ; need overlap
        (let ((s0 (/ 1.0 4.0))
              (s1 (/ 1.0 (* 4.0 (sqrt 2.0)))))
          (let ((o0 (add (scl size (list     s0 0.0 s1)) origin))
                (o1 (add (scl size (list (- s0) 0.0 s1)) origin))
                (o2 (add (scl size (list 0.0     s0 (- s1))) origin))
                (o3 (add (scl size (list 0.0 (- s0) (- s1))) origin))
                (s/2 (/ size 2)))
            (painter:union (sierpinski-tetrahedron% o0 s/2 (1- counter))
                           (sierpinski-tetrahedron% o1 s/2 (1- counter))
                           (sierpinski-tetrahedron% o2 s/2 (1- counter))
                           (sierpinski-tetrahedron% o3 s/2 (1- counter)))))))
  (sierpinski-tetrahedron% (list 0.0 0.0 0.0) size max-count))

(define sierpinski-tetrahedron-000
  (painter:sierpinski-tetrahedron attribute-001 40 0))

(define sierpinski-tetrahedron-001
  (painter:sierpinski-tetrahedron attribute-001 40 1))

(define sierpinski-tetrahedron-002
  (painter:sierpinski-tetrahedron attribute-001 40 2))

(define sierpinski-tetrahedron-003
  (painter:sierpinski-tetrahedron attribute-001 40 3))

(define sierpinski-tetrahedron-004
  (painter:sierpinski-tetrahedron attribute-001 40 4))

(define sierpinski-tetrahedron-004
  (painter:sierpinski-tetrahedron attribute-001 40 5))
