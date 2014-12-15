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

;;; Menger sponge

(define (painter:menger-sponge attribute size max-count)
  (define (menger-sponge% origin size counter)
    (if (<= counter 0)
        (painter:translate origin (painter:cube attribute (list size size size)))
        (let* ((s/3 (/ size 3.0))
               (2s/3 (* 2 (/ size 3.0)))
               (unit (list 0.0 s/3 2s/3))
               (void (list 4 10 12 13 14 16 22))
               (next (lambda (o) (menger-sponge% (add o origin) s/3 (1- counter)))))
          (let iter ((x 3) (y 3) (z 3) (number 0) (result nil))
            (cond ((= z 0) (apply painter:union (map next result)))
                  ((= y 0) (iter x 3 (1- z) number result))
                  ((= x 0) (iter 3 (1- y) z number result))
                  (else (iter (1- x) y z (1+ number)
                              (if (member number void)
                                  result
                                  (cons (map (lambda (n) (list-ref unit (1- n)))
                                             (list x y z) )
                                        result)))))))))
  (menger-sponge% (list 0.0 0.0 0.0) size max-count))

(define menger-sponge-000
  (painter:menger-sponge attribute-001 10 0))

(define menger-sponge-001
  (painter:menger-sponge attribute-001 10 1))

(define menger-sponge-002
  (painter:menger-sponge attribute-001 10 2))

(define menger-sponge-003
  (painter:menger-sponge attribute-001 10 3))
