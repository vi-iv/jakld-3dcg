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

;;; Helix

(define (painter:helix height radius begin interval end painters)
  (do ((ts (enumerate begin end interval) (cdr ts))
       (painters painters (cdr painters))
       (result nil))
      ((or (null? ts) (null? painters))
       (apply painter:union result))
    (let* ((theta (car ts))
           (painter (car painters))
           (x (* theta radius (cos theta)))
           (y (* theta radius (sin theta)))
           (z (* height (/ theta end))))
      (set! result
            (append1 result (painter:translate (list x y z) painter))))))

(define spiral-001
  (painter:helix 0 1 0 0.2 (* 10 +pi+)
                 (repeat (painter:cube attribute-001 (list 1 1 1)) 100)))

(define spiral-002
  (painter:helix 10 10 0 0.2 (* 2 +pi+)
                 (repeat (painter:cube attribute-001 (list 1 1 1)) 100)))
