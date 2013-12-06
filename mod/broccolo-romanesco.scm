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

;;; Broccolo Romanesco

(define (painter:cone attribute origin direction height radius)
  (let ((small 0.4)
        (zaxis (list 0.0 0.0 1.0)))
    (if (and (> radius small) (> height small))
        (painter:translate
         origin
         (painter:rotate
          (radian->degree (acos (cos-angle zaxis direction)))
          (cross zaxis direction)
          (painter:rotate
           (acos (x direction))
           zaxis
           (painter:cylinder attribute height 0 radius (sqrt radius))))))))

(define (painter:cone-helix height radius begin interval end)
  (do ((ts (enumerate begin end interval) (cdr ts))
       (result nil))
      ((null? ts) (apply painter:union result))
    (let* ((theta (car ts))
           (x0 (cos theta))
           (y0 (sin theta))
           (z0 (if (= height 0) 0 (/ radius height)))
           (x1 (* theta radius x0))
           (y1 (* theta radius y0))
           (z1 (* height (/ theta end)))
           (position (list x1 y1 z1))
           (direction (list x0 y0 z0)))
      (let ((painter (painter:cone attribute0 position direction 5 2)))
        (set! result (append1 result painter))))))

(define cone-helix-001
  (painter:cone-helix 0 5 0 0.2 (* 2 +pi+)))

(define cone-helix-001
  (painter:cone-helix 0 3 0 0.1 (* 4 +pi+)))

(define (painter:broccolo-romanesco attribute height radius max-count)
  (define (broccolo-romanesco% origin direction height radius counter)
    (if (<= counter 0)
        (painter:cone attribute origin direction height radius)
        (let ((t0 0)
              (t1 (* 2 +pi+))
              (dt (/ +pi+ 8))
              (scale 0.4))
          (do ((ts (enumerate t0 t1 dt) (cdr ts))
               (result nil))
              ((null? ts) (apply painter:union result))
            (let* ((theta (car ts))
                   (x0 (cos theta))
                   (y0 (sin theta))
                   (z0 (/ radius height))
                   (x1 (* (/ theta t1) radius x0))
                   (y1 (* (/ theta t1) radius y0))
                   (z1 (* (/ theta t1) height))
                   (new-origin (add origin (list x1 y1 z1)))
                   (new-direction (add direction (list x0 y0 (- z0)))))
              (let ((painter (broccolo-romanesco% new-origin
                                                  new-direction
                                                  (* scale (/ theta t1) height)
                                                  (* scale (/ theta t1) radius)
                                                  (1- counter))))
                (if (not (null? painter))
                    (set! result (append1 result painter)))))))))
  (broccolo-romanesco% (list 0.0 0.0 0.0)
                       (list 0.0 0.0 1.0)
                       height radius max-count))
