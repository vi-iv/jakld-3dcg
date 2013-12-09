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

;;;; variable (model)

;;; camera and lights

(define camera-001
  (make-camera '(12.0 12.0 12.0)
               '(-1.0 -1.0 -1.0)
               '(60.0 60.0 200.0)))

(define camera-002
  (make-camera '(0.5 1.0 0.5)
               '(-1.0 -2.0 -1.0)
               '(7.5 7.5 20.0)))

(define camera-003
  (make-camera '(1.0 1.0 1.0)
               '(-1.0 -1.0 -1.0)
               '(7.5 7.5 20.0)))

(define lights-001
  (list (make-parallel-light '(-1.0 -1.0 -1.0)
                             (make-intensity 0.5 0.5 0.5))))

(set! *camera* camera-001)
(set! *lights* lights-001)

;;; color and attribute

(define attribute-001
  (make-attribute (hex->color #x000000)
                  (hex->color #x4169E1)
                  (hex->color #x4169E1)
                  (hex->color #xffffff)
                  (hex->color #x000000)
                  3))

(define attribute-002
  (make-attribute (hex->color #x000000)
                  (hex->color #x222222)
                  (hex->color #x222222)
                  (hex->color #xffffff)
                  (hex->color #x000000)
                  3))
