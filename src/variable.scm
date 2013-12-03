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

;;;; Variables

;; (start-picture)

(define +origin+ (list 0.0 0.0 0.0))

(define +NL+ (string #\newline))

(define +nt-frame-full+
  (list (cons 0.0 0.0)
        (cons 1.0 0.0)
        (cons 0.0 1.0)))

(define +2d-frame-full+
  (make-2d-frame (list 0.0 0.0)
                 (list 1.0 0.0)
                 (list 0.0 1.0)))

(define +3d-frame-full+
  (make-3d-frame (list 0.0 0.0 0.0)
                 (list 1.0 0.0 0.0)
                 (list 0.0 1.0 0.0)
                 (list 0.0 0.0 1.0)))

(define *camera* nil)

(define *lights* nil)

(define *painter-filled?* #t)

(define *resolution-scale* 10)

(define *latest-model* nil)

(define *render-cache* nil)
