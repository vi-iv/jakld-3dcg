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

;;;; Practice 6
;;;; 独自の変換手続きの定義
;;;;
;;;; + 既存の手続き一覧
;;;;   - (make-3d-frame <原点の変換先> <(1,0,0) の変換先> <(0,1,0) の変換先> <(0,0,1) の変換先>)
;;;;   - (painter:transform <3D ペインタ> <3D フレーム>)

;;; 3D フレーム (アフィン変換行列に相当) の定義

(define *frame*
  (make-3d-frame '(0 0 0)
                 '(1 0 0)
                 '(0 1 0)
                 '(0.5 0 1)))

;;; 上記 3D フレームを適用した 3D ペインタの構成手続きを定義

(define (painter:skew painter)
  (painter:transform painter *frame*))

;;; 適用

(define *skew-red-cube*
  (painter:skew *red-cube*))
