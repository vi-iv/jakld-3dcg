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

;;;; Practice 5
;;;; その他，3Dモデルを描画する際の設定など
;;;;
;;;; + 既存の手続き一覧
;;;;   - (make-simple-attribute <色>)
;;;;   - (make-camera <視点の位置> <視線の方向> <視野の範囲>)
;;;;   - (make-simple-light <平行光線の方向>)

;;;; 色

(define *red*
  (make-simple-attribute #xff0000))

(define *red-cube*
  (painter:cube *red* '(5 5 5)))

;;(show *red-cube*)

;;;; 視点の位置・方向

;;(set! *camera*
;;      (make-camera '(0 0 20)
;;                   '(0 0 -1)
;;                   '(80 80 100)))

;;;; 光源 (光線) の方向

;;(set! *lights*
;;      (list (make-simple-light '(1 0 0))))
