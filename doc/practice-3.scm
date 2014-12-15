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

;;;; Practice 3
;;;; 基本的な変換や合成 (和) を適用した 3D ペインタを構成
;;;;
;;;; + 既存の手続き一覧
;;;;   - 拡大・縮小 (painter:scale <各軸方向の倍率> <3D ペインタ>)
;;;;   - 平行移動   (painter:translate <変位> <3D ペインタ>)
;;;;   - 回転       (painter:rotate <回転する角度> <回転軸> <3D ペインタ>)
;;;;   - 合成       (painter:union <3D ペインタ> ...)

;;; 拡大・縮小

(define *disk1*
  (painter:scale '(1 0.5 1)                  ;; y 軸方向にのみ縮小
                 (painter:sphere *attribute* ;; 半径 10 の球体を構成
                                 10
                                 3)))

(define *disk2*
  (painter:scale '(0.5 1 1)                  ;; x 軸方向にのみ縮小
                 (painter:sphere *attribute* ;; *disk1* 同様
                                 10
                                 3)))

;;; 平行移動

(define *translate*
  (painter:translate '(0 10 0) ;; 変位 (0,10,0)
                     *disk1*)) ;; *disk1* を平行移動

;;; 回転

(define *rotate*
  (painter:rotate 45        ;; 45 度回転
                  '(0 0 1)  ;; 回転軸を方向ベクトル (0,0,1) として指定
                  *disk1*)) ;; *disk1* を回転

;;; 合成

(define *union*
  (painter:union *disk1* *disk2*)) ;; *disk1* と *disk2* を合成
