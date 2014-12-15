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

;;;; Practice 2
;;;; 基本的な図形を表す 3D ペインタを構成
;;;;
;;;; + 既存の手続き一覧
;;;;   - 直方体     (painter:cube <色> <各辺の長さのリスト>)
;;;;   - 球体       (painter:sphere <色> <半径の長さ> <ポリゴン分割の細かさ>)
;;;;   - 円柱・円錐 (painter:cylinder <色> <高さ> <上底の半径> <下底の半径> <ポリゴン分割の細かさ>)
;;;;   - 回転体     (painter:revolution <色> <xy 平面上での回転断面の頂点のリスト> <ポリゴン分割の細かさ>)

;;; 直方体

(define *cube*
  (painter:cube *attribute*   ;; 色等を表す変数
                '(10 15 20))) ;; 各辺の長さを表す3要素からなるリスト

;;; 球体

(define *sphere*
  (painter:sphere *attribute* ;; 色等を表す変数
                  10          ;; 半径の長さ
                  3))         ;; ポリゴン分割の細かさ

;; ポリゴン分割の細かさは 0 から徐々に大きくしていくとよい
;; なお球体表面のポリゴン分割を荒くすると見かけ上は正多面体に見えるが，
;; これを 3D プリンタで造型する場合には球体になるので要注意

;;; 円柱・円錐

(define *cylinder*
  (painter:cylinder *attribute* ;; 色等を表す変数
                    10          ;; 高さ
                    5           ;; 上底面の半径の長さ
                    5           ;; 下底面の半径の長さ
                    2))         ;; ポリゴン分割の細かさ

(define *cone*
  (painter:cylinder *attribute*
                    20
                    0           ;; 上下底の半径が不等のとき円錐や円錐台となる
                    5
                    2))

;;; 回転体

(define *revolution*
  (painter:revolution *attribute*
                      '((0 5 0) (5 5 0) (5 10 0) (0 10 0))
                      ;; xy 平面上での回転断面の頂点の座標のリスト
   5))                ;; ポリゴン分割の細かさ
