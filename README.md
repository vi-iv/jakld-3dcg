## 3DCG on JAKLD (jakld-3dcg)
* version 0.2.3

### 概要
* Structure and Interpretation of Computer Programs[1] による Picture Language の応用
* Scheme 処理系 JAKLD[2] 上での 3 次元モデルの構成, 描画, 保存

### 環境
* JAKLD
    * vertexes->painter などに依存

### 使用法
* 必要なファイルをロード
    * (start-picture)
    * (load "load.scm")
* 視点や証明を定義して大域変数に設定
    * (define camera ...)
    * (set! \*camera\* camera)
    * (define lights ...)
    * (set! \*lights\* lights)
    * (set! \*painter-filled?\* #t) ; ソリッド
    * (set! \*painter-filled?\* #f) ; ワイヤフレーム
* モデルの表面属性を定義
    * (define attribute ...)
* モデルを定義
    * (define model ...)
* (2D|3D) フレームを定義
    * (define frame ...)
* 描画
    * (model frame) ; フレーム指定あり
    * (show model)  ; フレーム指定なし(描画可能範囲全体を使用)
* 保存
    * (export model "model.scm") ; S 式として保存
    * (export model "model.scad" 'scad) ; SCAD スクリプト[3]として保存
* 読込み
    * (import model "model.scm") ; 読込は S 式のみで可

### サンプル
* sample.scm (load.scm でロード済み)
    * (show cube0)
    * (show sphere0)
    * (show cylinder0)
    * ...

### 備考
* 保存形式は S 式または SCAD スクリプト
* 描画時に負荷が高いのは仕様 (計算時間大)
* 描画時の 3D フレーム指定を不可能とするかわりに遅延評価しない版: modeling2.scm
    * (load "modeling2.scm")
    * この版ではモデル定義時に負荷が高い
    * 更に視点, 証明はモデル定義時のもの (描画時ではなく) が使用される
* 標準で modeling2.scm をロードするようにした
* 立体の表面のポリゴン分割は SCAD と互換性なし

### TODO
* マニュアル書く
* phong-mode-shade 周辺にバグ (色が変)
* 未実装: intersection, defference, ...

### リンク
* [1] http://mitpress.mit.edu/sicp/full-text/book/book.html
* [2] http://www.yuasa.kuis.kyoto-u.ac.jp/~yuasa/jakld/index-j.html
* [3] http://www.openscad.org/
