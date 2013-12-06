## 3DCG on JAKLD (jakld-3dcg)
* version 0.3.2

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
    * (redraw)      ; 再描画 (高速)
* 保存
    * (export "model.scm") ; 直前のモデルを S 式として保存
    * (export "model.scad" 'scad) ; 同, SCAD スクリプト[3]として保存
* 読込み
    * (import "model.scm") ; 読込は S 式のみで可

### サンプル
* sample.scm (load.scm でロード済み)
    * (show cube0)
    * (show sphere0)
    * (show cylinder0)
    * ...

### 備考
* 保存形式は S 式または SCAD スクリプト
* 立体の表面のポリゴン分割は SCAD と互換性なし

### TODO
* マニュアル書く?
* phong-mode-shade 周辺にバグ (色が変)
* 未実装: intersection, defference, ...
* Broccolo Romanesco

### リンク
* [1] http://mitpress.mit.edu/sicp/full-text/book/book.html
* [2] http://www.yuasa.kuis.kyoto-u.ac.jp/~yuasa/jakld/index-j.html
* [3] http://www.openscad.org/
