## 3DCG on JAKLD (jakld-3dcg)
* version 0.2

### 概要
* Scheme 処理系 JAKLD 上での 3 次元モデルの構成, 描画, 出力
* Structure and Interpretation of Computer Programs[1] による Picture Language の拡張

### 環境
* JAKLD[2]

### 使用法
* (load "load.scm")
* (start-picture)
* (show cube0) など

### サンプル
* sample.scm

### TODO
* 遅延評価しているので遅い: モード切替可能に
* 色が変: phonng-mode-shade まわりのデバッグ
* cylinder, intersection, defference
* polyhedron

### リンク
* [1] http://mitpress.mit.edu/sicp/full-text/book/book.html
* [2] http://www.yuasa.kuis.kyoto-u.ac.jp/~yuasa/jakld/index-j.html
