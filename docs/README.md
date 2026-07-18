# hgg ドキュメント

Haskell / PureScript 両対応の宣言型 plot library。 ここは利用者向けドキュメントの入口。

## 読む順番

0. **[tutorials/](./tutorials/README.md)** ─ R for Data Science 2e ベースの実践チュートリアル
   (mpg データで 読込→確認→変換→整形→可視化→モデル を 19 章で・タスク指向の入口)
1. **[getting-started.md](./getting-started.md)** ─ インストール、 最短で 1 枚出す Quick Start、 backend の選び方
2. **[API リファレンス (api-guide/)](./api-guide/README.md)** ─ 公開 API を topic 別に網羅 (layer/mark・装飾・backend・df・analyze・3D)
3. **[modules.md](./modules.md)** ─ パッケージ構成と「何ができるか」 の一覧 (chart 種・座標系・テーマ等)
4. **[comparison.md](./comparison.md)** ─ matplotlib / ggplot2 との対応・差分
5. **[migration-from-ggplot.md](./migration-from-ggplot.md)** ─ ggplot2 ユーザ向け逐語移行ガイド (関数の対応表)
6. **[comparison-vega-lite.md](./comparison-vega-lite.md)** ─ Vega-Lite との比較 (DataFrame 連携・hanalyze 統計エンジン連携の軸)
7. **[vega-lite-gallery.md](./vega-lite-gallery.md)** ─ Vega-Lite 例ギャラリーの再現 (コード付き 28 例 + 描けない例の分類)
8. **../design/gallery/README.md** ─ 視覚ギャラリー (SVG 30+、 各 chart の 1 行コード例付き)

## 実行可能サンプル

`hgg-svg/examples/` 配下に番号付きチュートリアルがある。 いずれも 1 ファイルで
完結し、 SVG を 1〜2 枚出力する:

| 実行コマンド | 内容 | doc |
|---|---|---|
| `cabal run tutorial-01-easy` | Easy API で散布図を 1 枚 | [api-guide §Easy](./api-guide/01-quickstart.md#easy) |
| `cabal run tutorial-02-grammar` | Grammar (ggplot 風) で色分け + scale | [api-guide §Grammar](./api-guide/01-quickstart.md#grammar) |
| `cabal run tutorial-03-overlay` | 複数 layer の重畳 | [api-guide §重畳](./api-guide/04-decoration.md#overlay) |
| `cabal run tutorial-04-distribution` | violin / box (群比較) | [modules §分布](./modules.md) |
| `cabal run tutorial-05-theme` | テーマ明/暗の出し分け | [modules §テーマ](./modules.md) |

> 出力先はカレントディレクトリ。 リポジトリでは `design/tutorial/` で実行した出力を参照用に置いている。
