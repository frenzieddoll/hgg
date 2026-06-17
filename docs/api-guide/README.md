# hgg API リファレンス

公開 API を topic 別に網羅するリファレンス。 「どう学ぶか」 の学習導線は
[チュートリアル](../tutorials/README.md) が担い、 ここは **用語・網羅性重視の辞書**に振る。

> **用語**: 合成単位 = **layer**、 描画の種類 = **mark** (型 `MarkKind`・`scatter`/`line`/`bar`/…)、
> 図全体 = **`VisualSpec`**。 「geom」 は ggplot 利用者向けの相互参照としてのみ使う (本ライブラリ
> native の概念名は layer / mark)。 3D は別型系 ([07 3d](07-3d.md))。

## ページ一覧

| ページ | 内容 |
|---|---|
| [01 quickstart](01-quickstart.md) | 30 秒で 1 枚 + 書き方の 3 層 (Easy / Grammar / DataFrame) |
| [02 layers](02-layers.md) | レイヤとマーク (描けるグラフ一覧) + encoding |
| [03 decoration](03-decoration.md) | ラベル / scale / theme / facet / subplot / 座標 / 参照線 / 重畳 / 高度な積層 |
| [04 backends](04-backends.md) | backend (SVG / PDF / PNG / Jupyter) の選び方と保存関数 |
| [05 dataframe](05-dataframe.md) | DataFrame 連携 (`df |>> layer …`・nullable 列) |
| [06 analyze](06-analyze.md) | analyze 連携 (`toPlot` / `statLm` / HBM 抽出子) |
| [07 3d](07-3d.md) | 3D (別型系 `Layer3D` / `VisualSpec3D`・応答曲面・汎用 3D) |
| [08 appendix](08-appendix.md) | 付録 (層・ページ選択 / ggplot 移行 / ライブラリ拡張) + API 早見表 |

## 書き方の 3 層 (どの import でも下位層は全部使える)

| 層 | モジュール | 立ち位置 |
|---|---|---|
| **0. Quick** | `Hgg.Plot.Quick` | `IO` ワンショット。 `quickScatter "out.svg" xs ys` |
| **1. Easy** | `Hgg.Plot.Easy` | `[Double]` 直渡し + `overlay` 既定 |
| **2. Grammar** | `Hgg.Plot.Spec` | ggplot 同型の channel + `<>` 合成 (主役) |
| **3. Typed** | `Hgg.Plot.Spec` + `Resolver` | 型付き channel / scale / Resolver で encoding 制御 |
| **4. Low-level** | `Hgg.Plot.Render` | `Primitive` 直書き (backend 自作・特殊描画) |

## 2 つの黄金律

1. 図は **`purePlot <> layer (mark …) <> 設定 …`** の形。 `<>` で部品を足す。
2. `<>` は 2 階層。 mark・見た目は **`Layer`** を返し `layer (…)` の**中**、 タイトル・テーマ・facet 等は
   **`VisualSpec`** を返し `layer (…)` の**外**。 → [重畳の仕組み](03-decoration.md#overlay)。

## 関連ドキュメント

- [チュートリアル (R4DS 再現等)](../tutorials/README.md) ─ 学習導線
- [modules.md](../modules.md) ─ モジュール構成の全体像
- [migration-from-ggplot.md](../migration-from-ggplot.md) ─ ggplot2 → hgg 移行表
- [comparison-vega-lite.md](../comparison-vega-lite.md) ─ Vega-Lite との比較
