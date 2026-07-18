# hgg API リファレンス

公開 API を topic 別に網羅するリファレンス。 「どう学ぶか」 の学習導線は
[チュートリアル](../tutorials/README.md) が担い、 ここは **用語・網羅性重視の辞書**に振る。

> **用語**: 合成単位 = **layer**、 描画の種類 = **mark** (型 `MarkKind`・`scatter`/`line`/`bar`/…)、
> 図全体 = **`VisualSpec`**。 「geom」 は ggplot 利用者向けの相互参照としてのみ使う (本ライブラリ
> native の概念名は layer / mark)。 3D は別型系 ([08 3d](08-3d.md))。

## ページ一覧

| ページ | 内容 |
|---|---|
| [01 quickstart](01-quickstart.md) | 30 秒で 1 枚 + 書き方の 3 層 (Easy / Grammar / DataFrame) |
| [02 layers](02-layers.md) | レイヤとマーク (描けるグラフ一覧・mark カタログ) |
| [03 encoding & scale](03-encoding-scale.md) | channel (色 / サイズ / 形 / 固定色) + scale / palette + 軸 (position scale) |
| [04 decoration](04-decoration.md) | ラベル / theme / facet / subplot / 座標 / 参照線 / 重畳 / 高度な積層 |
| [05 backends](05-backends.md) | backend (SVG / PDF / PNG / Jupyter) の選び方と保存関数 |
| [06 dataframe](06-dataframe.md) | DataFrame 連携 (`df \|>> layer …`・nullable 列) |
| [07 analyze](07-analyze.md) | analyze 連携 (`toPlot` / `statLm` / HBM 抽出子) |
| [08 3d](08-3d.md) | 3D (別型系 `Layer3D` / `VisualSpec3D`・応答曲面・汎用 3D) |
| [09 appendix](09-appendix.md) | 付録 (層・ページ選択 / ggplot 移行 / ライブラリ拡張) + API 早見表 |
| [10 custom marks](10-custom-marks.md) | 自作 mark (`customMark`) — core 無改造で新プロット型を足す |

## 書き方の 3 層 (どの import でも下位層は全部使える)

| 層 | モジュール | 立ち位置 |
|---|---|---|
| **0. Quick** | `Hgg.Plot.Quick` | `IO` ワンショット。 `quickScatter "out.svg" xs ys` |
| **1. Easy** | `Hgg.Plot.Easy` | `[Double]` 直渡し + `overlay` 既定 |
| **2. Grammar** | `Hgg.Plot.Spec` | ggplot 同型の channel + `<>` 合成 (主役) |

## 2 つの黄金律

1. 図は **`purePlot <> layer (mark …) <> 設定 …`** の形。 `<>` で部品を足す。
2. `<>` は 2 階層。 mark・見た目は **`Layer`** を返し `layer (…)` の**中**、 タイトル・テーマ・facet 等は
   **`VisualSpec`** を返し `layer (…)` の**外**。 → [重畳の仕組み](04-decoration.md#overlay)。

## 演算子早見表

図を組み立てる演算子は 4 つだけ。 役割の違いはここを一次根拠にする (各ページは詳細のみ扱う)。

| 演算子 | 役割 | 詳細 |
|---|---|---|
| `<>` | spec / layer / 設定の**合成** (Monoid) | [2 つの黄金律](#2-つの黄金律) |
| `\|>` | DataFrame の**前方変換** (Hackage `dataframe`・groupBy/aggregate 等) | [06 dataframe](06-dataframe.md) |
| `\|>>` | df を図に**束ねる** (`BoundPlot` を作る純関数・ファイルは書かない) | [06 dataframe](06-dataframe.md#by-column-name) |
| `\|->` | df から列名で**モデルを fit** する | [07 analyze](07-analyze.md#fit-data) |

保存関数 (`saveSVG` / `saveSVGBound` / `saveSVGBoundStats` × SVG/PDF/PNG) は
[05 backends](05-backends.md#be-svg) に一覧がある。

## 関連ドキュメント

- **実例ギャラリー** ─ reference から実データ作例へ:
  [README ギャラリー](../../README.md#ギャラリー) (各図クリックで該当エントリへ)
- [チュートリアル (R4DS 再現等)](../tutorials/README.md) ─ 学習導線
- [R4DS 第 1 章](../tutorials/01-visualize/README.md) (penguins 全 24 図 + 再現コード)
