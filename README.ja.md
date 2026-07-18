# hgg — Haskell Grammar of Graphics

> 🌐 [English](README.ja.md) | **日本語**

> **状態: ドキュメント先行公開 (コードは公開予定)**
> 本リポジトリは現在 **API リファレンス等のドキュメントのみ**を公開しています。
> ライブラリ本体 (Haskell パッケージ) は追って公開します。

**hgg** は Haskell の宣言型作図ライブラリです。ggplot2 / Vega-Lite と同じ
**Grammar of Graphics** の発想で、 `purePlot <> layer (mark …) <> 設定 …` と
モノイド合成で図を組み立てます。統計ライブラリ
[**hanalyze**](https://hackage.haskell.org/package/hanalyze) と対になり
(hanalyze = 解析 / hgg = 可視化)、 回帰・GLM・GP・生存・時系列・ベイズ (HBM) など
fit 済みモデルをそのまま図に重ねられます。

## できること (抜粋)

- **layer / mark** ベースの宣言型 API (散布・線・棒・ヒスト・箱ひげ・violin・density・
  band・forest・heatmap・contour・vector field・DAG・MCMC 診断 …)
- **DataFrame 連携** — `df |>> layer (scatter "x" "y")` のように列名で書く
- **backend** — SVG / PDF / PNG (日本語フォント可) / Jupyter (iHaskell) inline
- **3D** — 応答曲面 (RSM)・汎用 3D プロット (CPU 投影 + WebGL)
- **統計連携** — `toPlot` / `statLm` / HBM 抽出子で hanalyze のモデルを描画

## ドキュメント

- 📚 **[API リファレンス](docs/api-guide/README.ja.md)** — topic 別の網羅リファレンス
- [チュートリアル](docs/tutorials/README.ja.md) — *R for Data Science 2e* の再現
- [Getting Started](docs/getting-started.ja.md) ／ [ggplot2 移行ガイド](docs/migration-from-ggplot.ja.md)
- [Vega-Lite との比較](docs/comparison-vega-lite.ja.md) ／ [モジュール構成](docs/modules.ja.md)

## ライセンス

[BSD-3-Clause](LICENSE) (hanalyze と同じ)。

---

*hgg — declarative Grammar-of-Graphics plotting for Haskell. コード本体は順次公開予定です。*
