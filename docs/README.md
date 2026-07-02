# hgg ドキュメント

Haskell / PureScript 両対応の宣言型 plot library。 ここは利用者向けドキュメントの入口。
読み物は 2 つ ── **学ぶならチュートリアル、 引くなら API リファレンス** に集約している。

## チュートリアル (学習導線)

**[tutorials/](./tutorials/README.md)** ─ R for Data Science 2e ベースの実践チュートリアル。
読込 → 確認 → 変換 → 整形 → 可視化 → モデル をタスク指向で各章をなぞる。 まずここから。

- [01 データ可視化 (R4DS Ch.1)](./tutorials/01-visualize/README.md) ─ penguins 全 24 図 + 再現コード

## API リファレンス (辞書)

**[api-guide/](./api-guide/README.md)** ─ 公開 API を topic 別に網羅する辞書。 用語・網羅性重視。

| ページ | 内容 |
|---|---|
| [01 quickstart](./api-guide/01-quickstart.md) | 30 秒で 1 枚 + 書き方の 3 層 (Easy / Grammar / DataFrame) |
| [02 layers](./api-guide/02-layers.md) | レイヤとマーク (mark カタログ) |
| [03 encoding & scale](./api-guide/03-encoding-scale.md) | channel (色 / サイズ / 形) + scale / palette + 軸 |
| [04 decoration](./api-guide/04-decoration.md) | ラベル / theme / facet / subplot / 座標 / 参照線 / 重畳 |
| [05 backends](./api-guide/05-backends.md) | backend (SVG / PDF / PNG / Jupyter) と保存関数 |
| [06 dataframe](./api-guide/06-dataframe.md) | DataFrame 連携 (`df \|>> layer …`・nullable 列) |
| [07 analyze](./api-guide/07-analyze.md) | analyze 連携 (`toPlot` / `statLm` / HBM 抽出子) |
| [08 3d](./api-guide/08-3d.md) | 3D (別型系・応答曲面・汎用 3D) |
| [09 appendix](./api-guide/09-appendix.md) | 付録 (層・ページ選択 / ggplot 移行 / 拡張) + API 早見表 |
