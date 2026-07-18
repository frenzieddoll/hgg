# 付録

> [📚 索引](README.md) ｜ [01 quickstart](01-quickstart.md) ｜ [02 layers](02-layers.md) ｜ [03 encoding & scale](03-encoding-scale.md) ｜ [04 decoration](04-decoration.md) ｜ [05 backends](05-backends.md) ｜ [06 dataframe](06-dataframe.md) ｜ [07 analyze](07-analyze.md) ｜ [08 3d](08-3d.md) ｜ **09 appendix**

このページの構成:
**[A: どの層・どのページを使うか](#appendix-layers)** ｜ **[B: ggplot2 ユーザの方へ](#appendix-ggplot)** ｜
**[C: ライブラリを拡張する人へ](#appendix-extend)**

## A: どの層・どのページを使うか {#appendix-layers}

```
1 枚だけ・REPL                          → 01 quickstart (Quick / Easy)
群で色分け・複数 mark・ggplot 経験あり    → 01 quickstart (Grammar) + 02 layers / 04 decoration
2 次元場 (contour / bin2d)               → 02 layers (mark 一覧)
backend の選択・保存関数                  → 05 backends
CSV / DataFrame を列名で                 → 06 dataframe (df |>> …)
回帰・GLM・HBM を描く                     → 07 analyze (analyze 連携)
3D 応答曲面 (surface / 床面 contour)     → 08 3d (hgg-3d)
連続値 gradient / scale 制御             → 04 decoration (scale) + Resolver
新 backend / mark に無い描画             → 付録 C (ライブラリ拡張)
```

## B: ggplot2 ユーザの方へ {#appendix-ggplot}

ggplot2 の概念との対応は各ページに **geom / aes との相互参照** として記載している
([02 layers](02-layers.md) の mark カタログ・[03 encoding & scale](03-encoding-scale.md) の channel・
[04 decoration](04-decoration.md) の theme/scale/facet)。 `aes()` に当たるのが mark 内の
`colorBy`/`shapeBy`/…、 `+` に当たるのが `<>`、 `geom_*()` に当たるのが `scatter`/`bar`/… の mark。

## C: ライブラリを拡張する人へ (backend / 新 mark) {#appendix-extend}

通常の作図では不要。 新しい出力 backend を書く、 既存 mark に無い描画をする等、
**ライブラリ自体を拡張する**ときだけ読む。

- **backend を書く** = `Hgg.Plot.Render` の
  `renderToPrimitives :: Resolver -> Layout -> VisualSpec -> [Primitive]` で spec を幾何プリミティブ列
  (`PLine` / `PRect` / `PCircle` / `PPath` / `PText` / `PClipPush`/`PClipPop` / `PTransformPush`/`PTransformPop`)
  に落とし、 それを対象フォーマットへ**畳む**だけ。 **雛形 = `hgg-svg` の `Backend/SVG.hs`**
  (`renderToPrimitives` → `primsToSvg` で文字列化し header/footer で包む)。 Primitive 列 → SVG の既製
  helper `renderPrimitivesSVG` / `savePrimitivesSVG` も export 済。
- **`Primitive` を直接組む** = backend に無い特殊描画をするとき手で構築する。

