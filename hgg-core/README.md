# hgg-core

Layer 1-3 of [`hgg`](../README.md) — backend 非依存の plot spec / data / layout / render primitive。

依存は `base` / `vector` / `text` / `containers` のみ。 backend (SVG / PDF /
Rasterific / Canvas) は別 package で実装。

## モジュール構成

| Module | Layer | 役割 |
|---|---|---|
| `Hgg.Plot.Spec` | 3 | `VisualSpec` ADT (全 chart 種の宣言型仕様) |
| `Hgg.Plot.Data` | 3 | `PlotData` ADT (Vector ベース、 DataFrame 非依存) |
| `Hgg.Plot.Layout` | 2 | `computeLayout` 純粋関数 (viewport / scale / axis) |
| `Hgg.Plot.Render` | 1 | `Primitive` ADT + `Renderer` class (backend interface) |
| `Hgg.Plot.Easy` | 4 | matplotlib 風 `scatter` / `bar` / `line` / `histogram` |

## Phase 26 進捗

- [x] §A-1 package 構造 / cabal / smoke test
- [ ] §A-2 VisualSpec ADT 各 *Spec record fill
- [ ] §A-3 PlotData ADT 完成
- [ ] §A-4 Layout 計算 (Scale / Tick / 余白)
- [ ] §A-5 renderToPrimitives 各 chart 種別実装
