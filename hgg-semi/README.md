# hgg-semi

半導体ドメイン特化チャートの add-on package (`hgg` ファミリ)。

`hgg-core` の backend 非依存 `Primitive` 列を生成するだけで、SVG / PDF /
PNG / Canvas 出力は既存 backend package がそのまま consume する。`hgg`
本体が「汎用 plot library」であるのに対し、本 package はドメイン特化機能のみを
上乗せする (package 境界を清潔に保つため別 package)。

## 収録チャート (Phase NN「半導体ドメイン特化チャート」)

| チャート | module | 状態 |
|---|---|---|
| WaferMap | `Hgg.Plot.Semi.WaferMap` | ✅ §A (die grid + bin 色塗り + edge 除外 + reticle 境界 + notch + yield/zone サマリ) |
| ControlChart | `Hgg.Plot.Semi.ControlChart` | ⚪ §B 予定 (X̄-R / I-MR / CUSUM / EWMA / WE・Nelson ルール) |
| ProbabilityPlot | `Hgg.Plot.Semi.ProbabilityPlot` | ⚪ §C 予定 (Q-Q / Weibull / log-normal + rank CI) |
| ParetoChart | `Hgg.Plot.Semi.ParetoChart` | ⚪ §D 予定 (件数 + 累積 % dual-Y) |
| BoxCoxPlot | `Hgg.Plot.Semi.BoxCoxPlot` | ⚪ §E 予定 (λ vs log-likelihood + 最適 λ マーク) |

## 使い方 (WaferMap)

```haskell
import Hgg.Plot.Semi.WaferMap
import Hgg.Plot.Backend.SVG (savePrimitivesSVG)

main :: IO ()
main = do
  let spec  = defaultWaferMapSpec 21 21 dies   -- 21x21 die grid
      (w,h) = waferMapViewport spec
  savePrimitivesSVG "wafer.svg" w h "#ffffff" (waferMapPrimitives spec)
  print (computeYield spec)                     -- yield + zone サマリ
```

## ライセンス

MIT (hgg 本体と同じ)。
