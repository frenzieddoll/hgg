# Getting Started

> 🌐 [English](getting-started.md) | **日本語**

## インストール

> ⚠️ hgg は **まだ Hackage / npm 未公開** (OSS 公開は 正式版 リリース後)。
> 現状はリポジトリ内のローカルパッケージとして利用する。

`cabal.project` に各パッケージへの `packages:` 行があり、 `cabal build` で全体が解決される。
自分のプロジェクトから使う場合は、 必要なパッケージを `build-depends` に足す:

```cabal
build-depends:
    hgg-core    -- Spec / Layout / Palette / DAG (純 Haskell、 base+vector+text+containers のみ)
  , hgg-svg     -- SVG backend (saveSVG で書き出す場合)
```

PureScript (frontend) 側は `hgg-canvas` を spago 依存に追加する (Halogen / web-canvas)。

## Quick Start ─ 最短で 1 枚

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Hgg.Easy                 -- Layer 1: 入門 API (Spec 全体も再 export)
import Graphics.Hgg.Backend.SVG (saveSVG)

main :: IO ()
main = saveSVG "quick.svg" $
     overlay [ points [1,2,3,4,5] [1,4,9,16,25] ]   -- [Double] を直接渡す
  <> title "y = x²"
  <> xLabel "x" <> yLabel "y"
  <> width 600 <> height 400
```

```bash
cabal run tutorial-01-easy    # 上とほぼ同じ内容を実行 → tutorial-01-easy.svg
```

ポイント:

- **副作用は最後の `saveSVG` だけ**。 図は純粋値 (`VisualSpec`) として組み立てる。
- **`<>` で合成**。 `title` も `width` も `VisualSpec` を返すので Monoid で足すだけ。
- 重畳は **`overlay [...]`** で包む (`scatter <> line` を直接足す落とし穴を回避。 理由は
  [api-guide/](./api-guide/README.ja.md) と `design/monoid-semantics.md`)。

## backend の選び方

core (`hgg-core`) は描画先非依存。 出力先で package を選ぶ:

| やりたいこと | package | 関数 / 入口 | 状態 |
|---|---|---|---|
| SVG ファイルに書き出す | `hgg-svg` | `saveSVG` (簡単) / `saveSVGWith` (Resolver) / `saveSVGBound` (df) | ✅ 実用 |
| ブラウザで対話的に描く (Halogen) | `hgg-canvas` (PureScript) | Canvas backend | ✅ 実戦投入中 |
| 3D を CPU 投影 (SVG/PDF/PNG) | `hgg-3d` | scatter3D / surface3D 等 | ✅ |
| 3D をブラウザで orbit/zoom | `hgg-canvas` (WebGL2) | `showBrowser` | ✅ |
| Jupyter (iHaskell) でセルにインライン描画 | `hgg-ihaskell` | `display` (SVG をインライン) | 🧪 Experimental |
| PDF / PNG に書き出す | `hgg-pdf` / `hgg-rasterific` | ─ | 🚧 placeholder |

入口の使い分け (詳細は [api-guide 05 dataframe](./api-guide/06-dataframe.ja.md)):

- `saveSVG :: FilePath -> VisualSpec -> IO ()` ― Resolver 不要 (= `inline` のみの図)。 **通常はこれ**。
- `saveSVGWith :: FilePath -> Resolver -> VisualSpec -> IO ()` ― `ColByName` を含む図に `Resolver` を渡す。
- `saveSVGBound :: FilePath -> BoundPlot -> IO ()` ― DataFrame の `df |>> spec` を保存。
- 文字列でなく SVG テキストが欲しいときは `renderSVG` / `renderSVGWith`。

## Jupyter (iHaskell) で使う

`hgg-ihaskell` を import すると、 セル評価値の図がそのまま**インライン
描画**される (matplotlib inline 相当だが、 現状 **SVG のみ**。 PNG/PDF は未対応)。
描画は SVG backend の `renderSVG` をそのまま使い、 iHaskell の `svg` display
helper に `Text` を渡すだけの薄い配線で、 `ihaskell` 依存は本 package に隔離して
ある (core/svg は無依存)。

```haskell
:set -XOverloadedStrings
import Graphics.Hgg.Easy
import Graphics.Hgg.IHaskell (DisplayPlot(..))   -- インスタンス + DisplayPlot

-- inline 列だけの図はそのままセル末尾に置けば描画される
layer (scatter (inline [0,1,2,3]) (inline [0,1,4,9])) <> title "demo"
```

列名参照 (`ColByName`) を含む図は `Resolver` が要るので `DisplayPlot` で包む:

```haskell
import qualified Data.Vector as V
let spec     = layer (scatter (ColByName "x") (ColByName "y"))
    resolver "x" = Just (NumData (V.fromList [0,1,2,3,4]))
    resolver "y" = Just (NumData (V.fromList [3,1,4,1,5]))
    resolver _   = Nothing
DisplayPlot (resolver, spec)
```

- `DisplayPlot` は df 統合 (spec-2 の `BoundPlot`) が入るまでの**暫定**。 着手時に
  `BoundPlot` へ寄せる。
- 動く notebook = `design/ihaskell/demo.ipynb`。
  各セルが出す図は `cabal run ihaskell-demo-svg` で SVG として書き出せる (同一描画経路)。
- GHC 9.6.7 で `ihaskell-0.13.0.0` の build 実測済。 kernel 環境にこれらの package を
  登録しておくこと。

## 次に読む

- API リファレンス (層・mark・装飾・backend・df・analyze・3D) → **[api-guide/](./api-guide/README.ja.md)**
- 何が描けるか一覧 → **[modules.ja.md](./modules.ja.md)**
- matplotlib / ggplot からの移行 → **[comparison.ja.md](./comparison.ja.md)**
