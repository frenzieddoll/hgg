# 10 custom marks — 自作の mark を足す

組み込み mark (`scatter` / `line` / `bar` / `box` / …) に無いプロット型を、**ライブラリ本体
(`MarkKind` の列挙) を一切編集せず**自分で定義するための拡張点が **custom mark** (`customMark`)。
このページだけ読めば、 新しい mark を最後まで組めるようにする。

> **いつ使うか**: 既存 mark で描けない図 (dendrogram・独自の annotation・専用ダイアグラム等) を
> 描きたいとき。 scale / legend / color と深く統合したい本格 mark は core への追加が要る
> ([09 appendix](09-appendix.md) のライブラリ拡張) が、 「線と文字を自分で置くだけ」の型は
> custom mark が最短。 ggplot の "Extending ggplot2"・matplotlib の Artist helper
> (`scipy…dendrogram` 等) と同じ発想。

## 1. 30 秒版 — 最小の custom mark

`customMark id drawFn` が `Layer` を返す。 `drawFn :: RenderCtx -> [Primitive]` が本体で、
「文脈 (`RenderCtx`) を受け取り、 図形 (`Primitive`) の列を返す」だけ。

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Hgg.Plot.Easy
import Hgg.Plot.Primitive (Primitive(..), Point(..), solid)
import Hgg.Plot.Spec      (RenderCtx(..), customMark)

-- 左下 (0,0) から右上 (1,1) へ対角線を 1 本引くだけの mark
diagonal :: RenderCtx -> [Primitive]
diagonal ctx =
  let p x y = uncurry Point (rcProjectXY ctx x y)   -- data 座標 → 画面 px
  in [ PLine (p 0 0) (p 1 1) (solid (rcColor ctx) 1.5) ]

main :: IO ()
main = saveSVG "diag.svg" $
     purePlot
  <> layer (scatter (inline [0,1]) (inline [0,1]) <> alpha 0.0)  -- 軸 range を確保 (後述)
  <> layer (customMark "diagonal" diagonal)                       -- ★ core 無改造
  <> title "my first custom mark"
```

要点は 3 つ:

1. **`rcProjectXY ctx x y`** で data 座標を画面 px に変換する (scale・log・polar・flip に自動追従)。
2. `Primitive` 構築子 (`PLine` 等) で図形を作る。
3. `customMark "id" drawFn` を `layer (…)` に入れて他の layer と `<>` で合成する。

## 2. RenderCtx — draw 関数に渡る文脈

`drawFn` が受け取る唯一の引数。 描画に必要なものはすべてここから取る。

| フィールド | 型 | 用途 |
|---|---|---|
| `rcProjectXY` | `Double -> Double -> (Double, Double)` | **data 座標 (x,y) → device px**。 軸 scale / 座標系に追従。 ほぼ必ず使う |
| `rcPlotArea` | `Rect` | plot 描画領域 (px)。 `Rect {rX,rY,rW,rH}`。 clip や領域基準の配置に |
| `rcResolver` | `Resolver` | 列名 → データ。 layer に束縛した列を引く (§5) |
| `rcColor` | `Text` | theme 既定の線 / 点色 |
| `rcFill` | `Text` | theme 既定の塗り色 |
| `rcTextColor` | `Text` | theme 既定の文字色 |
| `rcAxisColor` | `Text` | theme 既定の軸色 |

`RenderCtx(..)` は `Hgg.Plot.Spec` から import する。

## 3. Primitive — 使える図形の語彙

`drawFn` が返す backend 非依存の描画命令。 `Hgg.Plot.Primitive` (または再 export する
`Hgg.Plot.Render`) から import する。 座標はすべて **device px** (= `rcProjectXY` の出力)。

| 構築子 | 引数 | 意味 |
|---|---|---|
| `PLine` | `Point Point LineStyle` | 線分 |
| `PPath` | `[PathSegment] FillStyle (Maybe StrokeStyle)` | 折れ線 / 曲線 / 多角形 (`MoveTo`/`LineTo`/`CurveTo`/`ClosePath`) |
| `PRect` | `Rect FillStyle (Maybe StrokeStyle)` | 矩形 |
| `PCircle` | `Point Double FillStyle (Maybe StrokeStyle) (Maybe Text)` | 円 (最後は hover ラベル・不要なら `Nothing`) |
| `PText` | `Point Text TextStyle` | 文字 |

スタイル構築子:

```haskell
solid :: Text -> Double -> LineStyle          -- 実線: solid "#333" 1.5
FillStyle   { fsColor :: Text, fsOpacity :: Double }
StrokeStyle { ssColor :: Text, ssWidth   :: Double }
TextStyle   { tsColor, tsSize, tsFamily, tsAnchor, tsRotate, tsWeight, tsItalic }
Point x y                                     -- device px の点
```

`TextStyle` の例: `TextStyle (rcTextColor ctx) 10 "sans-serif" AnchorMiddle 0 "normal" False`
(`AnchorMiddle`/`AnchorStart`/`AnchorEnd`・`tsRotate` は degrees CCW)。

## 4. 手順で作る — dendrogram を「普通の mark」として作る

`"leaf"` (葉の x 位置) と `"height"` (節の高さ) の 2 列から、 併合を「Π 字」の elbow で描く簡易
dendrogram を作る。 最終形は **`dendrogram "leaf" "height"`** と `scatter x y` と同じ書き味で使える。
完成例は `hgg-svg/examples/CustomMarkDemo.hs` (`cabal run custom-mark-demo`)。

```haskell
import Hgg.Plot.Easy
import Hgg.Plot.Primitive (Primitive(..), Point(..), TextStyle(..), TextAnchor(..), solid)
import Hgg.Plot.Spec (RenderCtx(..), ColRef, ColData(..), customMark, encX, encY, resolveNum)
import qualified Data.Vector as V

-- ① draw: "leaf"/"height" 列を rcResolver で読み、 elbow を組む (簡単のため 4 葉固定)
dendroDraw :: RenderCtx -> [Primitive]
dendroDraw ctx =
  let num nm = maybe [] V.toList (resolveNum (rcResolver ctx) nm)  -- 列を読む
      p x y  = uncurry Point (rcProjectXY ctx x y)                 -- data→px
      ls     = solid (rcColor ctx) 1.5
      -- 子を高さ hy で併合する Π 字 (子の基準高さ by1/by2 から立てる)
      elbow cx1 by1 cx2 by2 hy =
        [ PLine (p cx1 by1) (p cx1 hy) ls
        , PLine (p cx2 by2) (p cx2 hy) ls
        , PLine (p cx1 hy)  (p cx2 hy) ls ]
      ts = TextStyle (rcTextColor ctx) 10 "sans-serif" AnchorMiddle 0 "normal" False
  in case (num "leaf", num "height") of
       ([x0,x1,x2,x3], [_,h1,h2]) ->               -- leaf=[0,1,2,3], height=[0,1,2]
         let m01 = (x0+x1)/2; m23 = (x2+x3)/2
         in concat
              [ elbow x0 0 x1 0 h1                  -- 葉0,1 → 高さ h1
              , elbow x2 0 x3 0 h1                  -- 葉2,3 → 高さ h1
              , elbow m01 h1 m23 h1 h2              -- (0,1) と (2,3) → 高さ h2 = root
              , [ PText (p x (-0.12)) nm ts
                | (x, nm) <- zip [x0,x1,x2,x3] ["A","B","C","D"] ] ]
       _ -> []

-- ② 名前付き combinator = 普通の mark 化。 x/y 列を束ねて軸 range を自動化する。
dendrogram :: ColRef -> ColRef -> Layer
dendrogram x y = customMark "dendrogram" dendroDraw <> encX x <> encY y

-- ③ 使う側 = scatter x y と同じ。 列は resolver (ここでは dat) / df |>> で供給。
main :: IO ()
main = saveSVGWith "dendro.svg" dat $
     purePlot
  <> layer (dendrogram "leaf" "height")
  <> xLabel "leaf" <> yLabel "merge height"
  where dat "leaf"   = Just (NumData (V.fromList [0,1,2,3]))  -- 葉の x 位置
        dat "height" = Just (NumData (V.fromList [0,1,2]))    -- 節の高さ (葉基準0 / 1段 / root)
        dat _        = Nothing
```

要点:

- **`dendrogram "leaf" "height"` は `scatter x y` と同じ形**。 中身は `customMark … <> encX <> encY`。
- draw は列を **`rcResolver`** で読む (§5)。 データは `saveSVGWith` の resolver や `df |>>` から流れる。
- **`encX`/`encY` が軸 range を自動化**する (下記 §4.1)。 無いと既定 `[0,1]` で図形が枠外に出る。

### 4.1 一級 mark 化の仕組み — `encX` / `encY`

`encX` / `encY` は **mark 種別に依らず x/y 列を束ねる単独 setter** (`Hgg.Plot.Spec`):

```haskell
encX :: ColRef -> Layer          -- x encoding 列だけを束ねる
encY :: ColRef -> Layer          -- y encoding 列だけを束ねる
```

`customMark id draw <> encX x <> encY y` と合成すると、 軸 range が `lyEncX`/`lyEncY` から自動計算
され (`RangeOf` が走査)、 `df |>>` (§5) でデータが流れる。 各 custom mark は固有の `draw` を持つので、
上の `dendrogram` のように **mark ごとに専用 combinator を書く**のが自然 (汎用の束ねヘルパーは不要)。

`encX`/`encY` を束ねると **軸 range が自動計算**され (`RangeOf` が `lyEncX`/`lyEncY` を走査)、
`df |>>` (§5) でデータが流れる。 各 custom mark は固有の `draw` を持つので、 このように
**mark ごとに専用 wrapper を書く**のが自然 (汎用の束ねヘルパーは不要)。

## 5. データを流し込む 2 通り

- **closure に閉じ込める** (上の例): 葉座標を draw 関数に直接書く。 HS の強み。 最短。
- **列から引く**: layer に束縛した列を `rcResolver` で読む。 スクリーン上のデータ駆動に。

```haskell
import Hgg.Plot.Spec (resolveNum)

fromCols :: RenderCtx -> [Primitive]
fromCols ctx =
  case (resolveNum (rcResolver ctx) "x", resolveNum (rcResolver ctx) "y") of
    (Just xs, Just ys) ->
      [ PCircle (uncurry Point (rcProjectXY ctx x y)) 3
                (FillStyle (rcColor ctx) 1) Nothing Nothing
      | (x, y) <- zip (V.toList xs) (V.toList ys) ]
    _ -> []
-- 列は resolver 経由で渡す:
--   saveSVGWith "out.svg" myResolver (… customMark "c" fromCols …)
-- DataFrame なら df |>> がそのまま resolver を供給する (§4.1 の一級 mark 化と併用):
--   saveSVGBound "out.svg" (df |>> (purePlot <> layer (dendrogram "leaf" "height")))
```

`df |>>` は df の列を読む resolver (`dfResolver df`) を図に束ねる。 custom mark の `draw` が
`rcResolver` で同じ列名を読めば、 追加配線なしで DataFrame のデータが流れる。

## 6. canvas (PureScript) でも描く — parity

custom mark の描画 closure は **HS 専用** (関数は JSON 化できない)。 図を JSON にして canvas
(PureScript) に渡すと `id` と option だけが送られ、 closure は落ちる。 canvas でも同じ mark を
描きたいときは、 **同じ `id` で PureScript 側の registry に draw 関数を手登録**する
(HS↔PS を手でミラーする現行の作法どおり)。 登録しなければ canvas では **skip** される
(= HS 専用 = SVG / PDF / Rasterific のみ)。

```purescript
import Hgg.Plot.Custom (registerMark)          -- RenderCtx もここから
import Hgg.Plot.Render.Common (Primitive(..), Point(..))

-- HS の drawFn と同じ図形を返すよう手で書く
dendroDrawPS :: Json -> RenderCtx -> Array Primitive
dendroDrawPS _opts ctx =
  let p x y = case ctx.projectXY x y of { r -> Point r.x r.y }  -- projection は {x,y}
  in [ PLine (p 0.0 0.0) (p 0.0 1.0) (solid ctx.color 1.5), … ]

main = do
  registerMark "dendrogram" dendroDrawPS   -- アプリ起動時に一度
  …
```

PS の `RenderCtx` は HS と同じ項目 (`projectXY` / `plotArea` / `resolver` / `color` / `fill` /
`textColor` / `axisColor`)。 違いは **projection が `{ x, y }` レコードを返す**点だけ (PS 慣例)。
option (`customMarkWith` で渡した JSON) は draw 関数の第 1 引数 `Json` に来る。

## 7. core への昇格 (promotion)

出来が良く「組み込みにしたい」ときは、 **登録をライブラリ内に移すだけ**で済むよう設計されている:

- HS: draw 関数をライブラリの module に置き、 `customMark "id"` 呼び出しをそのライブラリ関数にする
  (公開 API・`id` 不変ゆえ利用側コード非破壊)。
- PS: `registerMark "id" drawPS` をライブラリ初期化で呼ぶ。

scale / legend / color と本格統合したい場合のみ core の `MarkKind` に constructor + renderer を足す
(従来経路)。 custom mark はそこまで要らない型のための軽量チャネル。

## 8. API 早見

```haskell
-- Hgg.Plot.Spec (Easy でも可)
customMark     :: Text -> (RenderCtx -> [Primitive]) -> Layer
customMarkWith :: Text -> Value -> (RenderCtx -> [Primitive]) -> Layer   -- PS へ渡す option 付き
encX           :: ColRef -> Layer   -- x 列を束ねる (mark 非依存・一級 mark 化 / 軸 range 自動)
encY           :: ColRef -> Layer   -- y 列を束ねる

data RenderCtx = RenderCtx
  { rcProjectXY :: Double -> Double -> (Double, Double)
  , rcPlotArea  :: Rect
  , rcResolver  :: Resolver
  , rcColor, rcFill, rcTextColor, rcAxisColor :: Text }
```

```purescript
-- Hgg.Plot.Custom (PureScript)
registerMark :: String -> (Json -> RenderCtx -> Array Primitive) -> Effect Unit
type RenderCtx = { projectXY :: Number -> Number -> { x :: Number, y :: Number }
                 , plotArea :: Rect, resolver :: Resolver
                 , color, fill, textColor, axisColor :: String }
-- PureScript でも同型の customMark / customMarkWith が Hgg.Plot.Spec にある
```

## 関連

- 最小作例: `hgg-svg/examples/CustomMarkDemo.hs` (`cabal run custom-mark-demo`)。
- 設計背景 (なぜ closure・なぜ Primitive を leaf に): `specification/phases/phase-51-custom-mark-extension.md`。
- 参考: ggplot "Extending ggplot2" / matplotlib Artist / scipy dendrogram (helper 関数方式)。
- 本格 mark を core に足す場合: [09 appendix](09-appendix.md) のライブラリ拡張。
