# 装飾 ─ ラベル / scale / theme / facet / subplot / 座標 / 参照線 / 重畳

> [📚 索引](README.md) ｜ [01 quickstart](01-quickstart.md) ｜ [02 layers](02-layers.md) ｜ **03 decoration** ｜ [04 backends](04-backends.md) ｜ [05 dataframe](05-dataframe.md) ｜ [06 analyze](06-analyze.md) ｜ [07 3d](07-3d.md) ｜ [08 appendix](08-appendix.md)

図全体の設定 (いずれも `VisualSpec`・`purePlot <> … <> これ` の**外**で `<>`) を topic 別に並べる。
mark 自身の見た目・encoding は [02 layers](02-layers.md#encoding) を参照。

## タイトル・ラベル {#labels}

| 設定 | 意味 | 例 |
|---|---|---|
| `title t` | 図タイトル | `<> title "fig"` |
| `subtitle t` / `caption t` / `tag t` | 副題 / 脚注 / タグ | `<> subtitle "補足"` |
| `xLabel t` / `yLabel t` | 軸ラベル | `<> xLabel "x" <> yLabel "y"` |
| `legendTitle t` | 凡例タイトル | `<> legendTitle "群"` |
| `labs (emptyLabs{…})` | タイトル類を一括指定 | (下のデモ参照) |
| `width n` / `height n` | px サイズ (Int) | `<> width 600 <> height 400` |
| `aspectRatio r` | 縦横比 | `<> aspectRatio 1.0` |

デモ (`labs` 一括):

```haskell
purePlot <> layer (scatter xs ys <> size 6)
  <> labs (emptyLabs { labsTitle = Just "title", labsSubtitle = Just "subtitle"
                     , labsCaption = Just "caption", labsX = Just "x 軸", labsY = Just "y 軸" })
```

![3c labs](images/s3c-labs.svg)

## scale / palette {#scale}

| 設定 | 意味 | 例 |
|---|---|---|
| `scaleColorManual [("a","#…"),…]` | カテゴリ → 色の辞書 | `<> scaleColorManual [("A","#1B9E77")]` |
| `scaleColorGradient2 lo mid hi midPt` | 発散 gradient (連続値) | `<> scaleColorGradient2 "#2166AC" "#F7F7F7" "#B2182B" 0` |
| `scaleSize lo hi` | `sizeBy` の半径 range | `<> scaleSize 4 16` |
| `palette ["#…",…]` | 系列色を手動指定 | `<> palette ["#1B9E77","#D95F02"]` |
| `paletteGGplot` | ggplot 既定 hue palette | `<> paletteGGplot` |
| `continuousPalette …` | 連続値 palette | `<> continuousPalette viridis` |
| `scaleXDiscreteLimits ws` | 離散 x 軸のカテゴリ**選択 + 並び順** (= ggplot `scale_x_discrete(limits=)`、 Phase 18) | `<> scaleXDiscreteLimits ["b", "a"]` |
| `scaleYDiscreteLimits ws` | 同 y 軸版 (`forest` の行選択・並べ替えはこちら) | `<> scaleYDiscreteLimits ["b1_0","sigma"]` |

> `scale*DiscreteLimits` は **範囲外カテゴリの行を落とす** (連続軸の `axisRange` の離散版)。
> aes 基準なので `coord_flip` と直交 (flip 後も x/y はデータ軸を指す)。 対象は当該軸の
> encoding が文字列列 (`inlineCat` / TxtData) の layer のみ。

![3d scaleYDiscreteLimits: forest の行選択 + 並べ替え](images/s3d-discrete-limits.svg)

デモ (`colorContinuous` + `scaleColorGradient2`):

```haskell
purePlot <> layer (scatter xs ys <> colorContinuous zs <> size 9)
  <> scaleColorGradient2 "#2166AC" "#F7F7F7" "#B2182B" 3.0   -- lo mid hi midpoint
  <> legend
```

![3d scaleColorGradient2](images/s3d-scale.svg)

## theme ─ `theme :: ThemeName -> VisualSpec` {#theme}

```haskell
<> theme ThemeMinimal
```

選べる `ThemeName`: `ThemeDefault` / `ThemeMinimal` / `ThemeDark` / `ThemeLight` /
`ThemeGrey` / `ThemeBW` / `ThemeClassic` / `ThemeVoid` / `ThemeLinedraw` /
`ThemeNoir` / `ThemeLumen` / `ThemeCanvas` /
`ThemeCanvasDark`。

要素単位の上書きも `VisualSpec` で足せる: `themeGrid False`、 `panelFill "#…"`、
`gridColor "#…"`、 `plotBg "#…"`、 `axisColor "#…"`、 `textColor "#…"`、
`themeAxisTextAngle 45`、 フォントは `titleFont`/`axisLabelFont`/`tickFont`/`legendFont`
(`FontSpec`、 空の `emptyFontSpec` から `fontSize`/`fontFamily`/`fontWeight`/`fontItalic`/`fontColor` で組む)。
→ 動く例: `cabal run tutorial-05-theme`

> 各 font setter には `ThemeOverride` 経由の同名 `theme*Font` 版もある
> (`themeTitleFont`/`themeAxisLabelFont`/`themeTickFont`/`themeLegendFont`)。 描画では
> override (`theme*Font`) が setter (`titleFont` 系) より優先されるが、 **レイアウトの文字高
> 確保は `titleFont` 系のみが効く**ので、 単独で使うなら `titleFont` 系を推奨。

デモ (`theme ThemeDark`):

```haskell
purePlot <> layer (scatter xs ys <> colorStatic "#38bdf8" <> size 6)
  <> theme ThemeDark
```

![3e theme ThemeDark](images/s3e-theme.svg)

## facet (小分け) {#facet}

1 つの列の値でデータを小分けして複数 panel に並べる (ggplot `facet_*` 相当)。
**全オプション**:

```haskell
<> facet "g"                      -- 列 g で単純分割 (1 行 N 列)
<> facetWrap "g" 3                -- 列 g で分割し 3 列で折り返し
<> facetCols 3                    -- 列数だけ指定 (facet と併用)
<> facetGrid "row" "col"          -- row × col の 2 次元 cross 配置
<> facetScales FacetFreeY         -- 軸を panel ごとに自由化 (FacetFixed[既定]/FacetFreeX/FacetFreeY/FacetFree)
<> facetSpace SpaceFree           -- free 軸の panel サイズを data 範囲に比例配分 (facetGrid のみ有効)
```

| 関数 | 役割 | ggplot 対応 |
|---|---|---|
| `facet "g"` | 列 g で単純分割 | `facet_wrap(~g)` (ncol 自動) |
| `facetWrap "g" n` | 列 g で分割・n 列折返し | `facet_wrap(~g, ncol=n)` |
| `facetCols n` | 列数のみ (`facet` と併用) | `facet_wrap(ncol=n)` |
| `facetGrid "r" "c"` | r × c の 2 次元 | `facet_grid(r ~ c)` |
| `facetScales FacetFreeY` | 軸の共有方式 | `facet_*(scales="free_y")` |
| `facetSpace SpaceFree` | panel サイズ配分 (grid 限定) | `facet_grid(space="free")` |

> `FacetScales` = `FacetFixed` / `FacetFreeX` / `FacetFreeY` / `FacetFree` (`Spec.hs:664`)。
> `FacetSpace` = `SpaceFixed` / `SpaceFreeX` / `SpaceFreeY` / `SpaceFree` (`Spec.hs:694`)。
> 完全に別の spec を panel に並べたい場合 (facet でなく独立図の並置) は [subplot](#subplots) を使う。

デモ (`facetWrap "g" 2`)。 facet 列は名前参照なので `Resolver` (または DataFrame) で
`"g"` を供給する:

```haskell
-- r は "x"/"y"/"g" を返す Resolver
saveSVGWith "out.svg" r $
  purePlot <> layer (scatter "x" "y" <> color "g" <> size 6) <> facetWrap "g" 2
```

![3f facetWrap](images/s3f-facet.svg)

## subplot (独立図の並置) {#subplots}

`facet` が **1 つの列でデータを小分け**するのに対し、 `subplots` は **完全に別の `VisualSpec` を
並べる** (ggplot にはない・matplotlib `subplots` / patchwork 相当)。 図ごとに mark も軸も別でよい。

```haskell
<> subplots [ spec1, spec2, spec3 ]   -- 独立図のリストを並置
<> subplotCols 2                       -- 2 列で折り返し (既定は 1 行 N 列)
```

| 関数 | 役割 |
|---|---|
| `subplots [VisualSpec]` | 各 `VisualSpec` を独立 panel として並べる (`Spec.hs:2133`) |
| `subplotCols n` | 並置の折り返し列数 (`Spec.hs:2137`) |
| `repeatFields fields mk` | フィールド名を反復し各フィールドから 1 view を生成して `subplots` に展開 (Vega-Lite `repeat` 相当) |

**フィールド反復 (`repeatFields`)**: 同じ作図テンプレートを複数フィールドに適用したいときは、
`subplots` に手で並べる代わりに `repeatFields` を使う (Vega-Lite の `repeat` 相当・明示形)。
生成関数にフィールド名が渡るので、 各 view で別の列を使える:

```haskell
<> repeatFields ["height", "weight", "age"] (\f -> layer (hist f) <> title f)
<> subplotCols 3                                  -- 3 列に並べる
```

**panel の名前選択 (`selectPanels`、 Phase 18)**: `repeatFields` が「名前リスト → panel 群」
なのに対し、 その**逆方向** = でき上がった panel 群から **名前 (= 各 panel の `title`) で
一部だけ選ぶ**。 列挙順がそのまま表示順になる (選択 + 並べ替えを兼ねる)。 多パラメータの
診断 grid (例: analyze 連携の HBM trace) から注目パラメータだけ見るときに使う:

```haskell
<> subplots panels <> selectPanels ["b1_0", "b1_1", "sigma"] <> subplotCols 1
-- title が一致しない名前は無視。 未指定なら従来通り全 panel。
```

![3f-2 selectPanels: 4 panel から c, a を選択](images/s3f2-select-panels.svg)

各 panel はそれ自身が完全な図なので、 `title` / `theme` / mark を panel 単位で `<>` できる:

```haskell
saveSVG "dash.svg" $
  subplots [ layer (scatter "x" "y") <> title "散布"
           , layer (line    "x" "y") <> title "折れ線"
           , layer (bar     "g" "y") <> title "棒" ]
  <> subplotCols 3 <> title "ダッシュボード"
```

![3f-2 subplots: 独立図の並置](images/s3f2-subplot.svg)

**入れ子 (nested subplots)**: panel の中身自体に `subplots` を持たせると、 入れ子グリッドになる
(Phase 52.B1 で対応)。 左に主図 1 つ・右に小図 2 段、 のような非対称レイアウトが組める:

```haskell
subplots [ layer (scatter "x" "y") <> title "主図"
         , subplots [ layer (histogram "x") <> title "x 分布"
                    , layer (histogram "y") <> title "y 分布" ] <> subplotCols 1 ]
<> subplotCols 2
```

![3f-2 nested subplots: 主図 + 入れ子の周辺分布](images/s3f2-nested.svg)

> HBM 診断を 1 枚に並べた入れ子ダッシュボードの実例は
> [analyze 連携の HBM ダッシュボード](06-analyze.md#hbm-plotting) を参照。

**concat 合成 (`hconcat` / `vconcat` + 演算子)**: `subplots` + `subplotCols` の薄ラッパとして、
Vega-Lite の `hconcat`/`vconcat` 相当と patchwork 風の中置演算子を用意している。

| 関数 / 演算子 | 役割 |
|---|---|
| `hconcat [a, b, c]` | 横並び (1 行 n 列・`subplots ss <> subplotCols (length ss)`) |
| `vconcat [a, b]` | 縦並び (n 行 1 列・`subplots ss <> subplotCols 1`) |
| `a <-> b` | 横結合演算子 (`infixl 6`) |
| `a <:> b` | 縦結合演算子 (`infixl 5`) |

演算子は **同方向チェーンを平坦化**する。 `a <-> b <-> c` は 3 等分列 (二項ネストで左セルが
`a,b` に割れたりしない) になり、 異なる方向を混ぜると入れ子になる。 たとえば
**1 行目を 3 列・2 行目を全幅 (1 行目セルの 3 倍幅)** は次の 1 行で書ける:

```haskell
saveSVG "concat.svg" $
  (a <-> b <-> c) <:> d          -- = vconcat [hconcat [a, b, c], d] と同値
```

![3f-2 concat: (a <-> b <-> c) <:> d](images/concat.svg)

> ★演算子の選定: `<->`(横)・`<:>`(縦) は Prelude / 標準ライブラリと衝突しない。 縦に直感的な
> `<|>` は `Control.Applicative` (HS) / `Control.Alt` (PS) の Alternative と名前衝突するため採らなかった。

## 座標系 {#coord}

```haskell
<> coordFlip          -- x↔y 反転 (横棒グラフ等)
<> coordPolar         -- 極座標 (x 角度)
<> coordPolarY        -- 極座標 (y 角度)
<> reverseX           -- 軸反転
<> coordCartesian     -- 既定
```

デモ (`coordFlip` で横棒):

```haskell
purePlot <> layer (bar (inlineCat ["A","B","C"]) (inline [3,7,5])) <> coordFlip
```

![3g coordFlip](images/s3g-coord.svg)

## 凡例・参照線・補助 {#guides}

```haskell
<> legend                         -- 凡例 ON
<> legendOff                      -- OFF
<> legendPos LegendBottom         -- 位置 (Right/Bottom/None/Inside*)
<> refIdentity                    -- y=x 線
<> refHorizontal 0                -- 水平線 y=0
<> refVertical 1.0                -- 垂直線 x=1
<> refLine (RefLinear 2 1)        -- 任意 y = 2x + 1
<> marginalX                      -- x 周辺ヒストグラム
<> marginalY
<> annotate (…)                   -- 注釈 (text/arrow/rect/line)
<> inset (…)                      -- 図中図
<> xAxis (…) / yAxis (…)          -- 軸の細かい制御 (log/範囲/tick)
```

軸の調整は `logAxis` / `sqrtAxis` / `timeAxis` / `axisRange lo hi` / `axisMin` /
`axisMax` / `axisBreaksAt […]` / `hideTicks` 等。

デモ (`refHorizontal` + `refVertical` + `legend`):

```haskell
purePlot <> layer (scatter xs ys <> color gs <> size 6)
  <> refHorizontal 2.5 <> refVertical 2.5 <> legend
```

![3h refHorizontal / refVertical + legend](images/s3h-guides.svg)

## 値で選ぶ設定 (列挙型) の早見表 {#enum-tables}

取りうる値が決まっている設定 (`position` など) はここに **全部** 挙げる。 定義 = 最終的な真実は
`hgg-core` の **`Hgg/Plot/Spec.hs`** (行番号付き)。 値が増えたらソースが正。

| 設定関数 | 型 | 取りうる値 (すべて) | 定義 |
|---|---|---|---|
| `position` | `Position` | `PosIdentity` / `PosDodge` / `PosStack` / `PosFill` | `Spec.hs:611` |
| `linetype` / `linetypeBy` | `LineType` | `LtSolid` / `LtDashed` / `LtDotted` / `LtDotDash` / `LtLongDash` / `LtTwoDash` | `Spec.hs:779` |
| `theme` | `ThemeName` | `ThemeDefault` / `ThemeMinimal` / `ThemeDark` / `ThemeLight` / `ThemeGrey` / `ThemeBW` / `ThemeClassic` / `ThemeVoid` / `ThemeLinedraw` / `ThemeNoir` / `ThemeLumen` / `ThemeCanvas` / `ThemeCanvasDark` (13 種) | `Spec.hs:1704` |
| `facetScales` | `FacetScales` | `FacetFixed` / `FacetFreeX` / `FacetFreeY` / `FacetFree` | `Spec.hs:655` |
| `legendPos` | `LegendPosition` | `LegendRight` / `LegendBottom` / `LegendNone` / `LegendInsideTopRight` / `LegendInsideTopLeft` / `LegendInsideBottomRight` / `LegendInsideBottomLeft` | `Spec.hs:1626` |
| 座標系 (`coordFlip` / `coordPolar` …) | `Coord` | `CoordCartesian` / `CoordFlip` / `CoordPolarX` / `CoordPolarY` | `Spec.hs:633` |
| `refLine` | `ReferenceLine` | `RefIdentity` / `RefHorizontalAt c` / `RefVerticalAt c` / `RefLinear slope intercept` | `Spec.hs:1531` |

> 例: 積み上げ棒 `<> position PosStack`、 横並び `<> position PosDodge`、 100% 積み上げ
> `<> position PosFill`。 破線 `<> linetype LtDashed`。 凡例を内側右上 `<> legendPos LegendInsideTopRight`。

## 重畳の正しい書き方 (`<>` の仕組み) {#overlay}

`<>` が 2 階層あることの実害は **重畳**で出る。

```haskell
-- ✅ 2 つの mark を重ねる: 各々を layer で包んで <>
purePlot
  <> layer (scatter xs ys <> alpha 0.85 <> size 5)
  <> layer (line    xs fit <> colorStatic "#dc2626" <> stroke 2)

-- ❌ これは重ならない (= scatter と line のプロパティが合成され 1 つの mark になる)
purePlot
  <> layer (scatter xs ys <> line xs fit)
```

理由: `scatter`・`line` は `Layer` を返し、 `Layer` の `<>` は **同一 layer への
プロパティ合成** (色や太さの上書き) であって「2 図を重ねる」 意味ではない。
重畳は各々を `layer` で `VisualSpec` 化してから足す。 後に書いた layer が上に乗る。

![3j 重畳の出力 (散布 + 回帰直線)](images/lesson4-overlay.svg)

Easy 層の `overlay [a, b]` (= `foldMap layer`) はこの定型を 1 語にしたもの。

> **色と凡例の整合**: `color (ColByName …)` を持つ layer を重畳した場合、
> glyph の色と凡例 swatch は**全 layer のカテゴリ union** という同じ正本から
> 引かれる (カテゴリの初出順に palette を割当)。 layer ごとに色が振り直されて
> 凡例とズレることはない。 順序を明示したいときは `colorCats [..]` が優先される。
(詳細: `design/monoid-semantics.md`) → `cabal run tutorial-03-overlay`

## 高度な図 (設定の積層) {#advanced-layering}

`<>` で設定を積み重ねると、 1 枚に多くの encoding / 装飾を載せられる。 下は
**連続色 gradient + 点サイズ encoding + 回帰直線 overlay + 参照線 + theme + labs +
凡例**を 1 図に重ねた例 (本ページ各設定の組合せ)。 df 連携 ([05 dataframe](05-dataframe.md) で詳説) で書くと、
encoding はすべて列名で済み、 同じ列 (`"y"`) を色にも使い回せる:

```haskell
import           Hgg.Plot.Easy             -- Spec を re-export (scatter/layer/ColData…)
import           Hgg.Plot.Frame            ((|>>))
import           Hgg.Plot.Backend.SVG      (saveSVGBound)
import qualified Data.Map.Strict as M
import qualified Data.Vector     as V

num :: [Double] -> ColData ; num = NumData . V.fromList

-- x / y / sz (点サイズ用) / fit (回帰直線の予測値) を 1 つの df に
df :: M.Map Text ColData
df = M.fromList [ ("x", num xs), ("y", num ys), ("sz", num sz), ("fit", num fit) ]

main :: IO ()
main = saveSVGBound "advanced.svg" $
  df |>>
     ( layer ( scatter "x" "y"            -- 散布
               <> colorContinuous "y"     -- 連続色 (Viridis gradient・y 列を流用)
               <> sizeBy "sz"             -- 点サイズを列の値で
               <> alpha 0.85 )
     <> layer ( line "x" "fit"            -- 回帰直線 overlay
                <> colorStatic "#ef4444" <> stroke 2 )
     <> scaleSize 4 16                     -- サイズ range
     <> refHorizontal 1.0                  -- 水平参照線
     <> theme ThemeMinimal
     <> legend
     <> labs (emptyLabs
          { labsTitle    = Just "連続色 + サイズ + 回帰 + 参照線"
          , labsSubtitle = Just "colorContinuous / sizeBy / line overlay / refHorizontal"
          , labsCaption  = Just "<> で設定を積層"
          , labsX = Just "x", labsY = Just "y" }) )
```

![高度な図 (設定の積層)](images/advanced.svg)

ポイント: **encoding (色・サイズ) は mark の中で `<>`**、 **scale・theme・参照線・labs は
図の外で `<>`** ([02 layers](02-layers.md) の戻り型ルール)。 重畳は mark ごとに `layer`
([重畳](#overlay))。 → 図の生成コードは `hgg-svg/examples/DocFigures.hs`
(`cabal run doc-figures` で本ガイドの全図を再生成)。
