# encoding & scale ─ チャネルとスケール

> [📚 索引](README.ja.md) ｜ [01 quickstart](01-quickstart.ja.md) ｜ [02 layers](02-layers.ja.md) ｜ **03 encoding & scale** ｜ [04 decoration](04-decoration.ja.md) ｜ [05 backends](05-backends.ja.md) ｜ [06 dataframe](06-dataframe.ja.md) ｜ [07 analyze](07-analyze.ja.md) ｜ [08 3d](08-3d.ja.md) ｜ [09 appendix](09-appendix.ja.md)

データ列を視覚属性へ写す **encoding (channel)** と、その写像の見た目を制御する **scale** を 1 ページにまとめる。
「どの列を色・サイズ・形に割り当てるか」(channel) と「その色やサイズをどう見せるか」(palette / gradient /
limits / 軸 breaks) は連続した話題なので、まとめて辞書化する。

- **channel** は **mark の中で `<>`** で足す (`layer (scatter x y <> colorBy "g")`)。各 mark の
  「encoding」欄 ([02 layers](02-layers.ja.md)) はここの修飾子を参照する。
- **scale / 軸** は図全体の設定なので **`layer (…)` の外**で `<>` する (`VisualSpec` を返す)。
  純 cosmetic なフォント・グリッド・背景色は [04 decoration](04-decoration.ja.md#theme) の theme 側にある。

このページの構成:
**[1. encoding channel](#channel)** ｜ **[2. scale / palette](#scale)** ｜ **[3. position scale (軸)](#axis)**

---

<a id="encoding"></a>
<a id="channel"></a>

## 1. encoding channel

全 mark 共通の見た目・channel 修飾子。**mark の中で `<>`** で足す (`layer (scatter x y <> colorBy "g")`)。
各定型エントリ ([02 layers](02-layers.ja.md#entries)) の「encoding」欄はここの修飾子を参照する。

同じ散布を 4 つの channel で写し分けた例 (列の値を色・サイズ・形・線種へ):

```haskell
let xs = inline [1,2,3,4, 1,2,3,4]; ys = inline [2,3,1,4, 3,1,4,2]
    g  = inlineCat (replicate 4 "a" ++ replicate 4 "b"); sz = inline [1,2,3,4, 4,3,2,1]
    lx = inline [1,2,3,4,5, 1,2,3,4,5]; ly = inline [1,2,3,4,5, 2,3,3,4,5]
    lg = inlineCat (replicate 5 "p" ++ replicate 5 "q")
in subplots [ layer (scatter xs ys <> colorBy g  <> size 6)    -- カテゴリ→色
            , layer (scatter xs ys <> sizeBy sz)               -- 数値→点サイズ
            , layer (scatter xs ys <> shapeBy g <> size 6)     -- カテゴリ→形
            , layer (line lx ly <> linetypeBy lg <> stroke 2) ] -- カテゴリ→線種
   <> subplotCols 2 <> legend
```

![channel カタログ (colorBy / sizeBy / shapeBy / linetypeBy)](images/encoding-channels.svg)

### 色

| 修飾子 | 型 | 意味 |
|---|---|---|
| `colorBy` | `ColRef -> Layer` | カテゴリ列で色分け (列を渡す) |
| `color` | `Color -> Layer` | 単色固定 (`color (fromHex "#dc2626")`・[Color 型](#color-type)) |
| `colorContinuousBy` | `ColRef -> Layer` | 連続値 gradient (数値列) |
| `colorRGBA` | `Text -> Layer` | 8 桁 RGBA hex を `color <> alpha` に展開 ([詳細](#color-type)) |
| `colorCats` | `[Text] -> Layer` | 群の色割当順を明示 (カテゴリ→palette index) |

### サイズ・形・線

| 修飾子 | 型 | 意味 |
|---|---|---|
| `alpha` / `size` / `stroke` | `Double -> Layer` | 不透明度 (0–1) / 点サイズ / 線幅 |
| `alphaBy` / `sizeBy` | `ColRef -> Layer` | 数値列で連続不透明度 / 点サイズ |
| `shape` / `shapeBy` | `MarkShape -> Layer` / `ColRef -> Layer` | 形固定 / カテゴリで形 |
| `linetype` / `linetypeBy` | `LineType -> Layer` / `ColRef -> Layer` | 線種固定 / カテゴリで線種 |
| `shapeMapEntry` | `Text -> MarkShape -> Layer` | 特定カテゴリ→形の対応 1 件 |
| `edgeOn` / `edge` / `edgeWidth` | `Layer` / `Text -> Layer` / `Double -> Layer` | 散布点に**縁**を付ける (既定は縁なし = ggplot 塗り点 shape 19)。`edgeOn` は点と同色の 1px 縁、`edge "#333"` は縁色指定、`edgeWidth 1.5` は縁幅。透過は縁色に alpha 付き hex (`edge "#00000044"`)。`stroke` (線幅) とは別物 |
| `hollow` | `Layer` | マーカーを中抜き (ggplot `shape="circle open"`)。塗りを透明にし点色で輪郭のみ。`size` で輪郭径・`stroke` で線幅。点を輪で囲む強調に重畳 |

### 位置・群・接続

| 修飾子 | 型 | 意味 |
|---|---|---|
| `position` | `Position -> Layer` | 積み方 (`PosIdentity` / `PosDodge` / `PosStack` / `PosFill`) |
| `groupBy` | `ColRef -> Layer` | 色なしの群分け (分布 mark のスロット分割等) |
| `nudge` | `Double -> Layer` | 分布 mark の同一スロット内ずらし (raincloud 等) |
| `markWidth` | `Double -> Layer` | 分布 mark の横幅 (箱・violin 幅) |
| `side` | `Side -> Layer` | 片側描画 (half-violin 等) |
| `jitterX` / `jitterY` | `Double -> Layer` | 点を軸方向にジッタ (重なり回避) |
| `errorX` / `errorY` | `ColRef -> Layer` | エラーバー列 |
| `connect` / `connectOrder` / `connectGroup` | `Layer` / `ColRef -> Layer` | 点の接続線 (有効化 / 順序列 / 群列) |
| `connectColor` / `connectWidth` | `Text -> Layer` / `Double -> Layer` | 接続線の色 / 幅 |
| `hoverCols` | `[ColRef] -> Layer` | hover tooltip 列 |

> 関連型: `ConnectSpec` (接続線)・`MarkShape` / `ShapeMapEntry` (形状)・`Categorical` (カテゴリ encoding)。
> 列挙型 (`Position` / `LineType` / `MarkShape` / `Side`) の一覧は [04 decoration](04-decoration.ja.md#enum-tables)。

<a id="color-type"></a>

### 固定色の指定 ─ `Color` 型

固定色 (`color` / `colorRGBA`) は型安全な `Color` (`Graphics.Hgg.Color`) で渡す。構築子は
`fromHex :: Text -> Color` (`fromHex "#dc2626"`・3 桁可・不正は `error`)、`fromHexMaybe` (total)、
`rgb :: Word8 -> Word8 -> Word8 -> Color`。R の 657 色名は `Graphics.Hgg.Color.Named` に定数で
入っている (`import qualified … as N`・`color N.steelblue`)。8 桁 RGBA hex を直接貼るときは
`colorRGBA "#88888855"` (= `color (fromHex "#888888") <> alpha (85/255)`・total 版 `colorRGBAMaybe`)。
内部 helper は `fromHexA` / `fromHexAMaybe :: Text -> (Maybe) (Color, Double)`。3D 版は [08 3d](08-3d.ja.md)。

---

## 2. scale / palette {#scale}

channel で割り当てた色・サイズの**見た目**を制御する。色辞書・発散 gradient・サイズ range・離散軸の
カテゴリ選択など。いずれも `VisualSpec` を返し、図の外で `<>` する。

| 設定 | 型 (何を渡すか) | 意味 |
|---|---|---|
| `scaleColorManual` | `[(Text, Text)] -> VisualSpec` | カテゴリ → 色の辞書 (`[("A","#1B9E77")]`) |
| `scaleColorGradient2` | `Text -> Text -> Text -> Double -> VisualSpec` | 発散 gradient (lo, mid, hi 色, midpoint 値) |
| `scaleSize` | `Double -> Double -> VisualSpec` | `sizeBy` の半径 range (lo, hi) |
| `palette` / `continuousPalette` | `[Text] -> VisualSpec` | 系列色 / 連続色を色 hex 列で指定 |
| `paletteGGplot` | `VisualSpec` | ggplot 既定 hue palette (引数なし) |
| `scaleXDiscreteLimits` / `scaleYDiscreteLimits` | `[Text] -> VisualSpec` | 離散軸のカテゴリ選択 + 並び順 |
| 既製パレット `okabeIto` / `tolBright` / `brewerDark2` / `brewerSet2` | `[Text]` | `palette` に渡す色列 (Okabe-Ito は色覚多様性対応) |

既製パレットは単なる `[Text]` (色 hex 列) なので `palette` に渡すだけ。 例: 色覚多様性に配慮した
Okabe-Ito で系列色を割り当てる:

```haskell
purePlot <> layer (scatter xs ys <> colorBy gs) <> palette okabeIto <> legend
```

![palette okabeIto](images/s3d-palette-okabe.svg)

> 補助: `orderedCats :: [Text] -> [Text]` (カテゴリ順の正規化)・`themeSeriesPalette :: ThemeName -> [Text]`
> (テーマ既定の系列色を取り出す)・`viridisStops3D :: [Text]` (viridis colormap の色停止列) も
> `palette` / `continuousPalette` に渡せる。 関連型: `ColorEnc` (色 encoding)。

**離散軸のカテゴリ選択 + 並べ替え** (`scaleXDiscreteLimits` / `scaleYDiscreteLimits`、
= ggplot `scale_x_discrete(limits=)`): 列挙した順がそのまま表示順になり、 `forest` の行選択・並べ替えも
y 軸版で行う (`<> scaleYDiscreteLimits ["b1_0","sigma"]`)。

> `scale*DiscreteLimits` は **範囲外カテゴリの行を落とす** (連続軸の `axisRange` の離散版)。
> aes 基準なので `coord_flip` と直交 (flip 後も x/y はデータ軸を指す)。 対象は当該軸の
> encoding が文字列列 (`inlineCat` / TxtData) の layer のみ。

![scaleYDiscreteLimits: forest の行選択 + 並べ替え](images/s3d-discrete-limits.svg)

デモ (`colorContinuousBy` + `scaleColorGradient2`):

```haskell
purePlot <> layer (scatter xs ys <> colorContinuousBy zs <> size 9)
  <> scaleColorGradient2 "#2166AC" "#F7F7F7" "#B2182B" 3.0   -- lo mid hi midpoint
  <> legend
```

![scaleColorGradient2](images/s3d-scale.svg)

---

## 3. position scale ─ 軸の制御 {#axis}

x / y の **position scale** = 軸そのものの制御。スケール変換 (log / sqrt / time)・表示範囲・tick・
第 2 軸などを `AxisSpec` で組む。軸は scale (データ→位置の写像) と cosmetic を兼ねるので、軸全体を
ここに置く (フォント・グリッド色など純 cosmetic は [04 decoration](04-decoration.ja.md#theme) の theme 側)。

### 軸の細かい制御 (`xAxis` / `yAxis` に `AxisSpec` を渡す)

`xAxis :: AxisSpec -> VisualSpec` / `yAxis` に **`AxisSpec`** を渡す。 `AxisSpec` は Monoid なので
builder を `<>` で組み合わせる:

| AxisSpec builder | 効果 |
|---|---|
| `logAxis` / `sqrtAxis` / `linearAxis` | スケール (log / sqrt / 線形 = 既定) |
| `timeAxis "%Y-%m"` | 時間軸 (strftime フォーマット) |
| `axisRange lo hi` / `axisMin v` / `axisMax v` | 表示範囲 |
| `axisBreaksAt [..]` / `axisBreaksLabeled [(v,"…"),..]` | tick 位置 / 位置 + ラベル |
| `axisBreak from to` | 軸の break (省略) 区間 |
| `axisTickLabels ["a","b",..]` | tick ラベルを明示 |
| `axisFormat fmt` (`AxisFormat`) / `axisRotate 45` | 数値フォーマット / ラベル回転角 |
| `hideTicks` | tick を隠す |

> 関連型: `AxisSpec` (Monoid)・`AxisFormat` / `AxisKind` (linear/log/sqrt/time)・`AxisBreak`・
> `YAxisSide` (左/右)。

```haskell
-- x は log・範囲 1–1000・ラベル 45°、 y は 0–100
purePlot <> layer (scatter "x" "y")
  <> xAxis (logAxis <> axisRange 1 1000 <> axisRotate 45)
  <> yAxis (axisRange 0 100)
```

![xAxis (logAxis + axisRange + axisRotate)](images/s3h-axis.svg)

### 第 2 軸 (右 y 軸)

`yAxisRight :: AxisSpec -> VisualSpec` で右側に独立した y 軸を足し、 その軸に載せる layer の
**中**で `toRightY` を付ける (既定は左軸・`toLeftY` で明示も可)。 単位の違う 2 系列を重ねるときに使う:

```haskell
purePlot
  <> layer (line "t" "price")                  -- 左軸 (既定)
  <> layer (line "t" "volume" <> toRightY)     -- 右軸へ割当
  <> yAxisRight (axisRange 0 1000000)
```

![第 2 軸 (price=左 / volume=右)](images/s3h-second-axis.svg)

> **座標系の変換** (`coordFlip` / `coordPolar` / `reverseX` / `reverseY` / `coordCartesian*`) は
> 軸の値域そのものを差し替えるのではなく描画座標を変える操作なので、[04 decoration の座標系](04-decoration.ja.md#coord)
> にある。

---

> **関連ページ**: 各 mark がどの channel を受け付けるかは [02 layers](02-layers.ja.md) の定型エントリ。
> theme・facet・参照線など図の装飾は [04 decoration](04-decoration.ja.md)。列名で channel を書く DataFrame
> 連携は [06 dataframe](06-dataframe.ja.md)。
