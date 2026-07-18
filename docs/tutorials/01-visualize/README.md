# 01. データ可視化 (R4DS 2e Ch.1 "Data visualization")

> 一次情報: **R for Data Science 2e, Ch.1 "Data visualization"**
> <https://r4ds.hadley.nz/data-visualize>
> データ: **palmerpenguins** の `penguins`(344 個体。 出所は
> [`../_data/_raw/SOURCE.md`](../_data/_raw/SOURCE.md))
>
> 使う mark の詳細仕様(シグネチャ・encoding・全オプション)は
> [API リファレンス 02 layers](../../api-guide/02-layers.md) に。

> 「単純なグラフは、 他のどんな道具よりも多くの情報をデータ分析者の頭にもたらす。」
> — John Tukey

hgg は **グラフィックスの文法 (grammar of graphics)** —— グラフを記述し
組み立てるための一貫した体系 —— を実装しています。 中心になる考え方は 1 つだけ:
**可視化とは、 データの変数を位置・色・大きさ・形といった視覚属性 (aesthetic) へ
写像することだ** というものです。 この 1 つの体系を覚えれば、 さまざまな図を
同じ書き方で速く作れます。

この章では、 まず散布図を作りながら **aesthetic (視覚属性へのマッピング)** と
**mark (図形要素)** という 2 つの基本部品を導入し、 続いて 1 変数の分布、
2 変数以上の関係を可視化し、 最後に図の保存とつまずきどころを扱います。

penguins 全量(344 行)を使い、 R4DS 各図の見た目(mark の種類・色や形のマッピング・
binwidth/position/facet 等の設定)を hgg の `layer (mark …)` で同じ図になるよう
写しています。 hgg では data を `|>>` で束ね、 `scatter`/`bar`/`boxplot` 等の
**mark** を `layer` で重ねて図を作り、 色・形・大きさは mark 内の `colorBy`/`shapeBy`/…
で与えます。 以下は R4DS の流れに沿って、 **解説 → コード → 図** を順に並べた
walkthrough です。 完全な実行コードは [`Visualize.hs`](Visualize.hs)。

```sh
cd docs/tutorials/01-visualize
cabal run tut-01-visualize    # 01-teaser.svg .. 24-facet-island.svg を生成
```

## 欠損値(最初に 1 つだけ)

`flipper_length_mm` と `body_mass_g` には欠損(2 行)があり、 dataframe では
`Maybe Int` として読まれます。 hgg は **`Maybe` 列を列名でそのまま読め、
mark も stat(回帰線など)も `Nothing`(NA)を自動で落とします**(= R の `na.rm`)。
なので以降の図は `raw` を直読し、 明示的なフィルタは要りません。

明示的に欠損行を落としたいときは `DF.filterJust` も使えます(= R4DS の
*"removing 2 rows containing missing values"* に相当):

```haskell
-- 明示除去 (任意): NA 行を落として plain な Int 列にする
let cleaned = raw |> DF.filterJust "flipper_length_mm"
                  |> DF.filterJust "body_mass_g"
-- ただし以降の図は raw を直読する (mark/stat が NA を自動除外するため)
```

---

## §1.1 完成図(章扉の motivating plot)

R4DS は章の冒頭で「この章を終えると描けるようになる図」を見せます。 体重と
フリッパー長の関係を、 種で色分け・形分けし、 回帰直線(`statLm`)と
タイトル・整形ラベルを添えた完成図です(`09-final.svg` と同一)。

配色には colorblind-safe な **Okabe-Ito パレット**(`palette okabeIto`)を使います。
本章では、 この完成図と最後の仕上げ図の **2 枚だけ** が整形ラベルと colorblind
パレットを持ち、 途中の図は軸ラベル=変数名そのまま・配色=既定のままにします
(R4DS が `labs()` と `scale_color_colorblind()` をこの 2 枚にだけ付けるのに合わせ)。

```haskell
saveSVGBoundStats "01-teaser.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> shapeBy "species" <> alpha 0.85)
      <> layer (statLm "flipper_length_mm" "body_mass_g" <> color smoothBlue)
      <> palette okabeIto
      <> title "Body mass and flipper length"
      <> subtitle "Dimensions for Adelie, Chinstrap, and Gentoo Penguins"
      <> xLabel "Flipper length (mm)" <> yLabel "Body mass (g)"
      <> legendTitle "Species"
      <> theme ThemeGrey
```

![teaser](01-teaser.svg)

---

## §1.2 散布図を一歩ずつ組み立てる

hgg では、 データを `|>>` で束ねた spec に `layer (mark …)` を重ねて図を作ります。
mark を 1 つも足さなければ空のキャンバスのままです。 R4DS の「空パネル → 軸 → 図」
の流れに対応させて、 まず mark 無しの状態から見ていきます。

**mark 無し(`purePlot` = 空 spec)** — 何も重ねなければ空のパネルだけ。 列を
指定していないので、 軸は既定レンジ(0–1)になります(= hgg が空 spec で出す姿)。

```haskell
saveSVG "02-empty.svg" $
  purePlot
```

![empty](02-empty.svg)

**軸ラベルだけ付ける(まだ mark 無し)** — `xLabel`/`yLabel` で軸名を付けても、
列・スケールは mark が決めるので、 mark を足すまでは目盛は既定レンジ(0–1)のまま・
点もありません(R4DS の「軸だけ・図は無し」 に対応する step。 hgg は mark を
足して初めて列が定まる点が違います)。

```haskell
saveSVGBound "03-empty-axes.svg" $
  raw |>> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> theme ThemeGrey
```

![empty axes](03-empty-axes.svg)

**`layer (scatter …)` を足す** — 最初の散布図。
列名を与えた scatter mark がデータ範囲の軸と点を生みます。 体重とフリッパー長は正の相関。

```haskell
saveSVGBound "04-scatter.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g" <> alpha 0.85)
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> theme ThemeGrey
```

![scatter](04-scatter.svg)

**`colorBy "species"` を足す** — 種ごとに色分け。 カテゴリ変数を色 channel へ
写像すると、 hgg は各水準に自動で色を割り当て、 凡例も自動生成します
(= ggplot の scaling)。 色・x 軸・点の形・facet の **水準順はすべてアルファベット順**
(ここでは Adelie / Chinstrap / Gentoo)で、 配色は既定の hue パレット(3 色)です。

```haskell
saveSVGBound "05-color.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> alpha 0.85)
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> legendTitle "species"
      <> theme ThemeGrey
```

![color](05-color.svg)

**回帰直線 `layer (statLm …)` を重ねる** — `colorBy "species"` を散布と回帰の
両 layer に付けると、 群が両方に効くので **種ごとに 3 本**の回帰直線が引かれます。

```haskell
saveSVGBoundStats "06-smooth-species.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> alpha 0.85)
      <> layer (statLm "flipper_length_mm" "body_mass_g" <> colorBy "species")
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> legendTitle "species"
      <> theme ThemeGrey
```

![smooth species](06-smooth-species.svg)

**`colorBy` を散布 layer だけに付ける** — 回帰直線は全データで **1 本**(ggplot 既定の青)。
このように同じ aesthetic を **どの layer に置くか** で図の意味が変わります(全 layer に
効かせるか、 特定の mark だけに効かせるか)。

```haskell
saveSVGBoundStats "07-smooth-global.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> alpha 0.85)
      <> layer (statLm "flipper_length_mm" "body_mass_g" <> color smoothBlue)
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> legendTitle "species"
      <> theme ThemeGrey
```

![smooth global](07-smooth-global.svg)

**`colorBy "species"` + `shapeBy "species"`** — 色に加えて点の形でも種を区別。

```haskell
saveSVGBoundStats "08-shape.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> shapeBy "species" <> alpha 0.85)
      <> layer (statLm "flipper_length_mm" "body_mass_g" <> color smoothBlue)
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> legendTitle "species"
      <> theme ThemeGrey
```

![shape](08-shape.svg)

**完成図に仕上げる** — `title`/`subtitle`/`xLabel`/`legendTitle` でラベルを整え、
colorblind-safe パレット(Okabe-Ito)を `palette okabeIto` で添えます(= teaser)。

```haskell
saveSVGBoundStats "09-final.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> shapeBy "species" <> alpha 0.85)
      <> layer (statLm "flipper_length_mm" "body_mass_g" <> color smoothBlue)
      <> palette okabeIto
      <> title "Body mass and flipper length"
      <> subtitle "Dimensions for Adelie, Chinstrap, and Gentoo Penguins"
      <> xLabel "Flipper length (mm)" <> yLabel "Body mass (g)"
      <> legendTitle "Species"
      <> theme ThemeGrey
```

![final](09-final.svg)

---

## §1.3 hgg の書き方

ここまでで散布図の組み立て方を見てきました。 以降の図でくり返し現れる **書き方の型**
を一度まとめておきます(R4DS が §1.3 でコードを簡潔形へ移行するのに対応する節です)。

hgg の図は、 つねに次の形をしています:

```haskell
データ |>> layer (mark 列… <> aesthetic…) <> 装飾…
```

- **`|>>`** — DataFrame と spec を束ねる(plot bind)演算子。 左にデータ、 右に
  spec を置きます。 データ変換に使う `|>`(DataFrame の前方パイプ、 R4DS の `|>` と
  同型)とは別物です。
- **`layer (mark …)`** — 1 つの図形レイヤー。 `scatter`/`bar`/`histogram`/… の
  **mark** に列名と aesthetic を与えます。 `layer` を `<>` で重ねれば多層の図に
  なります。
- **`<>`** — mark 内の aesthetic どうし、 layer どうし、 装飾どうしを結合する演算子
  (モノイド結合)。 `colorBy "species" <> alpha 0.85` のように属性を足し合わせます。
- **装飾** — `xLabel`/`title`/`legendTitle`/`theme`/`palette` 等。 layer の外側に
  `<>` で足します。

図を出力する関数は、 図の中身に応じて 3 つを使い分けます:

| 関数 | いつ使うか |
|---|---|
| `saveSVG` | 列名を使わない(値を直接埋め込んだ inline データの)図 |
| `saveSVGBound` | `df \|>> spec` で **列名** を使う図(本章の基本) |
| `saveSVGBoundStats` | 上に加えて回帰線など **stat**(`statLm`/`statSmooth`)を含む図 |

この型さえ覚えれば、 あとは mark と aesthetic を差し替えるだけで、 散布図・棒・
ヒストグラム・箱ひげ…と同じ書き方のまま作り分けられます。 以降の節はすべて
この型で書かれています。

---

## §1.4 1 変数の分布

**`bar` mark(種ごとの件数)** — カテゴリ変数の件数。 棒は件数集計が要るので、
ここでは `DF.aggregate` で先に求めます(値は不変)。 x はアルファベット順。

```haskell
let bySpecies = raw |> DF.groupBy ["species"]
                    |> DF.aggregate [ F.count (F.col @Text "species") `F.as` "n" ]

saveSVGBound "10-bar-species.svg" $
  bySpecies |>> layer (bar "species" "n")
              <> xLabel "species" <> yLabel "count"
              <> theme ThemeGrey
```

![bar species](10-bar-species.svg)

**件数降順に並べる** — (Adelie 152 > Gentoo 124 > Chinstrap 68)。
`scaleXDiscreteLimits` で水準順を明示します(R4DS の `fct_infreq` 相当)。

```haskell
saveSVGBound "11-bar-infreq.svg" $
  bySpecies |>> layer (bar "species" "n")
              <> scaleXDiscreteLimits ["Adelie", "Gentoo", "Chinstrap"]
              <> xLabel "species" <> yLabel "count"
              <> theme ThemeGrey
```

![bar infreq](11-bar-infreq.svg)

**`histogram` mark(`binWidth 200`)** — 連続変数(体重)の分布。 `binWidth`
(= R4DS の `binwidth`)が bin の境界と棒の高さを決めます。

```haskell
saveSVGBound "12-histogram-bw200.svg" $
  raw |>> layer (histogram "body_mass_g" <> binWidth 200)
       <> xLabel "body_mass_g" <> yLabel "count"
       <> theme ThemeGrey
```

![histogram bw200](12-histogram-bw200.svg)

**`binWidth 20`** — 細かすぎてギザギザ(過剰に解像)。

```haskell
saveSVGBound "13-histogram-bw20.svg" $
  raw |>> layer (histogram "body_mass_g" <> binWidth 20)
       <> xLabel "body_mass_g" <> yLabel "count"
       <> theme ThemeGrey
```

![histogram bw20](13-histogram-bw20.svg)

**`binWidth 2000`** — 粗すぎて 3 bin(情報が潰れる)。

```haskell
saveSVGBound "14-histogram-bw2000.svg" $
  raw |>> layer (histogram "body_mass_g" <> binWidth 2000)
       <> xLabel "body_mass_g" <> yLabel "count"
       <> theme ThemeGrey
```

![histogram bw2000](14-histogram-bw2000.svg)

**`density` mark** — 分布を滑らかな曲線で。

```haskell
saveSVGBound "15-density.svg" $
  raw |>> layer (density "body_mass_g")
       <> xLabel "body_mass_g" <> yLabel "density"
       <> theme ThemeGrey
```

![density](15-density.svg)

---

## §1.5 2 変数(以上)の関係

**`boxplot` mark(種 × 体重)** — カテゴリ × 連続。

```haskell
saveSVGBound "16-boxplot.svg" $
  raw |>> layer (boxplot "body_mass_g" <> groupBy "species")
       <> xLabel "species" <> yLabel "body_mass_g"
       <> theme ThemeGrey
```

![boxplot](16-boxplot.svg)

**`density` + `colorBy "species"`** — 種ごとに 3 本の密度曲線。

```haskell
saveSVGBound "17-density-color.svg" $
  raw |>> layer (density "body_mass_g" <> colorBy "species")
       <> xLabel "body_mass_g" <> yLabel "density"
       <> legendTitle "species"
       <> theme ThemeGrey
```

![density color](17-density-color.svg)

**`densityFill True` + `alpha 0.5`** — 塗りつぶし付き密度曲線。

```haskell
saveSVGBound "18-density-fill.svg" $
  raw |>> layer (density "body_mass_g" <> colorBy "species"
                 <> densityFill True <> alpha 0.5)
       <> xLabel "body_mass_g" <> yLabel "density"
       <> legendTitle "species"
       <> theme ThemeGrey
```

![density fill](18-density-fill.svg)

**`bar` + `colorBy "species"`(島 × 種)** — 2 カテゴリ。 既定(stack)で
積み上げ。 第 1 水準(Adelie)を一番上に積みます。

```haskell
let byIslandSpecies = raw |> DF.groupBy ["island", "species"]
                          |> DF.aggregate [ F.count (F.col @Text "species") `F.as` "n" ]

saveSVGBound "19-bar-stack.svg" $
  byIslandSpecies |>> layer (bar "island" "n" <> colorBy "species" <> position PosStack)
                    <> xLabel "island" <> yLabel "count"
                    <> legendTitle "species"
                    <> theme ThemeGrey
```

![bar stack](19-bar-stack.svg)

**`position PosFill`** — 各島の合計を 1 に揃える(構成比)。 y 軸ラベルは既定のまま。

```haskell
saveSVGBound "20-bar-fill.svg" $
  byIslandSpecies |>> layer (bar "island" "n" <> colorBy "species" <> position PosFill)
                    <> xLabel "island" <> yLabel "count"
                    <> legendTitle "species"
                    <> theme ThemeGrey
```

![bar fill](20-bar-fill.svg)

**`yLabel "proportion"`** — y 軸ラベルを「proportion」に直す。

```haskell
saveSVGBound "21-bar-fill-proportion.svg" $
  byIslandSpecies |>> layer (bar "island" "n" <> colorBy "species" <> position PosFill)
                    <> xLabel "island" <> yLabel "proportion"
                    <> legendTitle "species"
                    <> theme ThemeGrey
```

![bar fill proportion](21-bar-fill-proportion.svg)

**素の散布図(§1.5.3 冒頭)** — 3 変数を加える前の出発点。

```haskell
saveSVGBound "22-scatter-plain.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g" <> alpha 0.85)
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> theme ThemeGrey
```

![scatter plain](22-scatter-plain.svg)

**`colorBy "species"` + `shapeBy "island"`** — 3 変数(色=種・形=島)。

```haskell
saveSVGBound "23-scatter-shape-island.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> shapeBy "island" <> alpha 0.85)
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> legendTitle "species"
      <> theme ThemeGrey
```

![scatter shape island](23-scatter-shape-island.svg)

**`facetWrap "island" 3`** — 島ごとに小パネルへ分割(パネルもアルファベット順)。

```haskell
saveSVGBound "24-facet-island.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                 <> colorBy "species" <> shapeBy "species" <> alpha 0.85)
      <> facetWrap "island" 3
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
      <> legendTitle "species"
      <> theme ThemeGrey
```

![facet island](24-facet-island.svg)

---

## §1.6 図を保存する

図を作ったら、 ファイルに書き出して他の場所で使いたくなります。 それが
`saveSVG` 系の関数の役目です。 本章ではずっと `saveSVGBound` / `saveSVGBoundStats`
を使ってきました。 これらは渡した spec を SVG ファイルとして保存します。

```haskell
saveSVGBound "penguin-plot.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g")
      <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
```

**図の寸法** は backend 引数ではなく **spec 側** で `width` / `height` /
`aspectRatio` として持ちます。 再現可能なコードにするため、 寸法は明示するのが
おすすめです:

```haskell
saveSVGBound "penguin-plot.svg" $
  raw |>> layer (scatter "flipper_length_mm" "body_mass_g")
      <> width 640 <> height 480
```

**出力形式** は backend を差し替えるだけで変えられます。 API は SVG/PDF/PNG で
対称です:

| 形式 | 関数 | import |
|---|---|---|
| SVG | `saveSVG` / `saveSVGBound` | `Hgg.Plot.Backend.SVG` |
| PDF | `savePDF` / `savePDFBound` | `Hgg.Plot.Backend.PDF` |
| PNG | `savePNG` / `savePNGBound` | `Hgg.Plot.Backend.Rasterific`(日本語ラベル可) |

詳しくは [API リファレンス 05 backends](../../api-guide/05-backends.md) を参照してください。

---

## §1.7 よくある問題

コードを書き始めると、 たいてい何かしら詰まります。 心配いりません —— 誰でも
通る道です。 まずは自分のコードを本章のコードと **一字ずつ** 見比べてください。
Haskell も型と構文にとても厳しく、 1 文字の取り違えで結果が変わります。

- **括弧の対応**: すべての `(` に `)` が、 すべての `"` に対の `"` があるか確認します。
- **演算子の優先順位**: `|>>` は `<>` より低い優先度です。 mark 内の aesthetic を
  `<>` で結合し、 layer の外側に装飾を `<>` で足す —— この入れ子を間違えると型が
  合いません。 迷ったら layer 単位で括弧を付けて切り分けます。
- **mark に必要な列**: `scatter` は x・y の 2 列、 `histogram`/`density` は 1 列が
  必須です。 列が足りないと型エラーになります。
- **列名のタイプミス**: `|>>` で渡す列名(`"flipper_length_mm"` 等)が DataFrame に
  実在するか確認します。 存在しない列名は実行時に分かります。
- **`OverloadedStrings` / `TypeApplications`**: 文字列リテラルを列名に使うには
  `OverloadedStrings`、 `F.col @Text` には `TypeApplications` の言語拡張が要ります
  (`Visualize.hs` 冒頭の `{-# LANGUAGE … #-}` 参照)。

それでも詰まったら、 [API リファレンス](../../api-guide/README.md)で該当 mark の
シグネチャと例を確認し、 型エラーのメッセージを落ち着いて読んでください。 答えが
そこに埋もれていることがよくあります。

---

## §1.8 まとめ

この章では、 hgg によるデータ可視化の基礎を学びました。 出発点は
**「可視化とは、 データの変数を位置・色・大きさ・形といった視覚属性へ写像することだ」**
という考え方です。 そこから、 `layer` を `<>` で重ねて図を一段ずつ複雑に・きれいに
していく方法を学びました。 さらに、 1 変数の分布(棒・ヒストグラム・密度)と、
2 変数以上の関係(箱ひげ・色分け密度・積み上げ棒・3 変数散布図)を、 追加の
aesthetic マッピングや `facetWrap` による小パネル分割で表現しました。 最後に、
図を SVG/PDF/PNG で保存する方法を見ました。

可視化はこのチュートリアル群を通じてくり返し登場します。 より進んだ encoding・
scale・theme の制御は [API リファレンス 03 encoding & scale](../../api-guide/03-encoding-scale.md) /
[04 decoration](../../api-guide/04-decoration.md) で、 統計モデルとの連携は
[07 analyze](../../api-guide/07-analyze.md) で深掘りできます。
