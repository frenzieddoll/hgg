# 04. コードスタイル (R4DS 2e Ch.4 "Workflow: code style")

> 一次情報: **R for Data Science 2e, Ch.4 "Workflow: code style"**
> <https://r4ds.hadley.nz/workflow-style>
> データ: **nycflights13** の `flights`(全量)。

R4DS 第 4 章は **図を描かない**「コードスタイル」章です(本文の R コードはすべて
`eval: false` のスタイル見本、 図は RStudio のスクリーンショットのみ)。 ここでは
[tidyverse style guide](https://style.tidyverse.org) の各ルールを **Haskell /
本プロジェクト規約(`../../../CLAUDE.md`)に対応づけ**、
「**Strive for(こう書く)/ Avoid(避ける)**」で示します。 実行コードは
[`WorkflowStyle.hs`](WorkflowStyle.hs)(Strive 版を実際に動かして確認)。

```sh
cd docs/tutorials/04-workflow-style
cabal run tut-04-workflow-style
```

このファイル群自体が本プロジェクトのスタイル見本です(2 スペース字下げ・`camelCase`・
セクション境界の `-- ===` 罫線・`=` の揃え・パイプは 1 行 1 動詞)。

---

## §4.2 名前(Names)

R: 変数名は小文字 + 数字 + `_`(snake_case)。 本プロジェクト規約は **`camelCase`**
(関数・束縛)/ `PascalCase`(型・コンストラクタ)。 長く説明的な名前を短い略語より
優先する点は R と同じです。

```haskell
-- Strive for: 説明的・camelCase
shortFlights = flights |> DF.filterJust "air_time"
                       |> DF.filterWhere (F.col @Int "air_time" .< 60)

-- Avoid: 略語・全大文字
sf = ...        -- 何の略か後で分からない
```

## §4.3 空白(Spaces)

R: 二項演算子(`+ - == <` …)の両側に空白(`^` は例外)、 代入(`<-`)の周りにも
空白。 関数呼び出しの括弧の内外には空白を入れない。 カンマの後に空白。 Haskell も同じ。

```haskell
-- Strive for
z = (a + b) ^ 2 / d

-- Avoid
z=( a+b )^2/d
```

`=` の **揃え**も R と同じく有効です。 複数列を作るときに `=`(dataframe では
`` `F.as` ``)を縦に揃えると読みやすくなります(`dep_time` は HHMM 形式なので、
R の `%/% 100`・`%% 100` は dataframe の `div`・`mod`):

```haskell
flights |> DF.deriveMany
  [ F.nullLift2 (\dist t -> fromIntegral dist / fromIntegral t :: Double)
      (F.col @Int "distance") (F.col @(Maybe Int) "air_time") `F.as` "speed"
  , F.nullLift (\t -> t `div` 100) (F.col @(Maybe Int) "dep_time") `F.as` "dep_hour"
  , F.nullLift (\t -> t `mod` 100) (F.col @(Maybe Int) "dep_time") `F.as` "dep_minute" ]
```

## §4.4 パイプ(Pipes)

R: `|>` の前に空白・行末に置き、 **1 行 1 動詞**。 名前付き引数を持つ関数
(`mutate` / `summarize`)は **引数を 1 行ずつ・2 スペース追加字下げ**、 閉じ括弧 `)` は
独立行で関数名の位置に揃える。 dataframe の `|>` も同じ流儀:

```haskell
-- Strive for
flights
  |> DF.filterJust "arr_delay"
  |> DF.filterJust "tailnum"
  |> DF.groupBy ["dest"]
  |> DF.aggregate [ F.countAll `F.as` "n" ]

-- Avoid
flights|>DF.filterJust "arr_delay"|>DF.groupBy["dest"]|>DF.aggregate[F.countAll `F.as` "n"]
```

名前付き引数(集計式)が複数あるときは 1 行ずつに分け、 リストの括弧を揃えます:

```haskell
-- Strive for (集計式を 1 行ずつ)
byTail =
  flights
    |> DF.groupBy ["tailnum"]
    |> DF.aggregate
         [ F.mean (F.col @Int "arr_delay") `F.as` "delay"
         , F.countAll                      `F.as` "n" ]
```

> ★この `group_by(tailnum)` 例は**整形の見本**です。 `tailnum` は元が `Maybe Text`
> (欠損あり)で、 この版の `dataframe` は**元が欠損列だった列での `groupBy` が
> クラッシュ**します。 そのため実行デモ([`WorkflowStyle.hs`](WorkflowStyle.hs))は、
> R4DS のもう一つの §4.4 例 `… |> count(dest)`(`dest` は非 null)で行っています。

## §4.5 ggplot2

R: ggplot の `+` もパイプと同じ流儀で整形(`+` を `|>` と同じに扱う)。
hgg では、 データを `|>>` で束ね、 レイヤを `<>` で重ねます(= ggplot の `+`)。
整形ルールは同じ(1 行 1 レイヤ、 引数が長ければ 1 行ずつ):

```haskell
-- Strive for (|>> でデータ束ね、 <> でレイヤ重ね、 1 行 1 レイヤ)
delayByMonth
  |>> layer (line    "month" "delay")
   <> layer (scatter "month" "delay")

-- 引数が多いレイヤは 1 行ずつ
plotData
  |>> layer (statSmooth "distance" "speed" 8
               <> colorStatic "#FFFFFF"
               <> stroke 4)
   <> layer (scatter "distance" "speed")
```

R の `|>` → `+` の切り替えに相当するのが、 本ライブラリの `|>>`(データ束ね)→ `<>`
(レイヤ重ね)の切り替えです。 ★R4DS はこの章で図を生成しない(例は `eval: false`)ので、
本チュートリアルでも図は作らず、 プロット元の集計(月別平均遅延)だけ出力します。

## §4.6 セクション罫線(Sectioning comments)

R: `# Load data ----------` のような区切りコメントでスクリプトを分割。 本プロジェクト
規約は **`-- ===` 罫線**(`../../../CLAUDE.md`)。
[`WorkflowStyle.hs`](WorkflowStyle.hs) の各セクションがその見本です。

```haskell
-- =========================================================================
-- §4.2 Names — 変数名
-- =========================================================================
```

## できないこと / 近似せず記録した相違

- **△ 元欠損列での `groupBy`**: `tailnum`(元 `Maybe Text`)のように欠損を含んでいた列で
  `groupBy` するとこの版の `dataframe` はクラッシュします。 §4.4 の実行デモは非 null の
  `dest` を使う R4DS のもう一つの例で代用しました(整形ルール自体は同一)。
- **R の実行時挙動**: R4DS のスタイル例は `eval: false`(描画・実行しない見本)です。
  本章も Strive 版だけを実際に動かし、 Avoid 版は対比のために示すだけです。
