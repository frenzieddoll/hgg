# 03. データ変換 (R4DS 2e Ch.3 "Data transformation")

> 一次情報: **R for Data Science 2e, Ch.3 "Data transformation"**
> <https://r4ds.hadley.nz/data-transform>
> データ: **nycflights13** の `flights`(**全 336,776 行**)と **Lahman** の `Batting`。
> 出所は [`../_data/_raw/SOURCE.md`](../_data/_raw/SOURCE.md)。

dplyr の動詞(`filter` / `arrange` / `distinct` / `count` / `mutate` / `select` /
`rename` / `relocate` / `group_by` + `summarize` / `slice_*`)を **dataframe** に
1:1 で対応づけます。 R4DS が本文で描く図は **1 枚だけ**(case study の打者成績散布図)
なので、 他の例はすべて結果の表(tibble)を印字して示します。 **データは全量**を使い
(間引きなし)、 値を R4DS と一致させています。 実行コードは
[`DataTransform.hs`](DataTransform.hs)。

```sh
cd docs/tutorials/03-data-transform
cabal run tut-03-data-transform     # 各操作の結果を印字し、 batters.svg を生成
```

欠損(キャンセル便の `dep_delay` / `arr_delay` / `air_time` 等)は `Maybe Int` として
読まれます。 R の算術は NA を伝播する(`gain = NA` など)ので、 `mutate` は
`F.nullLift2` で **NA を伝播させつつ行を落とさず**(R と同じ 336,776 行)再現します。

---

## §3.1 はじめに — flights を見る

```haskell
flightsRaw <- DF.readCsv "../_data/_raw/flights.csv"
let flights = flightsRaw |> DF.exclude ["rownames"]   -- CSV 化の副産物の行番号列を除く
```

`flights` は 336,776 行 × 19 列。 `glimpse(flights)` に相当するのは
`DF.describeColumns`(各列の名前・型・件数・欠損数)です。 欠損があるのは
`dep_time`/`dep_delay`(各 8,255)・`arr_time`(8,713)・`arr_delay`/`air_time`(各 9,430)・
`tailnum`(2,512)で、 これらが `Maybe` 型になります。

## §3.2 行 — filter() / arrange() / distinct()

**`filter(dep_delay > 120)`** — 2 時間以上遅れて出発した便。 R の `filter` は NA を
偽として落とすので、 `filterJust` で欠損を除いてから比較すれば同値です。

```haskell
flights |> DF.filterJust "dep_delay"
        |> DF.filterWhere (F.col @Int "dep_delay" .> (120 :: DF.Expr Int))
-- → 9,723 行
```

**`filter(month == 1 & day == 1)`**(842 行)、 **`filter(month == 1 | month == 2)`**
(51,955 行)。 `|` と `==` の近道 `%in%` は `filterBy` + `elem` で:

```haskell
flights |> DF.filterWhere (F.col @Int "month" .== 1 .&& F.col @Int "day" .== 1)  -- 842
flights |> DF.filterBy (`elem` [1, 2]) (F.col @Int "month")                       -- 51,955
```

**`arrange(desc(dep_delay))`** — 最も遅れた便から。 先頭は **HA 51(1,301 分遅れ)**で
R4DS と一致します。

```haskell
flights |> DF.sortBy [ DF.Desc (F.col @Int "dep_delay") ]
```

| year | month | day | dep_delay | carrier | flight |
|---|---|---|---|---|---|
| 2013 | 1 | 9 | 1301 | HA | 51 |
| 2013 | 6 | 15 | 1137 | MQ | 3535 |
| 2013 | 1 | 10 | 1126 | MQ | 3695 |

> ★`dataframe` は欠損列を「基底型(`Int`)のベクタ + null bitmap」で格納します。
> そのため sort の型注釈は **基底型 `@Int`** を使います(`@(Maybe Int)` だと型が
> 一致せず並べ替えが効きません)。

**`distinct(origin, dest)`** — 一意な出発地×目的地は **224 組**(R4DS 一致)。
**`count(origin, dest, sort = TRUE)`** — 便数の多い路線順:

```haskell
flights |> DF.groupBy ["origin", "dest"]
        |> DF.aggregate [ F.countAll `F.as` "n" ]
        |> DF.sortBy [ DF.Desc (F.col @Int "n") ]
```

| origin | dest | n |
|---|---|---|
| JFK | LAX | 11262 |
| LGA | ATL | 10263 |
| LGA | ORD | 8857 |

## §3.3 列 — mutate() / select() / rename() / relocate()

**`mutate(gain = dep_delay - arr_delay, speed = distance / air_time * 60)`** —
両辺の `Maybe` を `F.nullLift2` で NA 伝播させます(R の算術と同じ・行は落とさない)。

```haskell
let gainE  = F.nullLift2 (\d a -> d - a)
               (F.col @(Maybe Int) "dep_delay") (F.col @(Maybe Int) "arr_delay") `F.as` "gain"
    speedE = F.nullLift2 (\d a -> fromIntegral d / fromIntegral a * 60 :: Double)
               (F.col @Int "distance") (F.col @(Maybe Int) "air_time") `F.as` "speed"
flights |> DF.deriveMany [gainE, speedE]
```

先頭行は `gain = 2 - 11 = -9`、 `speed = 1400/227*60 = 370.0`(R4DS と一致)。
`.before = 1` / `.after = day` は列順の入れ替え(`DF.select` で並びを指定)、
`.keep = "used"` は計算に関与した列のみ残す(`DF.select` で射影)で再現します。

**`select`** の各形:

```haskell
flights |> DF.select ["year", "month", "day"]                 -- 名前指定
flights |> DF.selectBy [ DF.byNameRange ("year", "day") ]     -- year:day 範囲
flights |> DF.exclude ["year", "month", "day"]                -- !year:day 除外
flights |> DF.selectBy [ DF.byProperty isChar ]               -- where(is.character)
  where isChar c = "Text" `T.isInfixOf` T.pack (columnTypeString c)
flights |> DF.select ["tailnum"] |> DF.rename "tailnum" "tail_num"  -- 選びつつ改名
```

`where(is.character)` は文字列型の 5 列(`carrier` / `tailnum` / `origin` / `dest` /
`time_hour`)を返します。 **`rename(tail_num = tailnum)`** は `DF.rename`、
**`relocate`** は `DF.select` で列順を組み替えて再現します(例:
`relocate(starts_with("arr"), .before = dep_time)` で `arr_time` / `arr_delay` を
`dep_time` の前へ)。

## §3.4 パイプ — 複数動詞の連結

`filter |> mutate |> select |> arrange` を 1 本のパイプで(IAH 行きの最速便):

```haskell
flights |> DF.filterWhere (F.col @Text "dest" .== F.lit ("IAH" :: Text))
        |> DF.deriveMany [speedE]
        |> DF.selectBy [ DF.byNameRange ("year", "day"), DF.byName "dep_time"
                       , DF.byName "carrier", DF.byName "flight", DF.byName "speed" ]
        |> DF.sortBy [ DF.Desc (F.col @Double "speed") ]
```

R4DS の入れ子版・中間オブジェクト版は同じ結果になります(パイプが最も読みやすい)。

## §3.5 グループ — group_by() / summarize() / slice_*()

**`group_by(month) |> summarize(avg_delay = mean(dep_delay, na.rm = TRUE), n = n())`**

```haskell
let avgByMonth = flights |> DF.filterJust "dep_delay" |> DF.groupBy ["month"]
                         |> DF.aggregate [ F.mean (F.col @Int "dep_delay") `F.as` "avg_delay" ]
    nByMonth   = flights |> DF.groupBy ["month"] |> DF.aggregate [ F.countAll `F.as` "n" ]
DF.innerJoin ["month"] avgByMonth nByMonth |> DF.sortBy [ DF.Asc (F.col @Int "month") ]
```

| month | avg_delay | n |
|---|---|---|
| 1 | 10.04 | 27004 |
| 6 | 20.85 | 28243 |
| 7 | 21.73 | 29425 |
| 12 | 16.58 | 28135 |

値は R4DS と一致します。 平均(NA 無視)と件数(全行)を **別々に集計して
`innerJoin`** しているのは、 この版の `dataframe` のグループ集計が欠損 slot を 0 と
して混ぜてしまうため(→ LIMITATIONS)。

**`group_by(dest) |> slice_max(arr_delay, n = 1)`** — 各目的地で最も遅着した便
(タイは全部残す)。 `dataframe` に `slice_max` は無いので「dest ごとの最大値」を出して
`innerJoin` で復元します。 R4DS と同じ **105 目的地 → 108 行**(タイ +3 と、
`arr_delay` が全 NA の `LGA`〔キャンセル 1 便〕を `na_rm = FALSE` 既定で残す分):

```haskell
let arrNN   = flights |> DF.filterJust "arr_delay"
    destMax = arrNN |> DF.groupBy ["dest"]
                    |> DF.aggregate [ F.maximum (F.col @Int "arr_delay") `F.as` "arr_delay" ]
    tied    = DF.innerJoin ["dest", "arr_delay"] arrNN destMax
-- 全 NA の dest (LGA) の行も加えて R の na_rm=FALSE と同じ 108 行に
```

**`group_by(year, month, day) |> summarize(n = n())`** は 365 行(2013 年の日数)。
複数変数 group_by の summarize が「最後の群を 1 つ剥がす」 dplyr のメッセージは
`dataframe` では出ません(群は都度明示)。 `.by` は per-operation grouping で、
`groupBy` + `aggregate` と同値です。

## §3.6 ケーススタディ: 集計とサンプルサイズ(★この章で唯一の図)

Lahman の打者成績で、 打率 `performance = sum(H)/sum(AB)` を打数 `n = sum(AB)` に
対してプロットします。

```haskell
batting <- DF.readCsv "../_data/_raw/batting.csv"
let batters = batting |> DF.groupBy ["playerID"]
                      |> DF.aggregate [ F.sum (F.col @Int "AB") `F.as` "n"
                                      , F.sum (F.col @Int "H")  `F.as` "hits" ]
                      |> DF.derive "performance"
                           (F.toDouble (F.col @Int "hits") / F.toDouble (F.col @Int "n"))
saveSVGBoundStats "batters.svg" $
  (batters |> DF.filterWhere (F.col @Int "n" .> (100 :: DF.Expr Int)))
    |>> theme ThemeGrey <> layer (scatter "n" "performance" <> color (fromHex "#000000") <> alpha 0.1)
     <> layer (statSmooth "n" "performance" 8 <> color (fromHex "#3366FF"))
     <> xLabel "n" <> yLabel "performance"
```

![batters](batters.svg)

R4DS と同じ 2 つのパターンが読み取れます:
1. 打数 `n` が少ない選手ほど打率 `performance` のばらつきが大きい(大数の法則)。
2. 打率と打数に正の相関(チームは上手い打者に多く打席を与える)。

`arrange(desc(performance))` で素朴に並べると、 **打数が極端に少ない**選手
(`n = 1`〜`2` で打率 `1.0`)が上位に来ます。 これが「集計には必ず件数を添える」
という教訓です。

## できないこと / 近似せず記録した相違(LIMITATIONS)

近似・置換・間引きはしていません。 以下は `dataframe` 1.3 側の制約で、 正直に記録します。

- **✗ `distinct()`(全列)**: NA を含む列があると `fromMaybeVec: Nothing slot` で
  クラッシュします(この版の制約)。 NA の無い部分列の `distinct(origin, dest)` は
  動くのでそちらを示しています。
- **✗ `distinct(origin, dest, .keep_all = TRUE)`**: `dataframe` の `distinct` は
  完全重複行の除去のみで、 「部分列で一意化しつつ最初の出現の全列を残す」 直接の
  手段がありません。 この 1 例は未再現として記録します。
- **△ グループ平均の欠損**: この版の grouped `meanMaybe` は欠損 slot を 0 として
  混ぜ平均を下振れさせます。 `filterJust` + `F.mean` で正しい NA 無視平均にし、
  件数は別集計して `innerJoin` で結合しました(結果は R と一致)。
- **△ 並べ替えの NA 位置**: R は NA を常に末尾に置きますが、 `dataframe` の null slot は
  基底既定値として並びます。 表示される上位行の順序は R と一致します。
- **△ R の実行時エラー例**: `filter(month = 1)`(`=` 誤用)や `filter(month == 1 | 2)`
  の「動くが意図と違う」例は、 Haskell では型/構文で書けない(コンパイラが弾く)ため、
  実行時の挙動としては再現できません。 コメントで対応関係を説明しています。
